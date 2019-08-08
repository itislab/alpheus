module ItisLab.Alpheus.ComputationGraph

open System
open System.IO
open AlphFiles
open Angara.Data
open Angara.Graph
open Angara.Execution
open Angara.States
open ItisLab.Alpheus.DependencyGraph
open ItisLab.Alpheus.PathUtils


let private arrayType<'a> rank : Type =
    if rank < 0 then invalidArg "rank" "Rank is negative"
    else if rank = 0 then typeof<ArtefactId>
    else typeof<ArtefactId>.MakeArrayType(rank)

let private inputRank (v:MethodVertex) =
    match v with
    | Source src -> 0
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.max

let private outputRank (v:MethodVertex) =
    match v with
    | Source src -> src.Output.Artefact.Rank
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.max

let private methodRank (v:MethodVertex) = min (outputRank v) (inputRank v)

let getOutputTypes (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> [max 0 (src.Output.Artefact.Rank - rank) |> arrayType]
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType) |> List.ofSeq

let getInputTypes (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> List.empty
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType) |> List.ofSeq

type ArtefactItem =
    { FullPath: string
      Index: string list }

let rec private toJaggedArrayOrValue (mapValue: (string list * 'a) -> 'c) (index: string list) (map: MdMapTree<string,'a>) : obj =
    let isValue = function
    | MdMapTree.Value _ -> true
    | MdMapTree.Map _ -> false

    let mapToArray (getElement: (string * MdMapTree<string,'a>) -> 'b) (map: Map<string,MdMapTree<string,'a>>) : 'b[] =
        map |> Map.toSeq |> Seq.sortBy fst |> Seq.map getElement |> Seq.toArray

    let append v list = list |> List.append [v]

    match map with
    | MdMapTree.Value v -> upcast(mapValue (index, v))
    | MdMapTree.Map subMap ->
        match subMap |> Map.forall(fun _ -> isValue) with
        | true -> // final level
            upcast(subMap |> mapToArray (fun (k,t) -> 
                let newIndex = index |> append k
                match t with 
                | MdMapTree.Value v -> mapValue (newIndex, v)
                | MdMapTree.Map _ -> failwith "Unreachable case"))
        | false ->
            upcast(subMap |> mapToArray (fun (k,t) -> 
                let newIndex = index |> append k
                match t with 
                | MdMapTree.Map _ -> toJaggedArrayOrValue mapValue newIndex t 
                | MdMapTree.Value _ -> failwith "Data is incomplete and has missing elements"))


/// This type represents an Angara Flow method.
/// Note that execution modifies the given vertex and it is Angara Flow execution runtime who controls
/// the concurrency.
[<AbstractClass>]
type ComputationGraphNode(producerVertex:MethodVertex, experimentRoot:string) = 
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        getInputTypes producerVertex,
        getOutputTypes producerVertex)

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.Output.Artefact.Id
        |   Command(comp) -> (Seq.head comp.Outputs).Artefact.Id // first output is used as vertex ID

    override s.ToString() = 
        match producerVertex with
        | Source src -> sprintf "Source %A" src.Output.Artefact.Id
        | Command cmd -> sprintf "Command %s" cmd.Command


type SourceMethod(source: SourceVertex, experimentRoot) =
    inherit ComputationGraphNode(DependencyGraph.Source source, experimentRoot)

    override s.Execute(_, _) = // ignoring checkpoints
        let artefact = source.Output.Artefact

        // Output of the method is an scalar or a vector of full paths to the data of the artefact.
        let outputArtefact : Artefact =
            artefact.Id 
            |> PathUtils.enumerateItems experimentRoot
            |> MdMap.toTree
            |> toJaggedArrayOrValue (fun (index, fullPath) -> { FullPath = fullPath; Index = index }) []
        
        match artefact.ActualVersion with
        | None -> // todo : restore check if exists, but note that in case of vectore, only some of the items can exists/restore/etc.
            // The artefact does not exist on disk
            // This may be OK in case the specified version is available in storages
            //if sourceVertex.Output.StoragesContainingVersion.IsEmpty then
            //    invalidOp (sprintf "The source artefact must either exist on local disk or be restorable from storage: %A" sourceVertex.Output.Artefact.Id)
            //else
                // we don't need to save alph file as
                // 1) not tracked artefacts does not initially have alph file and do not need them
                // 2) tracked artefact already have alph files on disk
                ()
        | Some diskVersion when artefact.IsTracked ->
            // if alph file exists on disk (e.g. isTracked), we need to re-save it to update the expected version
            async {
                let artefactFullPath = idToFullPath experimentRoot artefact.Id
                let alphFileFullPath = idToAlphFileFullPath experimentRoot artefact.Id
                let alphFileSection : AlphFiles.VersionedArtefact = 
                    { RelativePath = relativePath alphFileFullPath artefactFullPath
                      Hash = diskVersion }
                let alphFile = artefactToAlphFile artefact artefactFullPath experimentRoot
                let alphFile = { alphFile with Origin = SourceOrigin alphFileSection }
                do! AlphFiles.saveAsync alphFile alphFileFullPath
            } |> Async.RunSynchronously  
        | Some _ -> // not tracked
            ()

        Seq.singleton ([outputArtefact], null)

type CommandMethod(command: CommandLineVertex, experimentRoot) =
    inherit ComputationGraphNode(DependencyGraph.Command command, experimentRoot)

    // todo: input must contain (for each artefact):
    //  - full path
    //  - vector of indices (replacements for asterisks), e.g. "files", "cities.txt", (1) to be substituted in the output pattern and (2) to get hash from vector of hashes (version).
    override s.Execute(inputs, _) = // ignoring checkpoints
        let inputItems = inputs |> List.map (fun inp -> inp :?> ArtefactItem)
        inputItems 
        |> Seq.map(fun item -> item.FullPath)
        |> Seq.iter (fun path -> 
            let exists = if isDirectory path then Directory.Exists path else File.Exists path
            if not exists then failwithf "Input %s does not exist" (if isDirectory path then"directory" else "file"))

        let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%20s:\t\t%s" command.MethodId str)

        //let inputVertices = methodVertex.Inputs |> Set.toList |> List.sortBy (fun x -> x.Artefact.FullID)
        let outputs = command.Outputs // the order is important here

        // intermediate graph node is actually a command execution
        // First, we need to decide whether the computation can be bypassed (the node is up to date) or the computation must be invoked               
        /// versions present and match
        let versionsMatch v1 v2 =
            match v1,v2 with
            | Some(vv1), Some(vv2) -> vv1 = vv2
            | None, Some(_) | Some(_), None | None, None -> false

        let doComputation = true // todo
            // NOTE: signature is checked during the .alph file reading
            // if it is invalid, output versions are empty
            //let isArtefactAvailable (art:LinkToArtefact) =
            //    (versionsMatch art.ExpectedVersion art.Artefact.ActualHash) || (not art.StoragesContainingVersion.IsEmpty)
     
            //// if any of the outputs is unavailable, we have to run the computation
            //not(Seq.forall isArtefactAvailable outputs)
      
        //if doComputation then
        //    // We need to do computation            
        //    // 1) deleting outputs if they exist            
        //    // 2) execute external command
        //    // 3) upon 0 exit code hash the outputs
        //    // 4) fill in corresponding method vertex (to fix propper versions)
        //    // 5) write alph files for outputs

        //    // 1) Deleting outputs
        //    if not command.DoNotCleanOutputs then
        //        let deletePath (path:string) =
        //            if path.EndsWith(Path.DirectorySeparatorChar) then
        //                if Directory.Exists path then
        //                    Directory.Delete(path,true)
        //            else
        //                if File.Exists path then
        //                    File.Delete path
        //        let fullOutputPaths = Seq.map (fun (x:DependencyGraph.VersionedArtefact) -> idToFullPath experimentRoot x.Artefact.Id) comp.Outputs |> List.ofSeq
        //        List.iter deletePath fullOutputPaths

        //    // 2) executing a command
        //    let print (s:string) = Console.WriteLine s
        //    let input idx = comp.Inputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot
        //    let output idx = comp.Outputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot
        //    let context : ComputationContext = { ExperimentRoot = experimentRoot; Print = print  }
        //    let exitCode = comp |> ExecuteCommand.runCommandLineMethodAndWait context (input, output) 

        //    // 3) upon 0 exit code hash the outputs
        //    if exitCode <> 0 then
        //        raise(InvalidOperationException(sprintf "Process exited with exit code %d" exitCode))
        //    else
        //        // 4a) hashing outputs disk content                
        //        let hashComputeation = DependencyGraph.fillinActualHashesAsync (outputs |> Seq.map (fun art -> art.Artefact)) experimentRoot
        //        logVerbose (sprintf "Calculated successfully. Calculating output hashes")
        //        Async.RunSynchronously hashComputeation
  
        //        // 4b) updating dependency versions in dependency graph
        //        let updateVersion (art:DependencyGraph.VersionedArtefact) = 
        //            { art with ExpectedVersion = art.Artefact.ActualHash }
        //        comp.UpdateArtefacts updateVersion

        //        // 5) dumping updated alph files to disk
        //        async {                        
        //            let idToFullAlphPath versionedArtefact = versionedArtefact.Artefact.Id |> idToAlphFileFullPath experimentRoot
        //            let outputAlphfilePaths = Seq.map idToFullAlphPath outputs |> Seq.toArray

        //            let updateAlphFileAsync (alphFilePath:string) artefact =
        //                async {
        //                    let alphFileFullPath = Path.GetFullPath(alphFilePath)
        //                    let alphFile = DependencyGraph.artefactToAlphFile artefact.Artefact alphFileFullPath experimentRoot
        //                    do! AlphFiles.saveAsync alphFile alphFilePath
        //                }
              
        //            let alphUpdatesComputations = Seq.map2 updateAlphFileAsync outputAlphfilePaths outputs
        //            let! _ = Async.Parallel alphUpdatesComputations
        //            return ()
        //        } |> Async.RunSynchronously
        //        logVerbose "Outputs metadata saved"
        //else
        //    logVerbose "skipping as up to date"
        //comp.Outputs |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.ExpectedVersion) |> List.ofSeq

        invalidOp "Not implemented"


let buildGraph experimentRoot (g:DependencyGraph.Graph) =    
    let factory method : ComputationGraphNode = 
        match method with 
        | DependencyGraph.Source src -> upcast SourceMethod(src, experimentRoot) 
        | DependencyGraph.Command cmd -> upcast CommandMethod(cmd, experimentRoot)

    g |> DependencyGraphToAngaraWrapper |> AngaraTranslator.translate factory

let doComputations (g:FlowGraph<ComputationGraphNode>) = 
    let state  = 
        {
            TimeIndex = 0UL
            Graph = g
            Vertices = Map.empty
        }
    try
        use engine = new Engine<ComputationGraphNode>(state,Scheduler.ThreadPool())        
        engine.Start()
        // engine.Changes.Subscribe(fun x -> x.State.Vertices)
        let final = Control.pickFinal engine.Changes
        let finalState = final.GetResult()
        Ok()
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.Message))
        Error(sprintf "Failed to compute the artefacts: \n\t%s" failed)