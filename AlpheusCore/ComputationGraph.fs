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
type ComputationGraphNode(
                            producerVertex:MethodVertex,
                            experimentRoot:string,
                            checkStoragePresence:HashString array -> Async<bool array>,
                            restoreFromStorage: (HashString*string) array -> Async<unit>) = // version*filename
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


type SourceMethod(source: SourceVertex, experimentRoot, checkStoragePresence, restoreFromStorage) =
    inherit ComputationGraphNode(DependencyGraph.Source source, experimentRoot, checkStoragePresence, restoreFromStorage)

    override s.Execute(_, _) = // ignoring checkpoints
        let expectedArtefact = source.Output
        let artefact = expectedArtefact.Artefact

        // Output of the method is an scalar or a vector of full paths to the data of the artefact.
        let outputArtefact : Artefact =
            artefact.Id 
            |> PathUtils.enumerateItems experimentRoot
            |> MdMap.toTree
            |> toJaggedArrayOrValue (fun (index, fullPath) -> { FullPath = fullPath; Index = index }) []
        

        let ensureScalar (artefactVersion:ArtefactVersion) =
            if not artefactVersion.IsScalar then
                failwithf "Source artefacts can't be vectored: %A" artefact.Id
            else
                artefactVersion.AsScalar()    

        let expectedVersionOpt = expectedArtefact.ExpectedVersion |> ensureScalar
        
        let expectedV =
            match expectedVersionOpt with
            | None ->
                // this could be file/dir without alph file.
                (artefact.ActualVersionAsync |> Async.RunSynchronously |> ensureScalar).Value
            | Some(v) -> v

        let actualVersionRes = artefact.ActualVersionAsync |> Async.RunSynchronously
        let actualVersionOpt = ensureScalar actualVersionRes

        match actualVersionOpt with
        |   None -> // The artefact does not exist on disk
            // This may be OK in case the specified version is available in storages
            // todo: note that in case of vectore, only some of the items can exists/restore/etc.
            if (checkStoragePresence [|expectedV|] |> Async.RunSynchronously).[0] then
                // If some methods needs this artefact as input, the artefact will be restored during the method execution
                ()
            else
                failwithf "Source artefact %A is not on disk and can't be found in any of the storages. Consider adding additional storages to look in. Can't proceed with computation." artefact.Id
        |   Some(_) ->
            // we now consider actual version as expected
            source.Output.ExpectActualVersionAsync() |> Async.RunSynchronously
            // if alph file exists on disk (e.g. isTracked), we need to re-save it to update the expected version
            if artefact.IsTracked then
                artefact.SaveAlphFile()
                        
        Seq.singleton ([outputArtefact], null)

type CommandMethod(command: CommandLineVertex, experimentRoot, checkStoragePresence, restoreFromStorage) =
    inherit ComputationGraphNode(DependencyGraph.Command command, experimentRoot, checkStoragePresence, restoreFromStorage)

    let resolveIndex (index:string list) (map: MdMap<string, 'a option>) =
        let rec resolveInTree (index:string list) (map: MdMapTree<string, 'a option>) =
            match map, index with
            | _,[] -> Some map
            | MdMapTree.Value value,_ -> Some map // index length > rank of the map
            | MdMapTree.Map values, k :: tail ->
                match values |> Map.tryFind k with
                | Some value -> resolveInTree tail value
                | None -> None
        match resolveInTree index (map |> MdMap.toTree) with
        | Some(MdMapTree.Value v) -> v
        | Some(MdMapTree.Map map) when map.IsEmpty -> None
        | Some(MdMapTree.Map _) -> invalidOp "Only one-to-one vectors are supported at the moment"
        | None -> None

    /// valid are items that either have actual disk data version match expected version, or actual disk data is missing and expected version is restorable from storage
    let areValidItemsVersions expectedVersionHashes actualVersionsHashes =
        if Array.exists Option.isNone expectedVersionHashes then
            // some of the artefact element was not ever produced, this is invalid
            false
        else
            /// Chooses the pairs that are not valid on disk (filtering out versions match)
            let invalidOnDiskChooser hash1 hash2 =
                match hash1,hash2 with
                |   Some(h1),Some(h2) ->  if h1 = h2 then None else Some(hash1,hash2)
                |   _ -> Some(hash1,hash2)
            let localInvalid = Seq.map2 invalidOnDiskChooser expectedVersionHashes actualVersionsHashes |> Seq.choose id |> Array.ofSeq
            if Array.length localInvalid = 0 then
                // valid as actual disk version match expected version. No need to check the storage
                true
            else
                // check if the locally "invalid" are "remote valid" (restorable from storage in case of disk data absence)
                let eligibleForRemoteCheckChooser pair =
                    let expected,actual = pair
                    match expected,actual with
                    |   Some(v),None -> Some(v)
                    |   _,_ -> None

                let eligibleForRemoteCheck = Array.choose eligibleForRemoteCheckChooser localInvalid
                if Array.length eligibleForRemoteCheck = Array.length localInvalid then
                    // we proceed with the remote checks only if all of the locally invalid items are eligible for remote check
                    let remotePresence = checkStoragePresence eligibleForRemoteCheck |> Async.RunSynchronously            
                    Array.forall id remotePresence
                else
                    // otherwise at least one unrestorable item exists. Thus invalid
                    false

    let isValid (actualVersion:ArtefactVersion) (expectedVersion:ArtefactVersion) =
        let actVersions = MdMap.toSeq actualVersion |> Array.ofSeq
        let expVersions = MdMap.toSeq expectedVersion |> Array.ofSeq
        let vectorKeysMatch = (actVersions |> Seq.map fst |> Set.ofSeq) = (expVersions |> Seq.map fst |> Set.ofSeq)
        if not vectorKeysMatch then
            // vector element keys are different. Thus we cant compare versions and the artefact is invalid
            false
        else
            let actVersionsHashes = actVersions |> Seq.map snd |> Array.ofSeq
            let expVersionHashes = expVersions |> Seq.map snd |> Array.ofSeq
            areValidItemsVersions expVersionHashes actVersionsHashes

    override s.Execute(inputs, _) = // ignoring checkpoints
        async{
            // Rules of execution
            // The artefact is valid either if actual disk version matches expected version or if the disk version is absend and expected version is restorable from storage
            // We can bypass the computation entirely if inputs and outputs are valid
        
           
            // If any input, output is not valid we need to
            //  1) restore inputs if they are absent on disk
            //  2) execute the command
            
            let inputItems = inputs |> List.map (fun inp -> inp :?> ArtefactItem)
                    
            let index =
                inputItems 
                |> Seq.map(fun item -> item.Index)
                |> Seq.fold(fun (max: string list) index -> if index.Length > max.Length then index else max) []
            let methodItemId = command.MethodId |> applyIndex index
            let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%s: %s" methodItemId str)
            // logVerbose "Started"

            // Build the output paths by applying the index of this method.
            let outputPaths = 
                command.Outputs // the order is important here
                |> List.map(fun out -> out.Artefact.Id |> PathUtils.idToFullPath experimentRoot |> applyIndex index)

            let extractExpectedVersionsFromLinks links =
                links |> Seq.map (fun (a:LinkToArtefact) -> MdMap.find index a.ExpectedVersion)

            let extractActualVersionsFromLinks links =
                async {
                    let! actualVersion =
                        links
                        |> Seq.map (fun (a:LinkToArtefact) -> a.Artefact.ActualVersionAsync)
                        |> Async.Parallel
                    return actualVersion |> Array.map (fun v -> MdMap.find index v)
                }

            let expectedInputItemVersions = extractExpectedVersionsFromLinks command.Inputs |> Array.ofSeq
            let! actualInputItemVersions = extractActualVersionsFromLinks command.Inputs

            let areInputsValid = areValidItemsVersions expectedInputItemVersions actualInputItemVersions
        
            let! doComputations = 
                async {
                    if not areInputsValid then
                        // we can avoid checking outputs to speed up the work
                        // is the inputs are invalid
                        logVerbose "Needs recomputation due to the outdated inputs"
                        return true 
                    else
                        // checking outputs
                        let expectedOutputItemVersions = extractExpectedVersionsFromLinks command.Outputs |> Array.ofSeq
                        let! actualOutputItemVersions = extractActualVersionsFromLinks command.Outputs
                        let areOutputsValid = areValidItemsVersions expectedOutputItemVersions actualOutputItemVersions
                        if not areOutputsValid then
                            logVerbose "Needs recomputation due to the outdated outputs"
                        return not areOutputsValid
                }

       
            if doComputations then
                // We need to do computation            
                // 1) deleting outputs if they exist   
                // 2) restoring inputs from storage if it is needed
                // 3) execute external command
                // 4) upon 0 exit code hash the outputs
                // 5) fill in corresponding method vertex (to fix proper versions)
                // 6) write alph files for outputs

                // 1) Deleting outputs
                if not command.DoNotCleanOutputs then
                    outputPaths |> List.iter deletePath

                // 2) restoring inputs from storage if it is needed
                let inputChooser (input:LinkToArtefact) =
                    async {
                        let! actualHashOpt = extractActualVersionsFromLinks [input]
                        if Option.isSome actualHashOpt.[0] then
                            return None
                        else
                            return Some(input)
                    }
                let! toRestoreOts = Seq.map inputChooser command.Inputs |> Array.ofSeq |> Async.Parallel
                let toRestore = Array.choose id toRestoreOts
                let hashesToRestore = toRestore |> extractExpectedVersionsFromLinks |> Seq.map (fun x -> x.Value)
                let pathsToRestore = toRestore |> Seq.map (fun x -> idToFullPath experimentRoot x.Artefact.Id |> applyIndex index )
                let zipped = Seq.zip hashesToRestore pathsToRestore |> Array.ofSeq
                if Array.length zipped > 0 then
                    logVerbose (sprintf "Restoring missing inputs from storage...")
                    do! restoreFromStorage zipped
                    logVerbose (sprintf "Inputs are restored")


                // 3) executing a command
                let print (s:string) = Console.WriteLine s
                let input idx = command.Inputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot |> applyIndex index
                let output idx = command.Outputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot |> applyIndex index
                let context : ComputationContext = { ExperimentRoot = experimentRoot; Print = print  }
                let exitCode = command |> ExecuteCommand.runCommandLineMethodAndWait context (input, output) 

                // 4) upon 0 exit code hash the outputs
                if exitCode <> 0 then
                    raise(InvalidOperationException(sprintf "Process exited with exit code %d" exitCode))
                else
                    logVerbose (sprintf "Program succeeded. Calculating hashes of the outputs...")
                    // 5a) hashing outputs disk content                
                    // 5b) updating dependency versions in dependency graph
                    // 6) dumping updated alph files to disk
                    do! command.OnSucceeded(index)
                    logVerbose "alph file saved"
            else
                logVerbose "skipping as up to date"
            //comp.Outputs |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.ExpectedVersion) |> List.ofSeq

            let outPathToArtefactItem outputPath : Artefact =
                upcast { FullPath = outputPath; Index = index }
            let result =  Seq.singleton(outputPaths |> List.map outPathToArtefactItem, null)
            return result
        } |> Async.RunSynchronously


let buildGraph experimentRoot (g:DependencyGraph.Graph) checkStoragePresence restoreFromStorage =    
    let factory method : ComputationGraphNode = 
        match method with 
        | DependencyGraph.Source src -> upcast SourceMethod(src, experimentRoot, checkStoragePresence, restoreFromStorage) 
        | DependencyGraph.Command cmd -> upcast CommandMethod(cmd, experimentRoot, checkStoragePresence, restoreFromStorage)

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
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
        Error(SystemError(sprintf "Failed to compute the artefacts: \n\t%s" failed))