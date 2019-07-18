module ItisLab.Alpheus.ComputationGraph

open System
open System.IO
open ItisLab.Alpheus.Hash
open AlphFiles
open Angara.Graph
open Angara.Execution
open Angara.States
open ItisLab.Alpheus.DependencyGraph

/// extracts the number of input artefacts used in producer vertex
let private getDepCount producerVertex =
    match producerVertex with
    | Source(_) -> 0
    | Command(c) -> c.Inputs.Count

/// extracts the number of artefacts produced by the vertex
let private getOutCount producerVertex =
    match producerVertex with
    | Source(_) -> 1
    | Command(c) -> c.Outputs.Count

/// This type represents an Angara Flow method.
/// Note that execution modifies the given vertex and it is Angara Flow execution runtime who controls
/// the concurrency.
type ComputationGraphNode(producerVertex:MethodVertex, experimentRoot:string) = 
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        List.init (getDepCount producerVertex) (fun _ -> typeof<ArtefactId>),
        List.init (getOutCount producerVertex) (fun _ -> typeof<ArtefactId>))

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.Artefact.Artefact.Id
        |   Command(comp) -> (Seq.head comp.Outputs).Artefact.Id // first output is used as vertex ID

    override s.Execute(_, _) = // ignoring checkpoints
        // we behave differently for source vertices and computed vertices            
        let outputVersions =
            match producerVertex with
            |   Source(sourceVertex) ->
                // source vertex can always produce output
                let expectedVersion = sourceVertex.Artefact.Version
                match sourceVertex.Artefact.Artefact.ActualHash with
                |   None ->
                    // The artefact does not exist on disk
                    // This may be OK in case the specified version is contained available in storages
                    if sourceVertex.Artefact.StoragesContainingVersion.IsEmpty then
                        invalidOp (sprintf "The source artefact must either exist on local disk or be restorable from storage: %A" sourceVertex.Artefact.Artefact.Id)
                    else
                        // we don't need to save alph file as
                        // 1) not tracked artefacts does not initially have alph file and do not need them
                        // 2) tracked artefact already have alph files on disk
                        ()
                |   Some(diskVersion) ->
                        // if alph file exists on disk (e.g. isTracked), we need to resave it to update the expected version
                        if sourceVertex.Artefact.Artefact.IsTracked then
                            let dumpComp = 
                                async {
                                    let artefactFullPath = fullIDtoFullPath experimentRoot sourceVertex.Artefact.Artefact.Id
                                    let artefactType =
                                        if isFullIDDirectory sourceVertex.Artefact.Artefact.Id then DirectoryArtefact else FileArtefact
                                    let alphFileFullPath = artefactPathToAlphFilePath artefactFullPath
                                    // but if the .alph file is present, we need to update it's version during the execution
                                    let snapshotSection = 
                                        {
                                            Type = artefactType
                                            Version = diskVersion
                                        }
                                    let alphFile = artefactToAlphFile sourceVertex.Artefact.Artefact artefactFullPath experimentRoot
                                    let alphFile =
                                        {
                                            alphFile with
                                                Origin = Snapshot snapshotSection
                                        }
                                    do! AlphFiles.saveAsync alphFile alphFileFullPath
                                }                    
                            Async.RunSynchronously dumpComp
                [sourceVertex.Artefact.Version]
            |   Command(comp) ->
                // if Angara.Flow calls execute, it means that the inputs are ready to be used in computation
  
                let logVerbose str =
                    printfn "Computation %20s:\t\t%s" comp.MethodId str

                //let inputVertices = methodVertex.Inputs |> Set.toList |> List.sortBy (fun x -> x.Artefact.FullID)

                let outputs = comp.Outputs // the order is important here

                // intermediate graph node is actually a command execution
                // First, we need to decide whether the computation can be bypassed (the node is up to date) or the computation must be invoked               

                /// versions present and match
                let versionsMatch v1 v2 =
                    match v1,v2 with
                    |   Some(vv1),Some(vv2) -> vv1 = vv2
                    |   None,Some(_) | Some(_),None | None,None -> false

                let doComputation =
                    // NOTE: signature is checked during the .alph file reading
                    // if it is invalid, output versions are empty
                    let isArtefactAvailable (art:VersionedArtefact) =
                        (versionsMatch art.Version art.Artefact.ActualHash) || (not art.StoragesContainingVersion.IsEmpty)
                       
                    // if any of the outputs is unavailable, we have to run the computation
                    not(Seq.forall isArtefactAvailable outputs)
                        
                if doComputation then
                    // We need to do computation            
                    // 1) deleting outputs if they exist            
                    // 2) execute external command
                    // 3) upon 0 exit code hash the outputs
                    // 4) fill in corresponding method vertex (to fix propper versions)
                    // 5) write alph files for outputs

                    // 1) Deleting outputs
                    if not comp.DoNotCleanOutputs then
                        let deletePath (path:string) =
                            if path.EndsWith(Path.DirectorySeparatorChar) then
                                if Directory.Exists path then
                                    Directory.Delete(path,true)
                            else
                                if File.Exists path then
                                    File.Delete path
                        let fullOutputPaths = Seq.map (fun (x:DependencyGraph.VersionedArtefact) -> AlphFiles.fullIDtoFullPath experimentRoot x.Artefact.Id) comp.Outputs |> List.ofSeq
                        List.iter deletePath fullOutputPaths

                    // 2) executing a command
                    let print (s:string) = Console.WriteLine s
                    let input idx = comp.Inputs.[idx-1].Artefact.Id.GetFullPath(experimentRoot)
                    let output idx = comp.Outputs.[idx-1].Artefact.Id.GetFullPath(experimentRoot)
                    let context : ComputationContext = { ExperimentRoot = experimentRoot; Print = print  }
                    let exitCode = comp |> ExecuteCommand.runAndWait context (input, output) 

                    // 3) upon 0 exit code hash the outputs
                    if exitCode <> 0 then
                        raise(InvalidOperationException(sprintf "Process exited with exit code %d" exitCode))
                    else
                        // 4a) hashing outputs disk content                
                        let hashComputeation = DependencyGraph.fillinActualHashesAsync (outputs |> Seq.map (fun art -> art.Artefact)) experimentRoot
                        logVerbose (sprintf "Calculated successfully. Calculating output hashes")
                        Async.RunSynchronously hashComputeation
                    
                        // 4b) updating dependency versions in dependency graph
                        let updateVersion (art:DependencyGraph.VersionedArtefact) = 
                            { art with Version = art.Artefact.ActualHash }
                        comp.UpdateArtefacts updateVersion
            
                        // 5) dumping updated alph files to disk
                        let diskDumpComputation = 
                            async {                        
                                let fullIDToFullAlphPath versionedArtefact =
                                    let fullArtefactPath = fullIDtoFullPath experimentRoot versionedArtefact.Artefact.Id
                                    artefactPathToAlphFilePath fullArtefactPath
                                let outputAlphfilePaths = Seq.map fullIDToFullAlphPath outputs |> Seq.toArray

                                let updateAlphFileAsync (alphFilePath:string) artefact =
                                    async {
                                        let alphFileFullPath = Path.GetFullPath(alphFilePath)
                                        let alphFile = DependencyGraph.artefactToAlphFile artefact.Artefact alphFileFullPath experimentRoot
                                        do! AlphFiles.saveAsync alphFile alphFilePath
                                    }
                                
                                let alphUpdatesComputations = Seq.map2 updateAlphFileAsync outputAlphfilePaths outputs
                                let! _ = Async.Parallel alphUpdatesComputations
                                return ()
                            }
                        Async.RunSynchronously diskDumpComputation 
                        logVerbose "Outputs metadata saved"
                else
                    logVerbose "skipping as up to date"
                comp.Outputs |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.Version) |> List.ofSeq
 
        let outputCasted = List.map (fun x -> x :> Artefact) outputVersions
        seq{ yield outputCasted, null }

let buildGraph experimentRoot (g:DependencyGraph.Graph) =    
    let factory method : ComputationGraphNode = ComputationGraphNode(method,experimentRoot)
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
        0
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.Message))
        printfn "Failed to compute the artefacts: \n\t%s" failed
        1