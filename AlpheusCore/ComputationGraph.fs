﻿module ItisLab.Alpheus.ComputationGraph

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
    | Computed(c) -> c.Inputs.Count

/// extracts the number of artefacts produced by the vertex
let private getOutCount producerVertex =
    match producerVertex with
    | Source(_) -> 1
    | Computed(c) -> c.Outputs.Count

type ComputationGraphNode(producerVertex:ProducerVertex, experimentRoot:string) = 
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        [ for i in 0..((getDepCount producerVertex) - 1) -> typeof<ArtefactFullID>] ,
        [for i in 0..((getOutCount producerVertex) - 1) -> typeof<ArtefactFullID>])

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.Artefact.Artefact.FullID
        |   Computed(comp) -> (Seq.head comp.Outputs).Artefact.FullID // first output is used as vertex ID

    override s.Execute(inputVersions, _) = // ignoring checkpoints
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
                        invalidOp "The source artefact must either exist on local disk or be restorable from storage"
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
                                    let artefactFullPath = fullIDtoFullPath experimentRoot sourceVertex.Artefact.Artefact.FullID
                                    let artefactType =
                                        if isFullIDDirectory sourceVertex.Artefact.Artefact.FullID then DirectoryArtefact else FileArtefact
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
            |   Computed(comp) ->
                // if Angara.Flow calls execute, it means that the inputs are ready to be used in computation
  
                let logVerbose str =
                    printfn "Computation %20s:\t\t%s" (AlphFiles.fullIDtoString comp.FirstOutputFullID) str

                //let inputVertices = methodVertex.Inputs |> Set.toList |> List.sortBy (fun x -> x.Artefact.FullID)

                let outputsArray = comp.Outputs |> Set.toArray //the order is important here (the internal ordering of the set is used)

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
                    not(Array.forall isArtefactAvailable outputsArray)
                        
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
                        let fullOutputPaths = Seq.map (fun (x:DependencyGraph.VersionedArtefact) -> AlphFiles.fullIDtoFullPath experimentRoot x.Artefact.FullID) comp.Outputs |> List.ofSeq
                        List.iter deletePath fullOutputPaths

                    // 2) executing a command
                    let command = comp.Command.Trim()
                    let program,args =
                        match Seq.tryFindIndex (fun c -> Char.IsWhiteSpace(c)) command with
                        |   Some(idx) -> command.Substring(0,idx),command.Substring(idx).Trim()
                        |   None -> command,""
                
                    let streamPrinterAsync name (stream:StreamReader) = 
                        async {
                            do! Async.SwitchToNewThread()
                            while not stream.EndOfStream do
                                let! line = Async.AwaitTask(stream.ReadLineAsync())
                                let annotatedLine = sprintf "[%40s]:\t%s" name line
                                printfn "%s" annotatedLine                    
                        }

                    use p = new System.Diagnostics.Process()
                

                    p.StartInfo.FileName <- program
                    p.StartInfo.Arguments <- args
                    p.StartInfo.WorkingDirectory <- Path.GetFullPath(Path.Combine(experimentRoot, comp.WorkingDirectory))
                    p.StartInfo.RedirectStandardError <- true
                    p.StartInfo.RedirectStandardOutput <- true
                    p.StartInfo.UseShellExecute <- false
                    p.StartInfo.CreateNoWindow <- true
                    logVerbose (sprintf "Executing \"%s %s\". Working dir is \"%s\"" program args p.StartInfo.WorkingDirectory)
                    p.Start() |> ignore
                            
                    streamPrinterAsync (sprintf "%s [stdout]" (AlphFiles.fullIDtoString comp.FirstOutputFullID)) p.StandardOutput |> Async.Start
                    streamPrinterAsync (sprintf "%s [stderr]"(AlphFiles.fullIDtoString comp.FirstOutputFullID)) p.StandardError |> Async.Start 
                
                    p.WaitForExit()            

                    // 3) upon 0 exit code hash the outputs
                    if p.ExitCode <> 0 then
                        raise(InvalidOperationException(sprintf "Process exited with exit code %d" p.ExitCode))
                    else
                        // 4a) hashing outputs disk content                
                        let hashComputeation = DependencyGraph.fillinActualHashesAsync (outputsArray |> Array.map (fun art -> art.Artefact)) experimentRoot
                        logVerbose (sprintf "Calculated successfully. Calculating output hashes")
                        Async.RunSynchronously hashComputeation
                    
                        // 4b) updating dependency versions in dependency graph
                        let updateVersions (artefacts:Set<DependencyGraph.VersionedArtefact>) =
                            let updateVersion (art:DependencyGraph.VersionedArtefact) = 
                                {
                                    art with                                
                                        Version = art.Artefact.ActualHash
                                }
                            Set.map updateVersion artefacts
                        comp.Inputs <- updateVersions comp.Inputs
                        comp.Outputs <- updateVersions comp.Outputs
            
                        // 5) dumping updated alph files to disk
                        let diskDumpComputation = 
                            async {                        
                                let fullIDToFullAlphPath versionedArtefact =
                                    let fullArtefactPath = fullIDtoFullPath experimentRoot versionedArtefact.Artefact.FullID
                                    artefactPathToAlphFilePath fullArtefactPath
                                let outputAlphfilePaths = Array.map fullIDToFullAlphPath outputsArray

                                let updateAlphFileAsync (alphFilePath:string) artefact =
                                    async {
                                        let alphFileFullPath = Path.GetFullPath(alphFilePath)
                                        let alphFile = DependencyGraph.artefactToAlphFile artefact.Artefact alphFileFullPath experimentRoot
                                        do! AlphFiles.saveAsync alphFile alphFilePath
                                    }
                                
                                let alphUpdatesComputations = Array.map2 updateAlphFileAsync outputAlphfilePaths outputsArray
                                let! _ = Async.Parallel alphUpdatesComputations
                                return ()
                            }
                        Async.RunSynchronously diskDumpComputation 
                        logVerbose "Outputs metadata saved"
                else
                    logVerbose "skipping as up to date"
                comp.Outputs |> Set.toSeq |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.Version) |> List.ofSeq
 
        let outputCasted = List.map (fun x -> x :> Artefact) outputVersions
        seq{ yield outputCasted, null }




let buildGraph experimentRoot (g:DependencyGraph.Graph) =    
    let factory method : ComputationGraphNode = ComputationGraphNode(method,experimentRoot)
    FlowGraphFactory.buildGraph g factory

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