module ItisLab.Alpheus.ComputationGraph

open System
open System.IO
open ItisLab.Alpheus.Hash
open AlphFiles
open Angara.Graph
open Angara.Execution
open Angara.States
open ItisLab.Alpheus.DependencyGraph

type ComputedArtefact = {
    FullID: ArtefactFullID
    Hash: HashString
}

[<AbstractClass>]
type ComputationGraphNode(depCount,outCount) = 
    inherit ExecutableMethod(System.Guid.NewGuid(), [ for i in 0..(depCount-1) -> typeof<ComputedArtefact>] , [for i in 0..(outCount-1) -> typeof<ComputedArtefact>])
    
type SourceGraphNode(orphanArtefact:DependencyGraph.ArtefactVertex, experimentRoot:string) =
    inherit ComputationGraphNode(0,1)

    override s.Execute(_, _) = //ignoring inputs and checkpoint.
        // Just utilizing parallel method computation feature of AngaraFlow to check the statuses of all method vertex
        // source method is always up to date, thus succeeds
        let hash =
            match orphanArtefact.ActualHash with
            |   None -> raise(InvalidOperationException("Source artefact must be on disk"))
            |   Some(actualHash) ->
                // dumping actual hash as expected
                let dumpComputations = 
                    async {
                        let filePath = Path.Combine(experimentRoot,orphanArtefact.FullID)
                        let alphFilePath = sprintf "%s.alph" filePath
                        let! alphLoadResults = AlphFiles.tryLoadAsync alphFilePath
                        let alphToDump =
                            match alphLoadResults with
                            |   None ->
                                {
                                    IsTracked = false
                                    Origin = Snapshot actualHash
                                }
                            |   Some(alphFile) ->
                                {
                                    alphFile with
                                        Origin = Snapshot actualHash
                                }
                        
                        do! AlphFiles.saveAsync alphToDump alphFilePath
                        return actualHash
                    }
                Async.RunSynchronously dumpComputations            
        seq{ yield [{FullID = orphanArtefact.FullID; Hash = hash} :> Artefact], null }

type IntermediateGraphNode(methodVertex:DependencyGraph.ComputedVertex, experimentRoot:string) =
    inherit ComputationGraphNode(methodVertex.Inputs.Count, methodVertex.Outputs.Count)

    member s.FirstOutputID =
        methodVertex.FirstOutputFullID

    override s.Execute(inputs, _) = //ignoring checkpoint.        
        let logVerbose str =
            printfn "Computation %20s:\t\t%s" methodVertex.FirstOutputFullID str

        let actualInputs = inputs |> List.map (fun x -> x:?> ComputedArtefact) |> List.sortBy (fun x -> x.FullID)
        let inputVertices = methodVertex.Inputs |> Set.toList |> List.sortBy (fun x -> x.Artefact.FullID)

        if List.exists2 (fun (art:ComputedArtefact) (inp:DependencyGraph.VersionedArtefact) -> art.FullID <> inp.Artefact.FullID) actualInputs inputVertices then
            raise(InvalidOperationException("Expected and actual vertex inputs are inconsistent. This is Alpheus error."))
        
        let outputsArray = methodVertex.Outputs |> Set.toArray //the order is important here (the internal ordering of the set)

        // intermediate graph node is actually a command execution
        // First, we need to decide whether the computation can be bypassed (the node is up to date) or the computation must be invoked               


        let doComputation =       
            if List.exists2 (fun (art:ComputedArtefact) (inp:DependencyGraph.VersionedArtefact) -> art.Hash <> inp.Version) actualInputs inputVertices then
                // input versions differ
                true
            else                    
                // NOTE: signature is checked during the .alph file reading
                // if it is invalid, output versions are empty
                let isVersionMismatch expected actual =
                    // printfn "output current %A expected %s" actual expected
                    match actual with
                    |   None -> true // if actual file is missing we do need to do recomputation
                    |   Some(actual) -> expected <> actual
                Array.exists (fun (output:DependencyGraph.VersionedArtefact) -> isVersionMismatch output.Version output.Artefact.ActualHash) outputsArray
        
        if doComputation then
            // We need to do computation            
            // 1) deleting outputs if they exist            
            // 2) execute external command
            // 3) upon 0 exit code hash the outputs
            // 5) fill in corresponding method vertex (to fix propper versions)
            // 4) write alph files for outputs

            // 1) Deleting outputs
            if not methodVertex.DoNotCleanOutputs then
                let deletePath path =
                    if File.Exists path then
                        File.Delete path
                    elif Directory.Exists path then
                        Directory.Delete(path,true)
                    else
                        ()
                let fullOutputPaths = Seq.map (fun (x:DependencyGraph.VersionedArtefact) -> Path.GetRelativePath(experimentRoot,x.Artefact.FullID)) methodVertex.Outputs |> List.ofSeq
                List.iter deletePath fullOutputPaths

            // 2) executing a command
            let command = methodVertex.Command.Trim()
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
            p.StartInfo.WorkingDirectory <- Path.GetFullPath(Path.Combine(experimentRoot, methodVertex.WorkingDirectory))
            p.StartInfo.RedirectStandardError <- true
            p.StartInfo.RedirectStandardOutput <- true
            p.StartInfo.UseShellExecute <- false
            p.StartInfo.CreateNoWindow <- true
            logVerbose (sprintf "Executing \"%s %s\". Working dir is \"%s\"" program args p.StartInfo.WorkingDirectory)
            p.Start() |> ignore
                        
            streamPrinterAsync (sprintf "%s [stdout]" methodVertex.FirstOutputFullID) p.StandardOutput |> Async.Start
            streamPrinterAsync (sprintf "%s [stderr]" methodVertex.FirstOutputFullID) p.StandardError |> Async.Start 
            
            p.WaitForExit()            

            // 3) upon 0 exit code hash the outputs
            if p.ExitCode <> 0 then
                raise(InvalidOperationException(sprintf "Process exited with exit code %d"p.ExitCode))
            else
                // hashing outputs disk content                
                let hashComputeation = DependencyGraph.fillinActualHashesAsync (outputsArray |> Array.map (fun art -> art.Artefact)) experimentRoot
                logVerbose (sprintf "Calculated successfully. Calculating output hashes")
                Async.RunSynchronously hashComputeation
                
                // updating dependency versions in dependency graph
                let updateVersions (artefacts:Set<DependencyGraph.VersionedArtefact>) =
                    let updateVersion (art:DependencyGraph.VersionedArtefact) = 
                        {
                            art with
                                //TODO: handle absence of output file/dir
                                Version = art.Artefact.ActualHash.Value //the value must be here, absence means that the output were not created
                        }
                    Set.map updateVersion artefacts
                methodVertex.Inputs <- updateVersions methodVertex.Inputs
                methodVertex.Outputs <- updateVersions methodVertex.Outputs

                // dumping to disk
                let diskDumpComputation = 
                    async {                        
                        let outputAlphfilePaths = Array.map (fun (x:DependencyGraph.VersionedArtefact) -> (sprintf "%s.alph" (Path.Combine(experimentRoot,x.Artefact.FullID)))) outputsArray

                        let updateAlphFileAsync (alphFilePath:string) artefact =
                            async {
                                let alphFileDir = Path.GetFullPath(Path.GetDirectoryName(alphFilePath))
                                let alphFile = DependencyGraph.artefactToAlphFile artefact.Artefact alphFileDir experimentRoot
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
        let results = methodVertex.Outputs |> Set.toSeq |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> {FullID=output.Artefact.FullID; Hash = output.Version} :> Artefact) |> List.ofSeq
        seq { yield (results, null) }


let buildStatusGraph experimentRoot (g:DependencyGraph.Graph) =    
    let factory method : ComputationGraphNode =
        match method with
        |   ProducerVertex.Source(source) -> upcast SourceGraphNode(source.Artefact.Artefact,experimentRoot)
        |   ProducerVertex.Computed(computed) -> upcast IntermediateGraphNode(computed,experimentRoot)
    FlowGraphFactory.buildStatusGraph g factory

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