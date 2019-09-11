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
open ItisLab.Alpheus.AngaraGraphCommon


type ArtefactItem =
    { FullPath: string
      Index: string list
      }

type SourceMethod(source: SourceVertex, experimentRoot,
                    checkStoragePresence : HashString seq -> Async<bool array>) = 
    inherit AngaraGraphNode(DependencyGraph.Source source)

    override s.Execute(_, _) = // ignoring checkpoints
        async {
            let expectedArtefact = source.Output
            let artefact = expectedArtefact.Artefact

            let ensureScalar (artefactVersion:ArtefactVersion) =
                if not artefactVersion.IsScalar then
                    failwithf "Source artefacts can't be vectored: %A" artefact.Id
                else
                    artefactVersion.AsScalar()

            let expectedVersionOpt = expectedArtefact.ExpectedVersion |> ensureScalar
        
            let! expectedV =
                async {
                    match expectedVersionOpt with
                    | None ->
                        // this could be file/dir without alph file.
                        let! v = artefact.ActualVersionAsync
                        return (v |> ensureScalar).Value
                    | Some(v) -> return v
                }

            let! actualVersionRes = artefact.ActualVersionAsync
            let actualVersionOpt = ensureScalar actualVersionRes

            match actualVersionOpt with
            |   None -> // The artefact does not exist on disk
                // This may be OK in case the specified version is available in storages
                // todo: note that in case of vectore, only some of the items can exists/restore/etc.
                let! presenceCheck = checkStoragePresence [|expectedV|]
                if presenceCheck.[0] then
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
            
            // Output of the method is an scalar or a vector of full paths to the data of the artefact.
            let outputArtefact : Artefact =
                artefact.Id 
                |> PathUtils.enumerateItems experimentRoot
                |> MdMap.toTree
                |> toJaggedArrayOrValue (fun (index, fullPath) -> { FullPath = fullPath; Index = index }) []
        

            return Seq.singleton ([outputArtefact], null)
        } |> Async.RunSynchronously

type CommandMethod(command: CommandLineVertex,
                    experimentRoot,
                    checkStoragePresence: HashString seq -> Async<bool array>,
                    restoreFromStorage: (HashString*string) array -> Async<unit>) = // version*filename
    inherit AngaraGraphNode(DependencyGraph.Command command)  
    
    override s.Execute(inputs, _) = // ignoring checkpoints
        async{
            // Rules of execution
            // The artefact is valid either if actual disk version matches expected version or if the disk version is absent and expected version is restorable from storage
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

            let! currentVertexStatus = getCommandVertexStatus checkStoragePresence command index
       
            match currentVertexStatus with
            |   Outdated _ ->
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
                        let! actualHashOpt = extractActualVersionsFromLinks index [input]
                        if Option.isSome actualHashOpt.[0] then
                            return None
                        else
                            return Some(input)
                    }
                let! toRestoreOts = Seq.map inputChooser command.Inputs |> Array.ofSeq |> Async.Parallel
                let toRestore = Array.choose id toRestoreOts
                let hashesToRestore = toRestore |> extractExpectedVersionsFromLinks index |> Seq.map (fun x -> x.Value)
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
            |   UpToDate _ ->
                logVerbose "skipping as up to date"
            //comp.Outputs |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.ExpectedVersion) |> List.ofSeq

            let outPathToArtefactItem outputPath : Artefact =
                upcast { FullPath = outputPath; Index = index }
            let result =  Seq.singleton(outputPaths |> List.map outPathToArtefactItem, null)
            return result
        } |> Async.RunSynchronously


let buildGraph experimentRoot (g:DependencyGraph.Graph) checkStoragePresence restoreFromStorage =    
    let factory method : AngaraGraphNode = 
        match method with
        | DependencyGraph.Source src -> upcast SourceMethod(src, experimentRoot, checkStoragePresence) 
        | DependencyGraph.Command cmd -> upcast CommandMethod(cmd, experimentRoot, checkStoragePresence, restoreFromStorage)

    g |> DependencyGraphToAngaraWrapper |> AngaraTranslator.translate factory

let doComputations (g:FlowGraph<AngaraGraphNode>) = 
    let state  = 
        {
            TimeIndex = 0UL
            Graph = g
            Vertices = Map.empty
        }
    try
        use engine = new Engine<AngaraGraphNode>(state,Scheduler.ThreadPool())        
        engine.Start()
        
        // engine.Changes.Subscribe(fun x -> x.State.Vertices)
        let final = Control.pickFinal engine.Changes
        let finalState = final.GetResult()
        Ok()
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
        Error(SystemError(sprintf "Failed to compute the artefacts: \n\t%s" failed))