﻿module ItisLab.Alpheus.ComputationGraph

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
open FSharp.Control

type ArtefactItemUpdateType =
    /// perform computation with this item
    |   Process
    /// propagate delete of this item
    |   Delete

type ArtefactItem =
    { FullPath: string
      Index: string list
      UpdateType: ArtefactItemUpdateType
      }

type NonSuccessfulExitCodeException(str:string) =
    inherit Exception(str)

type StorageRelatedUserError(str:string) =
    inherit System.Exception(str)


type SourceMethod(source: SourceVertex, experimentRoot) = 
    inherit AngaraGraphNode<ArtefactItem>(DependencyGraph.Source source)

    override s.Execute(_, _) = // ignoring checkpoints
        async {
            // we need to do two things here
            // 1. Update Alph file (if needed)
            // 2. Construct the ArtefactItem[s] and pass them as method result
            let expectedArtefact = source.Output
            let artefact = expectedArtefact.Artefact
            let diskItems = artefact.Id |> PathUtils.enumerateItems experimentRoot
            let expectedVersions = expectedArtefact.ExpectedVersion
            let actualIndices = diskItems |> MdMap.toSeq |> Seq.map fst
            let! actualVersionsStrached = actualIndices |> Seq.map artefact.ActualVersion.Get |> Async.Parallel
            let actualVersions = Seq.zip actualIndices actualVersionsStrached |> Seq.fold (fun s e -> let k,v = e in MdMap.add k v s) MdMap.empty

            let merger _ expectedOpt actualOpt = (Option.flatten expectedOpt), (Option.flatten actualOpt)
            let expAndAct = Utils.mdmapMerge merger expectedVersions actualVersions

            let alphExists = source.Output.Artefact.Id |> PathUtils.idToAlphFileFullPath source.ExperimentRoot |> File.Exists
            // 1. Update Alph file (if needed)
            // if alph file exists on disk (e.g. isTracked), we need to re-save it to update the expected version
            // also we need to save alph file in case we have vector output. It is needed to track which indices were generate on previous run
            // on the updated data run, the indices that disapperead will be propagated as delete operation
            if alphExists || (not expAndAct.IsScalar) then
                // we need to update (expect actual in) alph file only if there is something on disk
                // otherwise the artefact can be saved and restored later from storages, so we do not need to override expectations
                let doUpdateAlphFile = expAndAct |> MdMap.toSeq |> Seq.exists (fun p -> let _,(_,actual) = p in actual.IsSome)
                if doUpdateAlphFile then
                    do! expAndAct |> MdMap.toSeq |> Seq.map (fun p -> let idx,_  = p in source.Output.ExpectActualVersionAsync idx) |>  Async.Parallel |> Async.Ignore
                    Logger.logVerbose Logger.DependencyGraph (sprintf "Written alph file for source artefact (%O), as there is some data for the artefact on disk" source.Output)
                artefact.SaveAlphFile()
            
            // 2. Constructing the ArtefactItem[s] and pass them as method result
            // Output of the method is an scalar or a vector of full paths to the data of the artefact.
            let pairToStatus pair =
                let expectedOp,actualOp = pair
                match expectedOp,actualOp with
                |   Some(_),Some(_) -> Process
                |   Some(_), None -> Delete
                |   None, Some(_) -> Process
                |   None,None -> Delete // why this can happen?
            let diskItemExists pair =
                let expectedOp,(actualOp:'a option) = pair
                actualOp.IsSome
            let idxToPath idx =
                PathUtils.idToFullPath experimentRoot source.Output.Artefact.Id |> PathUtils.applyIndex idx
            let outputArtefact : Artefact =
                if expAndAct.IsScalar then
                    // in case of scalar we do not utilize vector element delete feature. Thus always process
                    upcast { FullPath = idxToPath []; Index = []; UpdateType = Process }
                else
                    let itemMapper =
                        let vectorIsNotEmpty = expAndAct |> MdMap.toSeq |> Seq.exists (fun p -> let _,v = p in diskItemExists v )
                        if vectorIsNotEmpty then
                            (fun (index, v) -> { FullPath = idxToPath index; Index = index; UpdateType = pairToStatus v })
                        else
                            // nothing is on disk. We hope the vector will be restored from storages. Thus Update type is not 'delete'
                            (fun (index, _) -> { FullPath = idxToPath index; Index = index; UpdateType = Process }) 
                    expAndAct
                    |> toJaggedArrayOrValue itemMapper

            return Seq.singleton ([outputArtefact], null)
        } |> Async.RunSynchronously

type CommandMethod(command: CommandLineVertex,
                    experimentRoot,
                    checkStoragePresence: HashString seq -> Async<bool array>,
                    restoreFromStorage: (HashString*string) array -> Async<Result<unit,AlpheusError>>, // version*filename
                    resourceSemaphores: Map<string,System.Threading.SemaphoreSlim> ref) = 
    inherit AngaraGraphNode<ArtefactItem>(DependencyGraph.Command command)  

    let reduceArtefactItem inputN (vector: ArtefactItem[]) : ArtefactItem =
        if vector.Length > 0 then
            let fullIndex = vector.[0].Index
            let reducedIndex = fullIndex |> List.truncate (fullIndex.Length-1)
            let path = command.Inputs.[inputN].Artefact.Id |> PathUtils.idToFullPath experimentRoot |> PathUtils.applyIndex reducedIndex
            { FullPath = path; Index = reducedIndex; UpdateType=Process } // is it valid to always treat reduce as process and never as delete?
        else failwith "Input is empty (no artefacts to reduce)"


    override s.Execute(inputs, _) = // ignoring checkpoints
        async{
            // Rules of execution
            // The artefact is valid either if actual disk version matches expected version or if the disk version is absent and expected version is restorable from storage
            // We can bypass the computation entirely if inputs and outputs are valid
        
           
            // If any input, output is not valid we need to
            //  if input status brings 'delete'
            //      1) we need to propagate the delete: delete disk content, output in delete ArtefactItem
            //  otherwise
            //      1) restore inputs if they are absent on disk
            //      2) execute the command
            
            let inputItems = inputs |> List.mapi (fun i inp -> 
                match inp with
                | :? ArtefactItem as item -> item
                | :? (ArtefactItem[]) as vector -> reduceArtefactItem i vector
                | _ -> failwith "Unexpected type of the input")
            
            // the longest defined index among all of the inputs
            let index = // empty list for scalar, nonempty for vector element
                inputItems 
                |> Seq.map(fun item -> item.Index)
                |> Seq.fold(fun (max: string list) index -> if index.Length > max.Length then index else max) []
            let methodItemId = command.MethodId |> applyIndex index

            let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%s%A: %s" methodItemId index str)
            let logLongRunningStart str = Logger.logVerboseLongRunningStart Logger.Execution (sprintf "%s%A: %s" methodItemId index str)
            let logLongRunningFinish ct str = Logger.logVerboseLongRunningFinish ct Logger.Execution (sprintf "%s%A: %s" methodItemId index str)
            let logInfo str = Logger.logVerbose Logger.Execution (sprintf "%s%A: %s" methodItemId index str)
            let logError str = Logger.logError Logger.Execution (sprintf "%s%A: %s" methodItemId index str)
            logVerbose "Started"

            let deletedIndices =
                let isDelete = function
                    |   Delete -> true
                    |   Process -> false
                inputItems |> Seq.filter (fun item -> isDelete item.UpdateType) |> Seq.map (fun x -> x.Index) |> Seq.distinct |> Array.ofSeq


            // Build the full output paths by applying the index of this method.
            // Note that in case of scatter, these still might contain '*'
            let outputPaths = 
                command.Outputs // the order is important here
                |> List.map(fun out -> out.Artefact.Id |> PathUtils.idToFullPath experimentRoot |> applyIndex index)
            if not (Array.isEmpty deletedIndices) then
                // we need to delete the outputs in order to propagate input
                logVerbose (sprintf "Deleting the outputs as input was deleted in vectorized operation")
                let deleteDiskData (outputPath:string) =
                    let deletePath path =
                        logVerbose (sprintf "Deleting path %s" path)
                        PathUtils.deletePath path
                    if outputPath.Contains('*') then
                        let outputItems = PathUtils.enumeratePath outputPath
                        let paths = outputItems |> MdMap.toSeq |> Seq.map snd
                        paths |> Seq.iter deletePath
                    else
                        deletePath outputPath
                outputPaths |> Seq.iter deleteDiskData

                do! command.OnSucceeded(index) // we erase the expectations for current index

                let outPathToArtefactItem (outputPath:string) : Artefact =
                    if outputPath.Contains('*') then
                        let outputItems = PathUtils.enumeratePath outputPath
                        outputItems |> toJaggedArrayOrValue (fun (extraIndex, itemFullPath) -> { FullPath = itemFullPath; Index = index @ extraIndex; UpdateType=Delete }) 
                    else
                        upcast { FullPath = outputPath; Index = index; UpdateType=Delete }
                return Seq.singleton(outputPaths |> List.map outPathToArtefactItem, null)
            else
                // not vector element delete branch
                let! currentVertexStatus = getCommandVertexStatus checkStoragePresence command index
       
                match currentVertexStatus with
                |   Outdated _ ->
                    // We need to do computation            
                    // 1) clearing the outputs if they exist   
                    // 2) restoring inputs from storage if it is needed
                    // 3) execute external command
                    // 4) upon 0 exit code hash the outputs
                    // 5) fill in corresponding method vertex (to fix proper versions)
                    // 6) write alph files for outputs

                    // 1) Deleting outputs
                    let recreatePath path =                    
                        if not command.DoNotCleanOutputs then
                            deletePath path
                        ensureDirectories path
                
                    outputPaths |> List.iter (fun path -> 
                        if path.Contains '*' then 
                            enumeratePath path |> MdMap.toSeq |> Seq.iter(fun (i,fullPath) -> recreatePath fullPath)
                        else 
                            recreatePath path)

                    // 2) restoring inputs from storage if it is needed
                    let! hashesToRestorePerInput = 
                        command.Inputs 
                        |> Seq.map(getPathsToRestore index)
                        |> Async.Parallel
                    let hashesToRestore =
                        hashesToRestorePerInput
                        |> Array.collect List.toArray
                        |> Array.map(fun (path, hash) -> hash, path)
                    if Array.length hashesToRestore > 0 then
                        let ct = logLongRunningStart (sprintf "Restoring missing inputs from storage...")
                        let! restoreResult = restoreFromStorage hashesToRestore
                        match restoreResult with
                        |   Ok () -> ()
                        |   Error(er) -> 
                            match er with
                            |   SystemError se -> failwith se
                            |   UserError ue -> raise (StorageRelatedUserError ue)
                        logLongRunningFinish ct (sprintf "Inputs are restored")


                    // 3) executing a command
                    let mutable exitCode = 0
                    let mutable aquiredResources = Set.empty
                    let mutable aquiringResources = Set.empty
                    try
                        // acquiring resource semaphores
                        let resourcesCt = logLongRunningStart "Waiting for system resources quota to become available"
                        for resorceAqcAsync,resName in command.ResourceGroups |> Seq.sort |> Seq.map (fun g -> (Utils.enterResourceGroupMonitorAsync resourceSemaphores g),g) do
                            aquiringResources <- Set.add resName aquiringResources
                            do! resorceAqcAsync // in is crucial to lock the resources sequential and in ordered manner to prevent deadlocks
                            aquiredResources <- Set.add resName aquiredResources
                            logVerbose (sprintf "Got resource: %s" resName)

                        logLongRunningFinish resourcesCt "Obtained needed system resources quota"
                        let print (s:string) = Logger.logInfo Logger.ExecutionOutput s
                        let input idx = command.Inputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot |> applyIndex index
                        let output idx = command.Outputs.[idx-1].Artefact.Id |> idToFullPath experimentRoot |> applyIndex index
                        let context : ComputationContext = { ExperimentRoot = experimentRoot; Print = print; Index=index }
                        let exitCode2 = command |> ExecuteCommand.runCommandLineMethodAndWait context (input, output) 
                        exitCode <- exitCode2
                    finally
                        // releasing resource semaphores
                        let notAquired = Set.difference aquiringResources aquiredResources
                        if not (Set.isEmpty notAquired) then                        
                            logError <| sprintf "Method could not success as failed to obtain the following resources: %A" notAquired
                        aquiredResources |> Seq.iter (fun g -> Utils.exitResourceGroupMonitor (!resourceSemaphores) g)

                    // 4) upon successful exit code hash the outputs
                    if not (Seq.exists (fun x -> exitCode = x) command.SuccessfulExitCodes) then
                        raise(NonSuccessfulExitCodeException(sprintf "Process exited with non-successful exit code %d" exitCode))
                    else
                        logInfo "Method succeeded"
                        logVerbose (sprintf "Program succeeded. Calculating hashes of the outputs...")
                        // 5a) hashing outputs disk content                
                        // 5b) updating dependency versions in dependency graph
                        // 6) dumping updated alph files to disk
                        do! command.OnSucceeded(index)
                |   UpToDate _ ->
                    logInfo "Up to date"
                //comp.Outputs |> Seq.map (fun (output:DependencyGraph.VersionedArtefact) -> output.ExpectedVersion) |> List.ofSeq

                let outPathToArtefactItem (outputPath:string) : Artefact =
                    if outputPath.Contains('*') then
                        let outputItems = PathUtils.enumeratePath outputPath
                        outputItems |> toJaggedArrayOrValue (fun (extraIndex, itemFullPath) -> { FullPath = itemFullPath; Index = index @ extraIndex; UpdateType=Process })
                    else
                        upcast { FullPath = outputPath; Index = index; UpdateType=Process }
                return Seq.singleton(outputPaths |> List.map outPathToArtefactItem, null)
                    
                
        } |> Async.RunSynchronously


let buildGraph experimentRoot (g:DependencyGraph.Graph) checkStoragePresence restoreFromStorage =    
    let resourceSemaphores = ref Map.empty
    let factory method : AngaraGraphNode<ArtefactItem> = 
        match method with
        | DependencyGraph.Source src -> upcast SourceMethod(src, experimentRoot) 
        | DependencyGraph.Command cmd -> upcast CommandMethod(cmd, experimentRoot, checkStoragePresence, restoreFromStorage, resourceSemaphores)

    let flow = g |> DependencyGraphToAngaraWrapper |> AngaraTranslator.translate factory
    flow

let doComputations (g:FlowGraph<AngaraGraphNode<ArtefactItem>>) = 
    let state  = 
        {
            TimeIndex = 0UL
            Graph = g
            Vertices = Map.empty
        }
    try
        use engine = new Engine<AngaraGraphNode<ArtefactItem>>(state,Scheduler.ThreadPool())        
        engine.Start()
        
        // engine.Changes.Subscribe(fun x -> x.State.Vertices)
        let final = Control.pickFinal engine.Changes
        let finalState = final.GetResult()
        Ok()
    with
    | :? Control.FlowFailedException as flowExc ->
        let userErrorExceptionTypes = [
            typedefof<NonSuccessfulExitCodeException>;
            typedefof<ExecuteCommand.MissingExecutableException>;
            typedefof<StorageRelatedUserError>
            ]
        let innerExtracted = 
            flowExc.InnerExceptions
            |> Seq.collect (fun (exc:exn) ->
                userErrorExceptionTypes
                |> List.choose (fun t -> if t.IsInstanceOfType(exc) then Some exc.Message else None ))
        match Seq.tryHead innerExtracted with
        | Some(message) ->
            Error(UserError(message))
        | None ->
            let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
            Error(SystemError(sprintf "Failed to compute the artefacts: \n\t%s" failed))