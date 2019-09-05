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


type ArtefactItem =
    { FullPath: string
      Index: string list
      }

type SourceMethod(source: SourceVertex, experimentRoot,
                    checkStoragePresence : HashString array -> Async<bool array>) = 
    inherit AngaraGraphNode(DependencyGraph.Source source)

    override s.Execute(_, _) = // ignoring checkpoints
        async {
            let expectedArtefact = source.Output
            let artefact = expectedArtefact.Artefact

            // if alph file exists on disk (e.g. isTracked), we need to re-save it to update the expected version
            let alphExists = source.Output.Artefact.Id |> PathUtils.idToAlphFileFullPath source.ExperimentRoot |> File.Exists
            if alphExists then
                do! source.Output.ExpectActualVersionAsync() 
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
                    checkStoragePresence: HashString array -> Async<bool array>,
                    restoreFromStorage: (HashString*string) array -> Async<unit>) = // version*filename
    inherit AngaraGraphNode(DependencyGraph.Command command)  

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

    
    (*
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
            areValidItemsVersions expVersionHashes actVersionsHashes *)

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

            let getExpectedVersion links =
                links |> Seq.map (fun (a:LinkToArtefact) -> a.ExpectedVersion |> resolveIndex index)

            let getActualVersion links =
                async {
                    let! actualVersion =
                        links
                        |> Seq.map (fun (a:LinkToArtefact) -> a.Artefact.ActualVersionAsync)
                        |> Async.Parallel
                    return actualVersion |> Array.map (resolveIndex index)
                }

            let expectedInputItemVersions = getExpectedVersion command.Inputs |> Array.ofSeq
            let! actualInputItemVersions = getActualVersion command.Inputs

            let! areInputsValid = areValidItemsVersions checkStoragePresence expectedInputItemVersions actualInputItemVersions
        
            let! doComputations = 
                async {
                    if not areInputsValid then
                        // we can avoid checking outputs to speed up the work
                        // is the inputs are invalid
                        logVerbose "Needs recomputation due to the outdated inputs"
                        return true 
                    else
                        // checking outputs
                        let expectedOutputItemVersions = getExpectedVersion command.Outputs |> Array.ofSeq
                        let! actualOutputItemVersions = getActualVersion command.Outputs
                        let! areOutputsValid = areValidItemsVersions checkStoragePresence expectedOutputItemVersions actualOutputItemVersions
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
                        let! actualHashOpt = getActualVersion [input]
                        if Option.isSome actualHashOpt.[0] then
                            return None
                        else
                            return Some(input)
                    }
                let! toRestoreOts = Seq.map inputChooser command.Inputs |> Array.ofSeq |> Async.Parallel
                let toRestore = Array.choose id toRestoreOts
                let hashesToRestore = toRestore |> getExpectedVersion |> Seq.map (fun x -> x.Value)
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