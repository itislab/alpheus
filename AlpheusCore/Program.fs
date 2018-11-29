module ItisLab.Alpheus.Program

open System
open Argu
open ItisLab.Alpheus.CLI
open System.IO
open System.Diagnostics
open ItisLab.Alpheus.DependencyGraph
open System.Net.Http.Headers
open System
open System.ComponentModel
open System.Text
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus

/// being in currentDir, returns the relative path to targetPath
let getRelativePath currentDir targetPath =
    if not(Directory.Exists currentDir) then
        raise(ArgumentException(sprintf "The \"%s\" directory does not exist" currentDir))
    else
        Path.GetRelativePath(currentDir,targetPath)
    


[<EntryPoint>]
let main argv =
    //dealing with tracing
    //let ts = TraceSource("CLI")
    let traceVerbose str =
        //ts.TraceEvent(TraceEventType.Verbose,0,str)
        printfn "%s" str

    let traceInfo str =
        printfn "%s" str

    //let sourceSwitch = new SourceSwitch("SourceSwitch", "Verbose")
    //ts.Switch <- sourceSwitch
    //ts.Listeners.Add(Diagnostics.DefaultTraceListener()) |> ignore

    let programName = "alpheus"
    let parser = ArgumentParser.Create<AlpheusArgs>(programName = programName)
    // probing environment
    // creating corresponding realizations
    

    try 
        let buildDependencyGraphAsync experimentRoot artefactFullID=
            async {
                let! config = Config.openExperimentDirectoryAsync experimentRoot

                let g = DependencyGraph.Graph()
                let vertex = g.GetOrAllocateArtefact artefactFullID
                let! _ = g.LoadDependenciesAsync [vertex] experimentRoot
                traceVerbose(sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
                // Filling in actual hashes
                do! fillinActualHashesAsync g.Artefacts experimentRoot
                traceVerbose("Actual hashes are loaded")

                let storages = config.ConfigFile.Storage
                let storagePresenceChecker = StorageFactory.getPresenseChecker experimentRoot (Map.toSeq storages)

                // Filling in with information about which storages hold the versions of the artefacts
                let! _ = Async.Parallel [|fillinArtefactContainingStoragesAsync g.Artefacts storagePresenceChecker; fillinMethodEdgeContainingStoragesAsync g.Methods storagePresenceChecker|]
                traceVerbose("Artefact presence in storages checked")
                return g
                }

        let usage = parser.PrintUsage(programName = programName)
        let parseResults = parser.ParseCommandLine(argv,false,true,true)
        if parseResults.Contains Init then            
            let cwd = Directory.GetCurrentDirectory()
            if Config.isExperimentDirectory cwd then
                printfn "The current directory is already Alpheus experiment directory"
                1
            else
                let initComputaiton = Config.createExperimentDirectoryAsync cwd
                Async.RunSynchronously initComputaiton |> ignore
                0
        elif parseResults.Contains Config then
            let configArgs = parseResults.GetResult <@ Config @>
            let cwd = Directory.GetCurrentDirectory()
            match Config.tryLocateExpereimentRoot cwd with
                |   None ->
                    printfn "The file you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->
                    let configComputation = 
                        async {
                                let! config = Config.openExperimentDirectoryAsync experimentRoot
                                if configArgs.Contains CLI.Storage then
                                    let storageArgs = configArgs.GetResult <@ CLI.Storage @>
                                    if storageArgs.Contains AddLocal then
                                        let name,dirPath = storageArgs.GetResult <@ AddLocal @>                                        
                                        let localStorage = Config.Storage.Directory(dirPath)                                            
                                        let configFile =
                                            {
                                                config.ConfigFile with
                                                    Storage = Map.add name localStorage config.ConfigFile.Storage
                                            }
                                        let config = {
                                            config with
                                                ConfigFile = configFile
                                        }
                                        do! Config.saveConfigAsync config
                                        return 0
                                    elif storageArgs.Contains AddAzure then
                                        let name,accName,accKey,container = storageArgs.GetResult <@ AddAzure @>                                        
                                        let azureStorage = Config.Storage.Azure(
                                            {
                                                AccountName= accName
                                                AccountKey = accKey
                                                ContainerName = container
                                            }
                                        )
                                        let configFile =
                                            {
                                                config.ConfigFile with
                                                    Storage = Map.add name azureStorage config.ConfigFile.Storage
                                            }
                                        let config = {
                                            config with
                                                ConfigFile = configFile
                                        }
                                        do! Config.saveConfigAsync config
                                        return 0
                                    elif storageArgs.Contains List then
                                        let sb = StringBuilder()
                                        let printer name storage =
                                            let storageStr = 
                                                match storage with
                                                |   Config.Directory(path) -> sprintf "Local directory \"%s\"" path
                                                |   Config.Azure(def) -> sprintf "Azure BLOB accountName:%s container:%s" def.AccountName def.ContainerName
                                            sb.AppendLine(sprintf "%20s\t\t%s" name storageStr) |> ignore
                                        Map.iter printer config.ConfigFile.Storage
                                        printfn "You have following storages configured:\n%s" (sb.ToString())
                                        return 0
                                    elif storageArgs.Contains Remove then
                                        let nametoRemove = storageArgs.GetResult <@ Remove @>
                                        let configFile =
                                            {
                                                config.ConfigFile with
                                                    Storage = Map.remove nametoRemove config.ConfigFile.Storage
                                            }
                                        let config = {
                                            config with
                                                ConfigFile = configFile
                                        }
                                        do! Config.saveConfigAsync config
                                        return 0
                                    else
                                        printfn "Please specify what to do with storage configuration"
                                        return 1
                                else
                                    printfn "Please specify what to configure"
                                    return 1
                        }
                    configComputation |> Async.RunSynchronously
        elif parseResults.Contains Compute then
            let computeArgs = parseResults.GetResult <@ Compute @>
            let filePath = computeArgs.GetResult <@ ComputeArgs.File @>
            let alphFilePath =
                if filePath.EndsWith(".alph") then
                    filePath
                else
                    sprintf "%s.alph" filePath
            if not (File.Exists(alphFilePath)) then
               printfn "Can't find .alph file \"%s\"" alphFilePath
               1
            else
                match Config.tryLocateExpereimentRoot alphFilePath with
                |   None ->
                    printfn "The file you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->
                    // removing .alph extension if it is present
                    let alphFilePath = if alphFilePath.EndsWith(".alph") then alphFilePath.Substring(0,alphFilePath.Length-5) else alphFilePath                               

                    let fullID = Path.GetRelativePath(experimentRoot, alphFilePath)
                    let statusAsync = async {
                        let! g = buildDependencyGraphAsync experimentRoot fullID
                        
                        //flow graph to calculate statuses
                        let flowGraph = ComputationGraph.buildStatusGraph experimentRoot g
                        traceVerbose("Running computations")
                        return ComputationGraph.doComputations flowGraph
                    }
                    Async.RunSynchronously statusAsync  
        elif parseResults.Contains Status then
            let statusArgs = parseResults.GetResult <@ Status @>
            let filepath = statusArgs.GetResult <@ StatusArgs.File  @>
            let alphFilePath = if filepath.EndsWith(".alph") then filepath else (sprintf "%s.alph" filepath)
            if not (File.Exists(alphFilePath)) then
               printfn "The alph file %s does not exist" alphFilePath
               1
            else
                match Config.tryLocateExpereimentRoot alphFilePath with
                |   None ->
                    printfn "The file you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->
                    // removing .alph extension if it is present
                    let alphFilePath = if alphFilePath.EndsWith(".alph") then alphFilePath.Substring(0,alphFilePath.Length-5) else alphFilePath
            
                    let fullID = Path.GetRelativePath(experimentRoot, alphFilePath)
                    let statusAsync=
                        async {
                        let! g = buildDependencyGraphAsync experimentRoot fullID
                
                        //flow graph to calculate statuses
                        let flowGraph = StatusGraph.buildStatusGraph g
                
                        return StatusGraph.printStatuses flowGraph
                        }
                    Async.RunSynchronously statusAsync
        elif parseResults.Contains Restore then
            let restoreArgs = parseResults.GetResult <@ Restore @>
            let inputpath = restoreArgs.GetResult <@ RestoreArgs.Path @>
            let alphFilePath,filePath =
                if inputpath.EndsWith(".alph") then
                    inputpath, (inputpath.Substring(0,inputpath.Length-5))
                else
                    (sprintf "%s.alph" inputpath),inputpath
            match Config.tryLocateExpereimentRoot alphFilePath with
                |   None ->
                    printfn "The file/dir you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->     
                    let restoreComputation = 
                        async {
                            let! loadResults = AlphFiles.tryLoadAsync alphFilePath                            
                            match loadResults with
                            |   None ->
                                printfn "There is no %s file on disk to fetch the restore version from" alphFilePath
                                return 2
                            |   Some(alphFile) ->
                                let fullID = Path.GetRelativePath(experimentRoot, filePath)
                                let absFilePath = Path.GetFullPath(filePath)
                                let alphFileDir = Path.GetDirectoryName(absFilePath)+Path.DirectorySeparatorChar.ToString()                                
                                let versionToRestore =
                                    match alphFile.Origin with
                                    |   Snapshot(ver) -> ver
                                    |   Computed(comp) ->
                                        let idToFullID id =
                                            Path.GetRelativePath(experimentRoot,Path.Combine(alphFileDir,id))
                                        (comp.Outputs |> Seq.find (fun o -> (idToFullID o.ID) = fullID)).Hash                                
                                let! config = Config.openExperimentDirectoryAsync experimentRoot
                                let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
                                let! restoreSourcesResults = checker [| versionToRestore |]
                                let restoreSources = restoreSourcesResults.[0]
                                if List.length restoreSources = 0 then
                                    printfn "%s:%s is not found in any registered storages" fullID (versionToRestore.Substring(0,6))
                                    return 2
                                else
                                    let restoreSource = List.head restoreSources
                                    traceVerbose (sprintf "Restoring %s:%s from %s storage" fullID (versionToRestore.Substring(0,6)) restoreSource)
                                    let restore = StorageFactory.getStorageRestore experimentRoot (Map.find restoreSource config.ConfigFile.Storage)
                                    do! restore fullID versionToRestore
                                    return 0
                        }
                    Async.RunSynchronously restoreComputation

        elif parseResults.Contains Save then
            let saveArgs = parseResults.GetResult <@ Save @>
            let saveAll = saveArgs.Contains AllUnsaved 
            let storageName = saveArgs.GetResult <@ SaveArgs.Storage @>
            
            let inputpath = saveArgs.GetResult <@ SaveArgs.Path @>
            let alphFilePath,filePath =
                if inputpath.EndsWith(".alph") then
                    inputpath, (inputpath.Substring(0,inputpath.Length-5))
                else
                    (sprintf "%s.alph" inputpath),inputpath

            match Config.tryLocateExpereimentRoot alphFilePath with
                |   None ->
                    printfn "The file/dir you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->     
                    let saveComputation = 
                        async {                            
                            let! alphFile =
                                async {
                                    let! loadResults = AlphFiles.tryLoadAsync alphFilePath                                                                        
                                    match loadResults with
                                    |   None ->                                
                                        // This is "source" file without .alph file created yet. creating an .alphfile for it                                        
                                        let! hashResult = Hash.fastHashDataAsync filePath
                                        match hashResult with
                                        |   None -> return raise(InvalidDataException("The data to save does not exist"))
                                        |   Some(version) ->
                                            return {
                                                IsTracked = true
                                                Origin = Snapshot version
                                            }
                                    |   Some(alphFile) ->
                                        return {
                                                alphFile with
                                                IsTracked = true
                                        }                                    
                                }                                       
                            do! AlphFiles.saveAsync alphFile alphFilePath
                            
                            let fullID = Path.GetRelativePath(experimentRoot, filePath)
                    
                            let! g = buildDependencyGraphAsync experimentRoot fullID                                                                                    
                            
                            let artefactsToSave =
                                if saveAll then
                                    g.Artefacts |> Seq.filter (fun art -> art.IsTracked) |> Array.ofSeq
                                else
                                    [| g.GetOrAllocateArtefact fullID |]
                                                        
                            let! config = Config.openExperimentDirectoryAsync experimentRoot
                            let storageToSaveTo =                                
                                config.ConfigFile.Storage |> Map.toSeq |> Seq.filter (fun pair -> let k,_ = pair in k=storageName) |> Seq.map snd |> Seq.head
                            
                            let save = StorageFactory.getStorageSaver experimentRoot storageToSaveTo
                            let saveDescriptors = artefactsToSave |> Array.map (fun art -> Path.Combine(experimentRoot,art.FullID),art.ActualHash.Value)
                            let! _ = save saveDescriptors
                            return 0
                        }
                    saveComputation |> Async.RunSynchronously
        elif parseResults.Contains Build then                
            let buildArgs = parseResults.GetResult <@ Build @>
            let deps = buildArgs.GetResults <@ D @>
            traceVerbose(sprintf "Dependencies: %A" deps)
            let outputs = buildArgs.GetResults <@ O @>
            traceVerbose(sprintf "Outputs: %A" outputs)
            let doNotCleanOutputs = 
                if buildArgs.Contains Disable_Outputs_Clean then true else false
            if List.length outputs = 0 then
                raise(ArgumentException("You need to specify at least one output"))
            let unrecognized = buildArgs.UnrecognizedCliParams
            
            if List.length unrecognized = 0 then
                raise(ArgumentException("You need to specify command"))
            else
                let command = String.Join(' ',unrecognized) // buildArgs.TryGetResult <@ Command @>
                // Checking that both dependencies and outputs are under the same experiment folder
                let allPathParams = List.append deps outputs
                let roots = List.map Config.tryLocateExpereimentRoot allPathParams |> List.distinct
                traceVerbose(sprintf "found experiment roots among supplied artefact paths: %A" roots)
                if List.length roots > 1 then
                    raise(ArgumentException("Not all of the artefacts (inputs, outputs) are under the same experiment root folder"))            

                // generating leafs full alpheus path
                let experimentRoot = (List.exactlyOne roots).Value
                let fullInputIDs = List.map (fun x -> getRelativePath experimentRoot x) deps
                let fullOutputIDs = List.map (fun x -> getRelativePath experimentRoot x) outputs
                traceVerbose(sprintf "full dependency IDs: %A" fullInputIDs)
                traceVerbose(sprintf "full output IDs: %A" fullOutputIDs)
                traceVerbose(sprintf "Command is \"%s\"" command)
                let computeAsync = async {
                    let g = DependencyGraph.Graph()
                    let inputVertices = List.map g.GetOrAllocateArtefact fullInputIDs
                    let! graphArtefacts = g.LoadDependenciesAsync inputVertices experimentRoot
                    // Filling in actual hashes
                    do! fillinActualHashesAsync g.Artefacts experimentRoot
                    traceVerbose("Actual hashes are loaded")                
                    let inputVersionedVertices = List.map getVersionedArtefact inputVertices
                    let outputVertices = List.map g.GetOrAllocateArtefact fullOutputIDs
                    let outputVersionedVertices = List.map getVersionedArtefact outputVertices
                    // traceVerbose(sprintf "Graph artefacts: %A" graphArtefacts)
                    // adding method vertex                
                    let methodVertex = g.AddMethod inputVersionedVertices outputVersionedVertices
                
                    // saving command and current working dir
                    let cwd = Directory.GetCurrentDirectory()
                    let rootBasedCwd = Path.GetRelativePath(experimentRoot, cwd)
                    methodVertex.WorkingDirectory <- rootBasedCwd
                    methodVertex.Command <- command

                    methodVertex.DoNotCleanOutputs <- doNotCleanOutputs
                    if doNotCleanOutputs then
                        printfn "Clearing of outputs by alpheus is disabled for this computation"

                    traceVerbose(sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
                    traceVerbose(sprintf "Graph artefacts: %A" g.Artefacts)
                    // dumping outputs as .alph files
                    // all outputs share the same compute section                    
                    let updateAlphFileAsync alphFilePath artefact =
                        async {
                            let alphFilePath =
                                if Seq.exists (fun c -> (c=Path.DirectorySeparatorChar) || (c=Path.AltDirectorySeparatorChar)) alphFilePath then
                                    alphFilePath
                                else
                                    Path.Combine(".",alphFilePath)
                            let alphFileDir = Path.GetFullPath(Path.GetDirectoryName(alphFilePath))
                            let alphFile = DependencyGraph.artefactToAlphFile artefact alphFileDir experimentRoot                                                
                            do! AlphFiles.saveAsync alphFile alphFilePath
                        }
                    let outputAlphPaths = List.map (fun x -> sprintf "%s.alph" x) outputs
                    let! dummy = List.map2 updateAlphFileAsync outputAlphPaths outputVertices |> Async.Parallel

                    return ()
                    }
                Async.RunSynchronously(computeAsync)
                0
        else
            printfn "%s" usage
            2
    with e ->
        printfn "%s" (e.ToString())
        0
