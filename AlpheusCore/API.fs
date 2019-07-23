/// High level operations to be called from CLI or GUI
module ItisLab.Alpheus.API

open ItisLab.Alpheus.DependencyGraph
open System.Text
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus
open System.IO

let traceVerbose str =
       //ts.TraceEvent(TraceEventType.Verbose,0,str)
       printfn "%s" str

let traceInfo str =
       printfn "%s" str

//dealing with tracing
//let ts = TraceSource("CLI")
   

//let sourceSwitch = new SourceSwitch("SourceSwitch", "Verbose")
//ts.Switch <- sourceSwitch
//ts.Listeners.Add(Diagnostics.DefaultTraceListener()) |> ignore


let buildDependencyGraphAsync experimentRoot artefactFullID =
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

/// Initializes provided directory with default empty alpheus configuration
let createExperimentDirectoryAsync dirPath = Config.createExperimentDirectoryAsync dirPath

/// Adds one more directory based artefact storage to the specified experiment folder
let configAddDirectoryStorageAsync experimentRoot storageName dirPath =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let localStorage = Config.Storage.Directory(dirPath)                                            
        let configFile =
            {
                config.ConfigFile with
                    Storage = Map.add storageName localStorage config.ConfigFile.Storage
            }
        let config = {
            config with
                ConfigFile = configFile
        }
        do! Config.saveConfigAsync config
    }

/// Adds one more azure BLOB container based artefact storage to the specified experiment folder
let configAddAzureStorageAsync experimentRoot storageName accName accKey containerName =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let azureStorage =
            Config.Storage.Azure(
                {
                    AccountName= accName
                    AccountKey = accKey
                    ContainerName = containerName
                }
        )
        let configFile =
            {
                config.ConfigFile with
                    Storage = Map.add storageName azureStorage config.ConfigFile.Storage
            }
        let config = {
            config with
                ConfigFile = configFile
        }
        do! Config.saveConfigAsync config
    }

/// Returns the configured artefact storages for the experiment folder supplied
let configListStoragesAsync expereimentRoot =
    async {
        let! config = Config.openExperimentDirectoryAsync expereimentRoot
        return config.ConfigFile.Storage
    }

/// Removed the storage by its name from the experiment folder specified
let configRemoveStorageAsync expereimentRoot storageToRemove =
    async {
        let! config = Config.openExperimentDirectoryAsync expereimentRoot
        let configFile =
            {
                config.ConfigFile with
                    Storage = Map.remove storageToRemove config.ConfigFile.Storage
            }
        let config = {
            config with
                ConfigFile = configFile
        }
        do! Config.saveConfigAsync config
    }

/// (Re)Computes the artefact specified
let compute (artefactPath:string) =
    let alphFilePath =
        if artefactPath.EndsWith(".alph") then
            artefactPath
        else
            artefactPathToAlphFilePath artefactPath
    if not (File.Exists(alphFilePath)) then
       Error(sprintf "Can't find .alph file \"%s\"" alphFilePath)
    else
        match Config.tryLocateExperimentRoot alphFilePath with
        |   None ->
            Error("The file you've specified is not under Alpheus experiment folder")
        |   Some(experimentRoot) ->                    
            let statusAsync = async {
                let! artefactPath = alphFilePathToArtefactPathAsync alphFilePath

                let fullID = ArtefactId.ID(Path.GetRelativePath(experimentRoot, artefactPath))
            
                let! g = buildDependencyGraphAsync experimentRoot fullID
                
                //flow graph to calculate statuses
                let flowGraph = ComputationGraph.buildGraph experimentRoot g
                traceVerbose("Running computations")
                return ComputationGraph.doComputations flowGraph
            }
            Async.RunSynchronously statusAsync 

/// Prints to the stdout the textural statuses of the artefact and its provenance
let status artefactPath =
    let alphFilePath = artefactPathToAlphFilePath artefactPath
    
    if not (File.Exists(alphFilePath)) then
       Error(sprintf "The alph file %s does not exist" alphFilePath)
    else
        match Config.tryLocateExperimentRoot alphFilePath with
        |   None ->
            Error("The file you've specified is not under Alpheus experiment folder")
        |   Some(experimentRoot) ->                    
            async {
                let! artefactPath = alphFilePathToArtefactPathAsync alphFilePath
            
                let fullID = ArtefactId.ID(Path.GetRelativePath(experimentRoot, artefactPath))
            
                let! g = buildDependencyGraphAsync experimentRoot fullID
        
                //flow graph to calculate statuses
                let flowGraph = StatusGraph.buildStatusGraph g
        
                return StatusGraph.printStatuses flowGraph
            } |> Async.RunSynchronously

/// Tries to restore the artefact to the version stored in .alph file using all available storages
let restoreAsync (artefactPath:string) =
    async {
        let alphFilePath =
            if artefactPath.EndsWith(".alph") then
                artefactPath
            else
                artefactPathToAlphFilePath artefactPath
        match Config.tryLocateExperimentRoot alphFilePath with
            |   None ->
                return Error("The file/dir you've specified is not under Alpheus experiment folder")
            |   Some(experimentRoot) ->     
                let! loadResults = AlphFiles.tryLoadAsync alphFilePath                            
                match loadResults with
                |   None ->
                    return Error(sprintf "There is no %s file on disk to fetch the restore version from" alphFilePath)
                |   Some(alphFile) ->
                    let alphFileFullPath = Path.GetFullPath(alphFilePath)
                    let! artefactPath =  alphFilePathToArtefactPathAsync alphFilePath
                    let fullID = AlphFiles.ArtefactId.ID(Path.GetRelativePath(experimentRoot, artefactPath))
                    let absFilePath = Path.GetFullPath(artefactPath)                                
                    let versionToRestore =
                        match alphFile.Origin with
                        |   Snapshot(ver) -> ver.Version
                        |   Computed(comp) ->
                            let idToFullID =
                                AlphFiles.relIDtoFullID experimentRoot alphFileFullPath                                            
                            (comp.Outputs |> Seq.find (fun o -> (idToFullID o.ID) = fullID)).Hash                                
                    let! config = Config.openExperimentDirectoryAsync experimentRoot
                    let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
                    let! restoreSourcesResults = checker [| Some(versionToRestore) |]
                    let restoreSources = restoreSourcesResults.[0]
                    if List.length restoreSources = 0 then
                        return Error(sprintf "%A:%s is not found in any registered storages" fullID (versionToRestore.Substring(0,6)))
                    else
                        let restoreSource = List.head restoreSources
                        traceVerbose (sprintf "Restoring %A:%s from %s storage" fullID (versionToRestore.Substring(0,6)) restoreSource)
                        let restore = StorageFactory.getStorageRestore experimentRoot (Map.find restoreSource config.ConfigFile.Storage)
                        do! restore fullID versionToRestore
                        return Ok()
        }

/// Saves the supplied artefact to the supplied storage.
/// saveAll means to save all dependencies for the specified artefact
let saveAsync (artefactPath:string) storageName saveAll =
    async {
    let alphFilePath =
        if artefactPath.EndsWith(".alph") then
            artefactPath
        else
            artefactPathToAlphFilePath artefactPath

    match Config.tryLocateExperimentRoot alphFilePath with
        |   None ->
            return Error("The file/dir you've specified is not under Alpheus experiment folder")
        |   Some(experimentRoot) ->                           
            let! alphFile,artefactPath =
                async {
                    let! loadResults = AlphFiles.tryLoadAsync alphFilePath                                                                        
                    let! artefactPath =
                        async {
                            if artefactPath.EndsWith(".alph") then
                                // extracting from alph file
                                let! result = alphFilePathToArtefactPathAsync artefactPath
                                return result
                            else
                                return artefactPath
                        }
                    match loadResults with
                    |   None ->                                
                        // This is "source" file without .alph file created yet. creating an .alphfile for it                                                                                
                        let! hashResult = Hash.fastHashPathAsync artefactPath
                        match hashResult with
                        |   None -> return raise(InvalidDataException("The data to save does not exist"))
                        |   Some(version) ->
                            let snapshotSection =
                                {
                                    Version = version
                                    Type = if artefactPath.EndsWith(Path.DirectorySeparatorChar) then DirectoryArtefact else FileArtefact
                                }
                            return {
                                IsTracked = true
                                Origin = Snapshot snapshotSection
                            },artefactPath
                    |   Some(alphFile) ->
                        return {
                                alphFile with
                                    IsTracked = true
                        },artefactPath                             
                }                                       
            do! AlphFiles.saveAsync alphFile alphFilePath
                    
            let fullID = ArtefactId.ID(Path.GetRelativePath(experimentRoot, artefactPath))
            
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
            let saveDescriptors = artefactsToSave |> Array.map (fun art -> (fullIDtoFullPath experimentRoot art.Id),art.ActualHash.Value)
            let! _ = save saveDescriptors
            return Ok()
    }

/// Adds one more method vertex to the experiment graph
/// deps: a list of paths to the input artefacts. outputs: a list of paths to the produced artefacts
let buildAsync experimentRoot deps outputs command doNotCleanOutputs =
    let getId = ArtefactId.Create experimentRoot
    let fullInputIDs = List.map getId deps
    let fullOutputIDs = List.map getId outputs
    traceVerbose(sprintf "Dependencies: %A" fullInputIDs)
    traceVerbose(sprintf "Outputs: %A" fullOutputIDs)
    traceVerbose(sprintf "Command: \"%s\"" command)

    command |> MethodCommand.validate (fullInputIDs.Length, fullOutputIDs.Length)

    async {
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
        let rootBasedCwd = Path.GetRelativePath(experimentRoot, cwd) + Path.DirectorySeparatorChar.ToString()
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
                let alphFileFullPath = Path.GetFullPath(alphFilePath)
                let alphFile = DependencyGraph.artefactToAlphFile artefact alphFileFullPath experimentRoot                                                
                do! AlphFiles.saveAsync alphFile alphFilePath
            }
        let outputAlphPaths = List.map (fun x -> artefactPathToAlphFilePath x) outputs
        let! dummy = List.map2 updateAlphFileAsync outputAlphPaths outputVertices |> Async.Parallel

        return Ok()
    }