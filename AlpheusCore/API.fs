/// High level operations to be called from CLI or GUI
module ItisLab.Alpheus.API

open ItisLab.Alpheus.DependencyGraph
open System.Text
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.Logger
open ItisLab.Alpheus.PathUtils
open ItisLab.Alpheus
open System.IO

let buildDependencyGraphAsync experimentRoot artefactList =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot

        let g = DependencyGraph.Graph()
        let vertexList = List.map g.GetOrAllocateArtefact artefactList
        Logger.logVerbose Logger.API "Building the dependency graph"
        let! _ = g.LoadDependenciesAsync vertexList experimentRoot
        logVerbose LogCategory.API (sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
        // Filling in actual hashes
        do! fillinActualHashesAsync g.Artefacts experimentRoot
        Logger.logInfo Logger.API "Actual hashes are loaded"

        let storages = config.ConfigFile.Storage
        let storagePresenceChecker = StorageFactory.getPresenseChecker experimentRoot (Map.toSeq storages)

        // Filling in with information about which storages hold the versions of the artefacts
        let! _ = Async.Parallel [|fillinArtefactContainingStoragesAsync g.Artefacts storagePresenceChecker; fillinMethodEdgeContainingStoragesAsync g.Methods storagePresenceChecker|]
        Logger.logVerbose Logger.API "Artefact presence in storages checked"
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

/// Returns the experiment root and artefact id for the given arbitrary path.
let artefactFor path : Result<string*ArtefactId, string> = 
    result {
        let! experimentRoot = (Config.tryLocateExperimentRoot path, sprintf "The given path is not under an Alpheus experiment folder: %s" path)
        let artefactId = path |> pathToId experimentRoot
        return (experimentRoot, artefactId)
    }

/// (Re)Computes the artefact specified
let compute (experimentRoot, artefactId) =
    async {
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]
                
        //flow graph to calculate statuses
        let flowGraph = ComputationGraph.buildGraph experimentRoot g
        logVerbose LogCategory.API "Running computations"
        return ComputationGraph.doComputations flowGraph
    } |> Async.RunSynchronously

/// Prints to the stdout the textural statuses of the artefact and its provenance
let status (experimentRoot, artefactId) =
    async {
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]
        let flowGraph = StatusGraph.buildStatusGraph g
        return StatusGraph.printStatuses flowGraph
    } |> Async.RunSynchronously

/// Tries to restore the artefact to the version stored in .alph file using all available storages
let restoreAsync (experimentRoot, artefactId : ArtefactId) =
    async {
        let alphFile = artefactId |> idToAlphFileFullPath experimentRoot
        let! loadResults = AlphFiles.tryLoadAsync alphFile                            
        match loadResults with
        | None ->
            return Error (sprintf "There is no artefact %A on disk to fetch the restore version from" artefactId)
        | Some(alphFile) ->
            let versionToRestore =
                match alphFile.Origin with
                |   SourceOrigin(ver) -> ver.Hash
                |   CommandOrigin(cmd) -> cmd.Outputs.[cmd.OutputIndex].Hash
            let! config = Config.openExperimentDirectoryAsync experimentRoot
            let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
            let! restoreSourcesResults = checker [| Some(versionToRestore) |]
            let restoreSources = restoreSourcesResults.[0]
            if List.length restoreSources = 0 then
                return Error (sprintf "%A:%s is not found in any registered storages" artefactId (versionToRestore.Substring(0,6)))
            else
                let restoreSource = List.head restoreSources
                logVerbose LogCategory.API (sprintf "Restoring %A:%s from %s storage" artefactId (versionToRestore.Substring(0,6)) restoreSource)
                let restore = StorageFactory.getStorageRestore experimentRoot (Map.find restoreSource config.ConfigFile.Storage)
                do! restore artefactId versionToRestore
                return Ok()
    }

/// Saves the supplied artefact to the supplied storage.
/// saveAll means to save all dependencies for the specified artefact
let saveAsync (experimentRoot, artefactId) storageName saveAll =
    let alphFilePath = artefactId |> idToAlphFileFullPath experimentRoot
    let artefactPath = artefactId |> idToFullPath experimentRoot
    async {
        let! alphFile =
            async {
                let! loadResults = AlphFiles.tryLoadAsync alphFilePath                                                                        
                match loadResults with
                | None ->                                
                    // This is a "source" file without .alph file created yet. creating an .alphfile for it                                                                                
                    let! hashResult = Hash.fastHashPathAsync artefactPath
                    match hashResult with
                    | None -> return raise(InvalidDataException("The data to save does not exist"))
                    | Some(version) ->
                        let snapshotSection : AlphFiles.VersionedArtefact =
                            { Hash = version
                              RelativePath = relativePath alphFilePath artefactPath }
                        return 
                            { IsTracked = true
                              Origin = SourceOrigin snapshotSection }
                | Some(alphFile) ->
                    return { alphFile with IsTracked = true }                             
        }                                       
        do! AlphFiles.saveAsync alphFile alphFilePath
                    
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]                                                                            
                    
        let artefactsToSave =
            if saveAll then
                g.Artefacts |> Seq.filter (fun art -> art.IsTracked) |> Array.ofSeq
            else
                [| g.GetOrAllocateArtefact artefactId |]
                                                
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let storageToSaveToOption =                                
            config.ConfigFile.Storage |> Map.toSeq |> Seq.filter (fun pair -> let k,_ = pair in k=storageName) |> Seq.map snd |> Seq.tryHead
          
        match storageToSaveToOption with
        | Some storageToSaveTo ->
            let save = StorageFactory.getStorageSaver experimentRoot storageToSaveTo
            let saveDescriptors = artefactsToSave |> Array.map (fun art -> (idToFullPath experimentRoot artefactId),art.ActualHash.Value)
            let! _ = save saveDescriptors

            // adding the saved artefact into the gitignore
            let newIgnoreEntry (art:ArtefactVertex) = art.Id |> idToExperimentPath |> unixPath
            let newIgnoreEntries = artefactsToSave |> Array.map newIgnoreEntry
            let gitIgnorePath = Path.Combine(experimentRoot,".gitignore")
            do! GitIgnoreManager.addEntriesAsync gitIgnorePath newIgnoreEntries

            return Ok()
        | None ->
            // specified storage is not found
            let storageNames = config.ConfigFile.Storage |> Map.toSeq |> Seq.map fst |> List.ofSeq
            let errMsg = 
                sprintf "The storage name you've specified (\"%s\") does not exist. Please use one of the available storages %A or register a new one." storageName storageNames
            return Error errMsg
    }


/// Adds one more method vertex to the experiment graph
/// deps: a list of paths to the input artefacts. outputs: a list of paths to the produced artefacts
let buildAsync experimentRoot deps outputs command doNotCleanOutputs =
    let getId = pathToId experimentRoot
    let fullInputIDs = List.map getId deps
    let fullOutputIDs = List.map getId outputs
    logVerbose LogCategory.API (sprintf "Dependencies: %A" fullInputIDs)
    logVerbose LogCategory.API (sprintf "Outputs: %A" fullOutputIDs)
    logVerbose LogCategory.API (sprintf "Command: \"%s\"" command)

    command |> MethodCommand.validate (fullInputIDs.Length, fullOutputIDs.Length)

    async {
        let! g = buildDependencyGraphAsync experimentRoot fullInputIDs
        let inputVertices = List.map g.GetOrAllocateArtefact fullInputIDs                     
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
            Logger.logInfo Logger.API "Clearing of outputs by alpheus is disabled for this computation"

        Logger.logVerbose Logger.API (sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
        Logger.logVerbose Logger.API (sprintf "Graph artefacts: %A" g.Artefacts)
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
        let outputAlphPaths = List.map pathToAlphFile outputs
        let! _ = List.map2 updateAlphFileAsync outputAlphPaths outputVertices |> Async.Parallel

        return Ok()
    }