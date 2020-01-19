/// High level operations to be called from CLI or GUI
module ItisLab.Alpheus.API

open ItisLab.Alpheus.DependencyGraph
open System.Text
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.Logger
open ItisLab.Alpheus.PathUtils
open ItisLab.Alpheus
open System.IO
open Angara.Data
open System.Threading.Tasks

let buildDependencyGraphAsync experimentRoot artefactIds =
    async {
        Logger.logVerbose Logger.API "Building the dependency graph"
        let g = DependencyGraph.Graph.Build(experimentRoot, artefactIds)
        logVerbose LogCategory.API (sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)

        // Filling in actual hashes
        // do! g.ReadActualVersions |> doAndLogElapsedTime (Logger.logInfo Logger.API) "Actual hashes are loaded"
        return g
    }

/// Initializes provided directory with default empty Alpheus configuration
let createExperimentDirectory dirPath =
    async {
        let! _ = Config.createExperimentDirectoryAsync dirPath
        let defaultEntries = 
            [
                "**/*.hash" // all of the .hash files are ignored
                ".alpheus/storage" // default local storage
            ]
        do! GitIgnoreManager.addEntriesAsync (Path.Combine(dirPath,".gitignore")) defaultEntries
    }

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

/// Returns the (configured artefact storages * defaultStorage name) for the experiment folder supplied
let configListStoragesAsync expereimentRoot =
    async {
        let! config = Config.openExperimentDirectoryAsync expereimentRoot
        let defaultStorageName = config.ConfigFile.DefaultStorage
        return config.ConfigFile.Storage, defaultStorageName
    }

//
let configStorageSetDefault experimentRoot newDefaultStorageName =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let newDefaultExists = Map.containsKey newDefaultStorageName config.ConfigFile.Storage

        if newDefaultExists then            
            let configFile =
                {
                    config.ConfigFile with
                        DefaultStorage = newDefaultStorageName
                }
            let config = {
                config with
                    ConfigFile = configFile
            }
            do! Config.saveConfigAsync config
            return Ok()
        else
            return Error(UserError(sprintf "Storage name you've specified \"%s\" is not configured. Add storage configuration first and then set is as default" newDefaultStorageName))        
    }

/// Removed the storage by its name from the experiment folder specified
let configRemoveStorageAsync experimentRoot storageToRemove =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
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
let artefactFor workingDir (path:string) : Result<string*ArtefactId, AlpheusError> = 
    result {
        let path2 =
            if Path.IsPathRooted(path) then path
            else
                Path.Combine(workingDir,path)
        let! experimentRoot = (Config.tryLocateExperimentRoot path2, UserError (sprintf "The given path is not under an Alpheus experiment folder: %s" path))
        let artefactId = path2 |> pathToId experimentRoot workingDir
        return (experimentRoot, artefactId)
    }

/// Thread unsafe! guard it with 'singleExecutionGuardAsync' if you expect it is called from several threads
let private restoreSingleItemAsync experimentRoot (path,versionToRestore) =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
        let! restoreSourcesResults = checker [| versionToRestore |]
        let restoreSources = restoreSourcesResults.[0]
        if List.length restoreSources = 0 then
            return Error (UserError(sprintf "%A:%s is not found in any registered storages" path (versionToRestore.Substring(0,6))))
        else
            let restoreSource = List.head restoreSources
            logVerbose LogCategory.API (sprintf "Restoring %A:%s from %s storage" path (versionToRestore.Substring(0,6)) restoreSource)
            let restore = StorageFactory.getStorageRestore experimentRoot (Map.find restoreSource config.ConfigFile.Storage)
            return! restore path versionToRestore
    }    

/// Checks whether the specified versions can be extraced from any available storages
let internal checkStoragePresence experimentRoot (versions:HashString seq) : Async<bool array> =
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
        let nonEmptylist listAsync =
            async {
                let! l = listAsync
                return l |> Array.map (fun x -> not (List.isEmpty x))
            }
        let res = versions |> Array.ofSeq |> (checker >> nonEmptylist)
        return! res
    }

/// (Re)Computes the artefact specified
let compute (experimentRoot, artefactId) =
    async {
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]


        
        let restoreFromStorage (pairs:(HashString*string) array) =
            async {
                let swapedPairs = pairs |> Array.map (fun p -> let x,y = p in y,x ) 
                let restoreTasksCache = ref Map.empty
                let restoreSingleItemTreadSafeAsync toRestore =
                    Utils.singleExecutionGuardAsync restoreTasksCache toRestore (restoreSingleItemAsync experimentRoot)
                let! comp = swapedPairs |> Array.map restoreSingleItemTreadSafeAsync |> Async.Parallel
                let checker res =
                    match res with
                    |   Ok _ -> ()
                    |   Error(e) -> failwith (e.ToString())

                Array.iter checker comp
                return ()
            }

        // flow graph to calculate statuses
        let flowGraph = ComputationGraph.buildGraph experimentRoot g (checkStoragePresence experimentRoot) restoreFromStorage
        logVerbose LogCategory.API "Running computations"
        return ComputationGraph.doComputations flowGraph
    } |> Async.RunSynchronously

/// Returns the status for the requested artefact and its provenance
let status (experimentRoot, artefactId) =
    async {
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]
        let flowGraph = StatusGraph.buildStatusGraph g experimentRoot (checkStoragePresence experimentRoot)
        return StatusGraph.getStatuses flowGraph
    } |> Async.RunSynchronously

let private artefactVersionToPaths fullArtPath (artefactVersion:ArtefactVersion) =
    if artefactVersion.IsScalar then
        Seq.singleton (fullArtPath, artefactVersion.AsScalar())
    else
        let mapper pair =
            let indices,(versionOption:HashString option) = pair
            (PathUtils.applyIndex indices fullArtPath),versionOption
        artefactVersion |> MdMap.toSeq |> Seq.map mapper

/// Tries to restore the artefact to the version stored in .alph file using all available storages
let restoreAsync (experimentRoot, artefactId : ArtefactId) = 
    
    async {
        let alphFile = artefactId |> idToAlphFileFullPath experimentRoot
        let artefactFullPath = idToFullPath experimentRoot artefactId
        let! loadResults = AlphFiles.tryLoadAsync alphFile                            
        match loadResults with
        | None ->
            return Error (UserError (sprintf "There is no alph file on disk for the artefact %A" artefactId))
        | Some(alphFile) ->
            let versionToRestore =
                match alphFile.Origin with
                |   SourceOrigin(ver) -> ver.Hash
                |   CommandOrigin(cmd) -> cmd.Outputs.[cmd.OutputIndex].Hash
            let pathsToRestore =
                let paths = artefactVersionToPaths artefactFullPath versionToRestore
                let chooser pair =
                    let path,verOpt = pair
                    match verOpt with
                    |   Some v -> Some(path,v)
                    |   None -> None
                Seq.choose chooser paths
            let restoreTasksCache = ref Map.empty
            let restoreSingleItemTreadSafeAsync toRestore =
                Utils.singleExecutionGuardAsync restoreTasksCache toRestore (restoreSingleItemAsync experimentRoot)
            let! restoreResults = pathsToRestore |> Seq.map restoreSingleItemTreadSafeAsync |> Async.Parallel

            if restoreResults |> Seq.forall (fun r -> match r with Ok _ -> true | Error _ -> false)
                then return Ok()
            else
                return Error (SystemError(sprintf "%A: some of its items couldn't be restored" artefactId))
    }

/// Saves the supplied artefact to the supplied storage.
/// saveAll means to save all dependencies for the specified artefact
let saveAsync (experimentRoot, artefactId) specifiedStorageName saveAll =    
    let alphFilePath = artefactId |> idToAlphFileFullPath experimentRoot
    let artefactPath = artefactId |> idToFullPath experimentRoot

    let toFullPath = idToFullPath experimentRoot
    
    async {
        let! config = Config.openExperimentDirectoryAsync experimentRoot
        let isStorageRegistered name =
            config.ConfigFile.Storage |> Map.toSeq |> Seq.map fst |> Seq.exists (fun x -> x = name)

        let storageName = 
            match specifiedStorageName with
            |   Some(name) -> name
            |   None ->
                config.ConfigFile.DefaultStorage
                // extracting default storage
        if isStorageRegistered storageName then
            let! alphFile =
                async {
                    let! loadResults = AlphFiles.tryLoadAsync alphFilePath                                                                        
                    match loadResults with
                    | None ->                                
                        // This is a "source" file without .alph file created yet. creating an .alphfile for it                                                                                
                        let! hashResult = Hash.hashVectorPathAndSave artefactPath
                        let snapshotSection : AlphFiles.VersionedArtefact =
                            {   Hash = hashResult
                                RelativePath = relativePath alphFilePath artefactPath }
                        return 
                            {   FileFormatVersion = Versioning.AlphFileCurrentVersion
                                IsTracked = true
                                Origin = SourceOrigin snapshotSection }
                    | Some(alphFile) ->
                        return { alphFile with IsTracked = true }                             
            }                                       
            do! AlphFiles.saveAsync alphFile alphFilePath
                    
            let! g = buildDependencyGraphAsync experimentRoot [artefactId]                                                                            
                    
            let artefactsToSaveResult =
                if saveAll then
                    Ok (g.Artefacts |> Seq.filter (fun art -> art.IsTracked) |> Array.ofSeq)
                else
                    match g.GetArtefact artefactId with
                    |   Ok artefactVertex -> Ok [| artefactVertex |]
                    |   Error e -> Error e
        
            match artefactsToSaveResult with
            |   Ok artefactsToSave ->
                let! config = Config.openExperimentDirectoryAsync experimentRoot
                let storageToSaveToOption =                                
                    config.ConfigFile.Storage |> Map.toSeq |> Seq.filter (fun pair -> let k,_ = pair in k=storageName) |> Seq.map snd |> Seq.tryHead
          
                match storageToSaveToOption with
                | Some storageToSaveTo ->
                    let save = StorageFactory.getStorageSaver experimentRoot storageToSaveTo
                    let! saveDescriptors = 
                        async {
                            let artToSaveDescriptorsAsync (art:ArtefactVertex) =
                                async {
                                    let! pathAndVersionToSave = 
                                        art.Id
                                        |> enumerateItems experimentRoot 
                                        |> MdMap.toSeq 
                                        |> Seq.map(fun (index, path) -> 
                                            async {
                                                let! v = art.ActualVersion.Get index
                                                return path, v
                                            })
                                        |> Async.Parallel
                                    let absentChooser pair =
                                        let path,verOpt = pair
                                        match verOpt with
                                        |   None -> Some(path)
                                        |   Some(_) -> None
                                    let absentPaths = Array.choose absentChooser pathAndVersionToSave
                                    if Array.length absentPaths>0 then 
                                        Logger.logWarning LogCategory.API (sprintf "The following paths are not saved, as they are not on disk: %A" absentPaths)
                                    let presentChooser pair = 
                                        let path,verOpt = pair
                                        match verOpt with
                                        |   None -> None
                                        |   Some(v) -> Some(path,v)
                                    return Array.choose presentChooser pathAndVersionToSave
                                }
                            let! versionedArtefactsToSave = artefactsToSave |> Seq.map artToSaveDescriptorsAsync |> Array.ofSeq |> Async.Parallel
                            return versionedArtefactsToSave |> Seq.concat |> Seq.toArray
                        }
                    
                    
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
                    return Error (UserError errMsg)
            |   Error er -> return Error er
        else
            let isDefault = Option.isNone specifiedStorageName
            let error =
                if isDefault then
                    sprintf "The default storage named \"%s\" is not configured. Please, either add storage configuration \"%s\" or set another default storage" storageName storageName
                else
                    sprintf "The storage you've specified \"%s\" is not configured. Please configure it with \"alpheus config storage setDefault\"" storageName
            return Error(UserError error)
    }


/// Adds one more method vertex to the experiment graph
/// deps: a list of paths to the input artefacts. outputs: a list of paths to the produced artefacts
/// workingDirecotry - the full path to the dir, relative to which the command and the inputs/outputs are considered
let buildAsync experimentRoot workingDir deps outputs command executionSettings =
    let getId = pathToId experimentRoot workingDir // WARNING: this method depends on the current directory!
    let inputIDs = List.map getId deps
    let outputIDs = List.map getId outputs
    logVerbose LogCategory.API (sprintf "Dependencies: %A" inputIDs)
    logVerbose LogCategory.API (sprintf "Outputs: %A" outputIDs)
    logVerbose LogCategory.API (sprintf "Command: \"%s\"" command)

    command |> MethodCommand.validate (inputIDs.Length, outputIDs.Length)

    async {
        let! g = buildDependencyGraphAsync experimentRoot inputIDs
        
        // saving command and current working dir
        let rootBasedCwd = Path.GetRelativePath(experimentRoot, workingDir) + Path.DirectorySeparatorChar.ToString()       

        let! _ = g.AddMethod command inputIDs outputIDs rootBasedCwd executionSettings
    
        if executionSettings.DoNotCleanOutputs then
            Logger.logVerbose Logger.API "Clearing of outputs by alpheus is disabled for this computation"
        Logger.logVerbose Logger.API (sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
        Logger.logVerbose Logger.API (sprintf "Graph artefacts: %A" g.Artefacts)
        
        return Ok()
    }