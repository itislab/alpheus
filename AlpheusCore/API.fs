/// High level operations to be called from CLI or GUI
module ItisLab.Alpheus.API

open ItisLab.Alpheus.DependencyGraph
open System.Text
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.Logger
open ItisLab.Alpheus.PathUtils
open ItisLab.Alpheus.AsyncUtils
open ItisLab.Alpheus
open System.IO
open Angara.Data

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
let artefactFor path : Result<string*ArtefactId, AlpheusError> = 
    result {
        let! experimentRoot = (Config.tryLocateExperimentRoot path, UserError (sprintf "The given path is not under an Alpheus experiment folder: %s" path))
        let artefactId = path |> pathToId experimentRoot
        return (experimentRoot, artefactId)
    }

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

/// (Re)Computes the artefact specified
let compute (experimentRoot, artefactId) =
    async {
        let! g = buildDependencyGraphAsync experimentRoot [artefactId]

        let checkStoragePresence (versions:HashString array) : Async<bool array> =
            async {
                let! config = Config.openExperimentDirectoryAsync experimentRoot
                let checker = config.ConfigFile.Storage |> Map.toSeq |> StorageFactory.getPresenseChecker experimentRoot
                let nonEmptylist listAsync =
                    async {
                        let! l = listAsync
                        return l |> Array.map (fun x -> not (List.isEmpty x))
                    }
                let res = versions |> (checker >> nonEmptylist)
                return! res
            }

        let restoreFromStorage (pairs:(HashString*string) array) =
            async {
                let swapedPairs = pairs |> Array.map (fun p -> let x,y = p in y,x ) 
                let! comp = swapedPairs |> Array.map (restoreSingleItemAsync experimentRoot) |> Async.Parallel
                let checker res =
                    match res with
                    |   Ok _ -> ()
                    |   Error(e) -> failwith (e.ToString())

                Array.iter checker comp
                return ()
            }

        //flow graph to calculate statuses
        let flowGraph = ComputationGraph.buildGraph experimentRoot g checkStoragePresence restoreFromStorage
        logVerbose LogCategory.API "Running computations"
        return ComputationGraph.doComputations flowGraph
    } |> Async.RunSynchronously

/// Prints to the stdout the textural statuses of the artefact and its provenance
let status (experimentRoot, artefactId) =
    failwith "Not implemented" |> ignore
    //async {
    //    let! g = buildDependencyGraphAsync experimentRoot [artefactId]
    //    let flowGraph = StatusGraph.buildStatusGraph g
    //    return StatusGraph.printStatuses flowGraph
    //} |> Async.RunSynchronously

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
            let! restoreResults = pathsToRestore |> Seq.map (restoreSingleItemAsync experimentRoot) |> Array.ofSeq |> Async.Parallel

            if restoreResults |> Seq.forall (fun r -> match r with Ok _ -> true | Error _ -> false)
                then return Ok()
            else
                return Error (SystemError(sprintf "%A: some of its items couldn't be restored" artefactId))
    }

/// Saves the supplied artefact to the supplied storage.
/// saveAll means to save all dependencies for the specified artefact
let saveAsync (experimentRoot, artefactId) storageName saveAll =    
    let alphFilePath = artefactId |> idToAlphFileFullPath experimentRoot
    let artefactPath = artefactId |> idToFullPath experimentRoot

    let toFullPath = idToFullPath experimentRoot
    
    async {
        let! alphFile =
            async {
                let! loadResults = AlphFiles.tryLoadAsync alphFilePath                                                                        
                match loadResults with
                | None ->                                
                    // This is a "source" file without .alph file created yet. creating an .alphfile for it                                                                                
                    let! hashResult = Hash.hashVectorPathAndSave artefactPath
                    let snapshotSection : AlphFiles.VersionedArtefact =
                        { Hash = hashResult
                          RelativePath = relativePath alphFilePath artefactPath }
                    return 
                        { IsTracked = true
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
                                let artFullPath = toFullPath art.Id
                                let! artVersion = art.ActualVersionAsync
                                let pathsToSave = artefactVersionToPaths artFullPath artVersion |> Array.ofSeq
                                let absentChooser pair =
                                    let path,verOpt = pair
                                    match verOpt with
                                    |   None -> Some(path)
                                    |   Some(_) -> None
                                let absentPaths = Array.choose absentChooser pathsToSave
                                if Array.length absentPaths>0 then 
                                    Logger.logWarning LogCategory.API (sprintf "The following paths are not saved, as they are not on disk: %A" absentPaths)
                                let presentChooser pair = 
                                    let path,verOpt = pair
                                    match verOpt with
                                    |   None -> None
                                    |   Some(v) -> Some(path,v)
                                return Array.choose presentChooser pathsToSave
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
    }


/// Adds one more method vertex to the experiment graph
/// deps: a list of paths to the input artefacts. outputs: a list of paths to the produced artefacts
let buildAsync experimentRoot deps outputs command doNotCleanOutputs =
    let getId = pathToId experimentRoot
    let inputIDs = List.map getId deps
    let outputIDs = List.map getId outputs
    logVerbose LogCategory.API (sprintf "Dependencies: %A" inputIDs)
    logVerbose LogCategory.API (sprintf "Outputs: %A" outputIDs)
    logVerbose LogCategory.API (sprintf "Command: \"%s\"" command)

    command |> MethodCommand.validate (inputIDs.Length, outputIDs.Length)

    async {
        let! g = buildDependencyGraphAsync experimentRoot inputIDs
        let! methodVertex = g.AddMethod command inputIDs outputIDs
    
        if doNotCleanOutputs then
            Logger.logInfo Logger.API "Clearing of outputs by alpheus is disabled for this computation"
        Logger.logVerbose Logger.API (sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
        Logger.logVerbose Logger.API (sprintf "Graph artefacts: %A" g.Artefacts)


        // saving command and current working dir
        let cwd = Directory.GetCurrentDirectory()
        let rootBasedCwd = Path.GetRelativePath(experimentRoot, cwd) + Path.DirectorySeparatorChar.ToString()
        methodVertex.WorkingDirectory <- rootBasedCwd
        methodVertex.DoNotCleanOutputs <- doNotCleanOutputs

        // saving .alph files for the outputs
        let updateArtefactAlph artefactId =
            result {
                let! artefact = g.GetArtefact artefactId
                artefact.SaveAlphFile()
                return ()
            }

        let allOk results =
            let errorChooser item =
                match item with
                |   Ok _ -> None
                |   Error e -> Some(e)
            let firstError = results |> Seq.choose errorChooser |> Seq.tryHead
            match firstError with
            |   None -> Ok()
            |   Some(e) -> Error e

            
        let updateResults = 
            outputIDs
            |> Seq.map updateArtefactAlph |> Array.ofSeq
            

        return allOk updateResults
    }