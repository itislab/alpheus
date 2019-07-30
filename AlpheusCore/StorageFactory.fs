module ItisLab.Alpheus.StorageFactory

open ItisLab.Alpheus.Config
open System.IO
open ItisLab.Alpheus.Storage
open System
open System.Threading
open ItisLab.Alpheus.PathUtils
open Logger

let createStorage (projectRoot:string) storageDef =
    match storageDef with
        |   Storage.Directory(root) ->
            //root can be absolute or relative to project folder
            let absRoot =
                if Path.IsPathRooted root then root
                    else Path.GetFullPath(Path.Combine(projectRoot,root))
            let storage = StorageLocal.Storage(absRoot)
            storage :> IStorage
        |   Azure(definition) ->
            let storage = StorageAzure.Storage(definition.AccountName, definition.AccountKey,definition.ContainerName)
            storage :> IStorage

// based on code from https://stackoverflow.com/questions/27012651/scheduling-with-async-parallel
// We can ask the agent to enqueue a new work item;
// and the agent sends itself a completed notification
type ThrottlingMessage = 
  | Enqueue of Async<unit>
  | Completed
  | ExpectedCount of int

let throttlingAgent limit =
    let mre = new ManualResetEvent(false)
    let completeAsync=
        Async.AwaitTask(System.Threading.Tasks.Task.Run(Action(fun () -> mre.WaitOne()|> ignore; mre.Dispose())))
    MailboxProcessor.Start(fun inbox -> async {
      // The agent body is not executing in parallel, 
      // so we can safely use mutable queue & counter 
      let queue = System.Collections.Generic.Queue<_>()
      let running = ref 0
      let mutable totCount = 1
      let mutable completeCounter = 0
      while completeCounter < totCount do
        // Enqueue new work items or decrement the counter
        // of how many tasks are running in the background
        let! msg = inbox.Receive()
        match msg with
        | Completed ->
            decr running
            completeCounter <- completeCounter + 1
            // printfn "throttling: %d complete" (int (round((float completeCounter)/(float totCount)*100.0)))
            if completeCounter = totCount then
                mre.Set() |> ignore
        | Enqueue w ->
            // printfn "throttling: enqueued new work package"
            queue.Enqueue(w)
        | ExpectedCount count ->
            // printfn "throttling: waiting for %d work packages" count
            totCount <- count            
        // If we have less than limit & there is some work to
        // do, then start the work in the background!
        while (running.Value < limit && queue.Count > 0) do
          let work = queue.Dequeue()
          incr running
          do! 
            // When the work completes, send 'Completed'
            // back to the agent to free a slot
            async {
                    // printfn "throttling: starting a work package"
                    do! work
                    // printfn "throttling: Complete work package"
                    inbox.Post(Completed) } 
            |> Async.StartChild
            |> Async.Ignore }),completeAsync

let getPresenseChecker (projectRoot:string) (storages:(string*Config.Storage) seq) =
    let singleStorageChecker pair =
        let (name:string),storageDef = pair
        
        let storage = createStorage projectRoot storageDef
        let check (versions:(Hash.HashString option) seq) =
            async {
                let versionsArray = Array.ofSeq versions
                let saveCheck v =                     
                    match v with
                    |   None -> async { return ArtefactType.Absent}
                    |   Some(ver) -> storage.IsInStorageAsync ver
                        
                let! isInStorage = versionsArray |> Array.map saveCheck |> Async.Parallel
                let mapper checkResult =
                    match checkResult with
                    |   ArtefactType.SingleFile -> Some(name)
                    |   ArtefactType.Directory -> Some(name)
                    |   ArtefactType.Absent -> None
                let namedResults = isInStorage |> Array.map mapper
                return namedResults
            }
        check
    let check (versions:(Hash.HashString option) array) =
        async {            
            let N = Array.length versions
            let checkers = Seq.map singleStorageChecker storages |> Array.ofSeq
            let! curVersionLocations = Array.map (fun checker -> checker versions) checkers |> Async.Parallel
            let initialState = Array.create N List.empty
            let folder acc singleStorageCheckResult =
                let listBuilder storageNames checkResult =
                    match checkResult with
                    |   None -> storageNames
                    |   Some(name) -> name::storageNames
                Array.map2 listBuilder acc singleStorageCheckResult
            let state = Array.fold folder initialState curVersionLocations
            return state
        }
    check

let maxDirChunks = 16

/// returns: fullpath -> version  -> Async<unit>
let getStorageSaver (projectRoot:string)  storageDef =    
    let storage = createStorage projectRoot storageDef

    let singleArtefactSaver (artefactFullPath:string) (version:Hash.HashString) =
        async {            
            if version.Length = 0 then
                return ()
            else
                let fullID = artefactFullPath.Replace(projectRoot, System.String.Empty)
                let isSingleFile = File.Exists artefactFullPath                
                let! existCheck = storage.IsInStorageAsync version
                let alreadyExists = 
                    match existCheck with
                    |   SingleFile -> true
                    |   Directory -> true
                    |   Absent -> false
                if alreadyExists then
                    Logger.logInfo Logger.Storage (sprintf "already saved  %s:%s" fullID (version.Substring(0,8).ToLower()))
                    return ()
                else
                    if isSingleFile then
                        use! stream = storage.getFileSaveStreamAsync version                        
                        Logger.logVerbose Logger.Storage (sprintf "saving  %s:%s" fullID (version.Substring(0,8).ToLower()))
                        do! ArtefactArchiver.archiveSingleFileToStreamAsync artefactFullPath stream                                                
                        Logger.logInfo Logger.Storage (sprintf "saved  %s:%s" fullID (version.Substring(0,8).ToLower()))
                    else
                        let files = System.IO.Directory.GetFiles(artefactFullPath,"*.*",System.IO.SearchOption.AllDirectories)
                        if Array.length files = 0 then return()
                        else    
                            let NBuckets = min (Array.length files) maxDirChunks
                            Logger.logVerbose Logger.Storage (sprintf "saving  %s:%s %d files into %d buckets" fullID (version.Substring(0,8).ToLower()) (Array.length files) NBuckets)
                            let buckets = Array.init NBuckets (fun _ -> System.Collections.Generic.List<string>())
                            // splitting files in NBuckets groups
                            let iterator idx filename =                            
                                let bucket_idx = idx % NBuckets
                                buckets.[bucket_idx].Add(filename)
                            Array.iteri iterator files

                            //Archiving buckets
                            let! bucketStreams = storage.getDirSaveStreamsAsync version NBuckets

                            let dirFullPath = artefactFullPath+Path.DirectorySeparatorChar.ToString()

                            let sharedCounter = ref 0 
                            let N = Array.length files
                            let counterSync = obj()

                            let fileCompleteCallback filename =
                                lock counterSync (fun () ->
                                    incr sharedCounter
                                    printfn "%d/%d\t %s Done" sharedCounter.Value N filename
                                ) 

                            let archiver = ArtefactArchiver.archiveDirFilesToStreamAsync fileCompleteCallback dirFullPath
                            let bucketComps = Array.map2 archiver buckets bucketStreams
                            let concurrencyLevel = System.Environment.ProcessorCount*2
                            Logger.logVerbose Logger.Storage (sprintf "using %d concurrent archivers" concurrencyLevel)
                            let bucketProcessor,waitComplete = throttlingAgent concurrencyLevel
                            bucketProcessor.Post (ExpectedCount (Array.length bucketComps))
                            Array.iter (fun x -> bucketProcessor.Post (Enqueue x)) bucketComps                        
                            do! waitComplete
                            bucketStreams |> Array.iter (fun s -> s.Flush(); s.Close(); s.Dispose())
                            Logger.logInfo Logger.Storage (sprintf "saved  %s:%s" fullID (version.Substring(0,8).ToLower()))
                            return ()                        
        }
    let saver artefacts =
        async {
            let distinctArtefacts = artefacts |> Array.distinctBy (fun art -> let name,ver = art in ver)
            let! _ = Async.Parallel <| Array.map (fun x -> let fullpath,version = x in singleArtefactSaver fullpath version) distinctArtefacts
            return ()
        }
    saver

//returns fullID -> version -> Async<unit>
let getStorageRestore (projectRoot:string)  (storageDef:Config.Storage) =    
    let restore (artefactId:ArtefactId) (version:Hash.HashString) =
        async {                
                let absPath = idToFullPath projectRoot artefactId
                let storage = createStorage projectRoot storageDef                         
                Logger.logVerbose Logger.Storage (sprintf "restoring  %A:%s" artefactId (version.Substring(0,8).ToLower()))
                let! checkResult = storage.IsInStorageAsync version                            
                match checkResult with
                |   SingleFile ->
                    let! stream = storage.getFileRestoreStreamAsync version
                    do! ArtefactArchiver.artefactFromArchiveStreamAsync absPath stream true
                    stream.Dispose()
                |   ArtefactType.Directory ->
                    let! streams = storage.getDirRestoreStreamsAsync version
                    let streamsCount = Array.length streams
                    let extracter stream = 
                        async {
                            // printfn "WP: starting async artefact restore"
                            do! ArtefactArchiver.artefactFromArchiveStreamAsync absPath stream false
                            // printfn "WP: async artefact restore done"
                            stream.Dispose()
                            //printfn "Last op of the work package"
                            }
                    let extractionComps = streams |> Array.map extracter
                    let concurrencyLevel = System.Environment.ProcessorCount*2
                    Logger.logVerbose Logger.Storage (sprintf "using pool of %d concurrent extractors to extract %d buckets" concurrencyLevel streamsCount)
                    let bucketProcessor,waitComplete = throttlingAgent concurrencyLevel
                    bucketProcessor.Post (ExpectedCount (Array.length extractionComps))
                    extractionComps |> Array.iter (fun x -> bucketProcessor.Post (Enqueue x))                    
                    do! waitComplete
                |   Absent -> raise(InvalidDataException("Artefact is absent in storage"))        
                logInfo LogCategory.Storage (sprintf "restored  %A:%s" artefactId (version.Substring(0,8).ToLower()))
                return ()            
        }
    restore