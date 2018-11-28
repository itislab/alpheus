module ItisLab.Alpheus.ArtefactArchiver

open System.IO
open System.IO.Compression
open System.Threading.Tasks
open System.Threading

type ArchivingMessage =
    | Filename of filename:string
    | Filedata of filename:string*data:Stream
    | Done of filename:string
    | ExpectedFileCount of int

let archiveSingleFileToStreamAsync (fileAbsPath:string) (streamToWriteTo:Stream) =
    async {
        use archive = new ZipArchive(streamToWriteTo,ZipArchiveMode.Create,true)
        use fileStream = new FileStream(fileAbsPath, FileMode.Open, FileAccess.Read)
        use fileStreamInZip = archive.CreateEntry("__artefact__", CompressionLevel.Fastest).Open()
        do! Async.AwaitTask (fileStream.CopyToAsync fileStreamInZip)
        return ()                            
    }

let archiveDirFilesToStreamAsync (syncObj:obj) (totCount:int) (sharedcounter:int ref) (directoryFullPath:string) (fileFullPaths:string seq) (streamToWriteTo:Stream) =
    async {        
        let fileFullPaths = Array.ofSeq fileFullPaths
        use archive = new ZipArchive(streamToWriteTo,ZipArchiveMode.Create,true)
        
        let archivingDoneEvent = ManualResetEvent(false)
        let readConcurrency = 16       
        let archivingAgent = MailboxProcessor.Start(fun inbox -> async {
            let filenamesQ = System.Collections.Generic.Queue<string>()
            let filedataQ = System.Collections.Generic.Queue<string*Stream>()
            let reading = ref 0
            let archiving = ref 0
            let processed = ref 0
            let mutable totalFiles = 1            
            while processed.Value<totalFiles do
                // 1) handling message
                let! msg = inbox.Receive()
                match msg with
                |   Filename fn -> filenamesQ.Enqueue fn                        
                |   Filedata(name,stream) -> filedataQ.Enqueue (name,stream)
                |   Done name ->
                    decr reading // permit one more reading
                    decr archiving // permit archiving
                    incr processed
                    lock syncObj (fun () ->
                        incr sharedcounter
                        printfn "%d/%d\t %s Done" sharedcounter.Value totCount name
                    )                    
                |   ExpectedFileCount count ->
                    totalFiles <- count                    
                    
                // 2) handling queues and counters
                while (filenamesQ.Count>0 && reading.Value<readConcurrency) || (filedataQ.Count>0 && archiving.Value<1)  do
                    if (filenamesQ.Count>0 && reading.Value<readConcurrency) then
                        let filePath = filenamesQ.Dequeue()
                        incr reading                    
                        async {
                            //printfn "Reading %s" filePath
                            let fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read)
                            let memStream = new MemoryStream()
                            do! Async.AwaitTask(fileStream.CopyToAsync memStream)
                            fileStream.CopyTo memStream
                            fileStream.Close()
                            fileStream.Dispose()
                            memStream.Seek(0L,SeekOrigin.Begin) |> ignore
                            //printfn "Read %s into memory" filePath
                            inbox.Post(Filedata (filePath,memStream))
                        } |> Async.Start
                    if (filedataQ.Count>0 && archiving.Value<1) then
                        let filePath, memStream = filedataQ.Dequeue()
                        incr archiving
                        async {
                            //printfn "Archiving %s" filePath
                            let nameInArchive =                                 
                                // relative path
                                filePath.Replace(directoryFullPath,System.String.Empty)
                                
                            let fileStreamInZip = archive.CreateEntry(nameInArchive, CompressionLevel.Fastest).Open()
                            do! Async.AwaitTask (memStream.CopyToAsync fileStreamInZip)
                            memStream.Dispose()
                            fileStreamInZip.Dispose()
                            //printfn "Archived %s" filePath
                            inbox.Post(Done filePath)
                        } |> Async.Start
            printfn "All %d files are saved" totalFiles |> ignore
            archivingDoneEvent.Set() |> ignore          
            })    

        archivingAgent.Post(ExpectedFileCount(Array.length fileFullPaths))
        fileFullPaths |> Array.iter (fun name -> archivingAgent.Post(Filename name))            
        do! Async.AwaitTask (Task.Run(System.Action(fun () -> archivingDoneEvent.WaitOne() |> ignore)))
        archivingDoneEvent.Dispose()

        return ()
    }

let artefactFromArchiveStreamAsync (targetAbsPath:string) (streamToReadFrom:Stream) isSingleFile =
    async {
        do! Async.SwitchToThreadPool()
        use archive = new ZipArchive(streamToReadFrom, ZipArchiveMode.Read, true)
        if isSingleFile then
            let entry = archive.GetEntry("__artefact__")
            entry.ExtractToFile(targetAbsPath,true)
        else
            archive.ExtractToDirectory targetAbsPath
    }