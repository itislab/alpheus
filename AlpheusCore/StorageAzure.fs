module ItisLab.Alpheus.StorageAzure

open ItisLab.Alpheus.Storage
open Azure.Storage.Blobs
open System
open System.IO
open System.Diagnostics
open Azure.Storage

type Storage(accountName:string,accountKey:string,containerName:string) =
    let sharedKeyCredential = StorageSharedKeyCredential(accountName, accountKey);

    let blobUri = "https://" + accountName + ".blob.core.windows.net";

    let blobServiceClient = BlobServiceClient(Uri(blobUri), sharedKeyCredential);
    
    let containerClient = blobServiceClient.GetBlobContainerClient(containerName)
    
    let getBlobStream blobName =
        async {
            let blobClient = containerClient.GetBlobClient(blobName)
            let! blobStream = Async.AwaitTask (blobClient.OpenWriteAsync(true))
            return blobStream
        }
    do
        try
            containerClient.CreateIfNotExistsAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
        with
        | :? AggregateException as agex ->            
            printfn "Failed to initialize Azure storage. Check azure credentials and container name validity (%s)" (agex.ToString())
            raise(agex)
    interface IStorage with
        member s.IsInStorageAsync (version: HashString) =
            async {                
                    if String.IsNullOrEmpty(version) then
                        return Storage.Absent
                    else
                        let fileBlob = containerClient.GetBlobClient(version+".file")
                        let dirBlob =  containerClient.GetBlobClient(version+".dir-0")
                        let! res =  [|Async.AwaitTask (fileBlob.ExistsAsync()) ; Async.AwaitTask(dirBlob.ExistsAsync()) |] |> Async.Parallel
                        let result =
                            match res.[0].Value,res.[1].Value with
                            |   true,false -> Storage.SingleFile
                            |   false,true -> Storage.Directory
                            |   false,false -> Storage.Absent
                            |   true,true -> raise(InvalidDataException("Both file and dir exists with the same version"))
                        return result
            }
        
        member s.getFileSaveStreamAsync version =
                getBlobStream (version+".file")
        member s.getDirSaveStreamsAsync version streamsCount =
                Seq.init streamsCount (fun idx -> sprintf "%s.dir-%d" version idx ) |> Seq.map getBlobStream |> Async.Parallel

        member s.getFileRestoreStreamAsync version =
            async {
                let fileBlob = containerClient.GetBlobClient(version+".file")                
                let! blobStream = Async.AwaitTask (fileBlob.OpenReadAsync())
                return blobStream
            }

        member s.getDirRestoreStreamsAsync version =
            async {
                let mutable streamTasks = []
                let mutable idx =0
                let mutable stop = false
                while not stop do
                    let blobName = sprintf "%s.dir-%d" version idx
                    idx <- idx + 1
                    let blobRef = containerClient.GetBlobClient(blobName)
                    let! exists = Async.AwaitTask(blobRef.ExistsAsync())                    
                    if exists.Value then
                        streamTasks <- blobRef.OpenReadAsync() :: streamTasks
                    else
                        stop <- true
                
                let! streams = streamTasks |> List.toArray |> Array.map Async.AwaitTask |> Async.Parallel
                printfn "Azure storage: %d streams are opened for reading" (Array.length streams)
                return streams
            }



