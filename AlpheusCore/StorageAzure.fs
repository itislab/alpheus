module ItisLab.Alpheus.StorageAzure

open ItisLab.Alpheus.Storage
open Azure.Storage.Blobs
open System
open System.IO
open System.Diagnostics

type Storage(accountName:string,accountKey:string,containerName:string) =
    let storageCredentials = Auth.StorageCredentials(accountName,accountKey)
    let cloudStorageAccount = CloudStorageAccount(storageCredentials, true)
    let blobClient = cloudStorageAccount.CreateCloudBlobClient()
    let container = blobClient.GetContainerReference(containerName)
    let getBlobStream blobName =
        async {
            let blob = container.GetBlockBlobReference(blobName)     
            let! blobStream = Async.AwaitTask (blob.OpenWriteAsync())
            return (blobStream :> Stream)
        }
    do
        try
            container.CreateIfNotExistsAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore
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
                        let fileBlob = container.GetBlockBlobReference(version+".file")
                        let dirBlob =  container.GetBlockBlobReference(version+".dir-0")
                        let! res =  [|Async.AwaitTask (fileBlob.ExistsAsync()) ; Async.AwaitTask(dirBlob.ExistsAsync()) |] |> Async.Parallel                    
                        let result =
                            match res.[0],res.[1] with
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
                let fileBlob = container.GetBlockBlobReference(version+".file")                
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
                    let blobRef = container.GetBlockBlobReference(blobName)
                    let! exists = Async.AwaitTask(blobRef.ExistsAsync())                    
                    if exists then
                        streamTasks <- blobRef.OpenReadAsync() :: streamTasks
                    else
                        stop <- true
                
                let! streams = streamTasks |> List.toArray |> Array.map Async.AwaitTask |> Async.Parallel
                printfn "Azure storage: %d streams are opened for reading" (Array.length streams)
                return streams
            }



