module ItisLab.Alpheus.Tests.StorageTests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.Storage
open ItisLab.Alpheus.Hash
open ItisLab.Alpheus

[<AbstractClass>]
/// Class tests abstract IStorage with some common tests
type CommonTests() =    

    /// Storage to test
    abstract member Storage:IStorage with get

    [<Fact>]
    member s.``File artefact presence check``() =
        async {
            let versionToCheck:HashString = "00011122233445566778899"
            let testData = "This string will be used as test data" |> System.Text.Encoding.UTF8.GetBytes

            let! isAlreadyInStoreage = s.Storage.IsInStorageAsync versionToCheck
            match isAlreadyInStoreage with
            |   Absent -> ()
            |   _ -> Assert.True(false,"Artefact is not expected to be in empty storage")

            let! saveStream = s.Storage.getFileSaveStreamAsync versionToCheck
            do! Async.AwaitTask (saveStream.WriteAsync(testData,0,testData.Length))
            do! Async.AwaitTask (saveStream.FlushAsync())
            saveStream.Close()

            let! isInStoreage = s.Storage.IsInStorageAsync versionToCheck
            match isInStoreage with
            |   SingleFile -> ()
            |   _ -> Assert.True(false,"Single file artefact is expected to be in the storage")
        } |> toAsyncFact

    /// Helper that writes the hardcoded data into the directory artefact streams and checks that presence of the directory artefact
    member private s.DirectoryArtefactPresenceCheck numberOfStreams =
        async {
            let versionToCheck:HashString = sprintf "dasdad1e23e23d%d" numberOfStreams
            let testData = "This string will be used as test data" |> System.Text.Encoding.UTF8.GetBytes

            let! isAlreadyInStoreage = s.Storage.IsInStorageAsync versionToCheck
            match isAlreadyInStoreage with
            |   Absent -> ()
            |   _ -> Assert.True(false,"Artefact is not expected to be in empty storage")

            let! saveStreams = s.Storage.getDirSaveStreamsAsync versionToCheck numberOfStreams

            let writeToStreamAsync (stream:Stream) =
                async {
                    do! Async.AwaitTask (stream.WriteAsync(testData,0,testData.Length))
                    do! Async.AwaitTask (stream.FlushAsync())
                    stream.Close()
                }

            let! dummy =  saveStreams |> Array.map writeToStreamAsync |> Async.Parallel

            let! isInStoreage = s.Storage.IsInStorageAsync versionToCheck
            match isInStoreage with
            |   Directory -> ()
            |   _ -> Assert.True(false,"Directory artefact is expected to be in the storage")
        } 

    [<Fact>]
    member s.``Directory artefact presence check (1 stream)``() =
         s.DirectoryArtefactPresenceCheck 1 |> toAsyncFact

    [<Fact>]
    member s.``Directory artefact presence check (32 streams)``() =
        s.DirectoryArtefactPresenceCheck 1 |> toAsyncFact

    [<Fact>]
    member s.``File artefact content is preserved``() =
        async {
            let versionToCheck:HashString = "wqscuh71d3"
            let testData = "This string will be used as test data..." |> System.Text.Encoding.UTF8.GetBytes

            // writing to storage
            let! saveStream = s.Storage.getFileSaveStreamAsync versionToCheck
            do! Async.AwaitTask (saveStream.WriteAsync(testData,0,testData.Length))
            do! Async.AwaitTask (saveStream.FlushAsync())
            saveStream.Close()

            // restoring from storage
            let! restoreStream = s.Storage.getFileRestoreStreamAsync versionToCheck
            
            let buffer = Array.zeroCreate<byte> 32768
            let! readBytes = Async.AwaitTask(restoreStream.ReadAsync(buffer,0,buffer.Length))

            restoreStream.Close()

            Assert.Equal(readBytes,testData.Length)

            let readShrinked = Array.take testData.Length buffer

            Assert.Equal<Byte>(readShrinked, testData)

        } |> toAsyncFact

    [<Fact>]
       member s.``Different file artefact versions store different data``() =
           async {
               let versionToCheck:HashString array = [| "wqscuh71d3"; "version2" |]
               let testData = [|"data1";"data2"|] |> Array.map System.Text.Encoding.UTF8.GetBytes

               let writeDataAsync content version =
                   async {
                       // writing to storage
                       let! saveStream = s.Storage.getFileSaveStreamAsync version
                       do! Async.AwaitTask (saveStream.WriteAsync(content,0,content.Length))
                       do! Async.AwaitTask (saveStream.FlushAsync())
                       saveStream.Close()
                   }
               
               let readDataAsync version =
                   async {
                       // restoring from storage
                       let! restoreStream = s.Storage.getFileRestoreStreamAsync version
               
                       let buffer = Array.zeroCreate<byte> 32768
                       let! readBytes = Async.AwaitTask(restoreStream.ReadAsync(buffer,0,buffer.Length))

                       restoreStream.Close()
                       return Array.take readBytes buffer
                   }
               
               // writes
               let! dummy = Array.map2 (fun c v -> writeDataAsync c v) testData versionToCheck |> Async.Parallel

               // reads
               let! restoredFiles = Array.map readDataAsync versionToCheck |> Async.Parallel

               Array.iter2 (fun (restored:byte array) orig -> Assert.Equal<byte>(restored,orig)) restoredFiles testData

           } |> toAsyncFact

    /// Helper that writes the hardcoded data into the directory artefact streams and checks that it can be restored
    member private s.DirectoryArtefactContentCheck numberOfStreams =
        async {
            let versionToCheck:HashString = sprintf "jknca7c3kj3s%d" numberOfStreams
            let testString = "This string will be used as test data"

            let testStringArray = Array.init numberOfStreams (fun i -> sprintf "%s - %d" testString i)
            let testDataArray = Array.map (fun (s:string) -> System.Text.Encoding.UTF8.GetBytes(s)) testStringArray

            // Getting streams to write into
            let! saveStreams = s.Storage.getDirSaveStreamsAsync versionToCheck numberOfStreams

            // writing into these streams
            let writeToStreamAsync idx (stream:Stream) =
                async {
                    do! Async.AwaitTask (stream.WriteAsync(testDataArray.[idx],0,testDataArray.[idx].Length))
                    do! Async.AwaitTask (stream.FlushAsync())
                    stream.Close()
                    return ()
                }
            let! dummy =  saveStreams |> Array.mapi writeToStreamAsync |> Async.Parallel

            // now restoring
            let! restoreStreams = s.Storage.getDirRestoreStreamsAsync versionToCheck

            Assert.Equal(numberOfStreams, restoreStreams.Length)

            let readFromStreamAsync (stream:Stream) =
                async {
                    let buffer = Array.zeroCreate<byte> 32768
                    let! readCount = Async.AwaitTask(stream.ReadAsync(buffer,0,buffer.Length))
                    let res = Array.take readCount buffer
                    stream.Close()
                    return res
                }

            let! restoredDataArray = restoreStreams |> Array.map readFromStreamAsync |> Async.Parallel
            Array.iter2 (fun (restored:byte array) original -> Assert.Equal<Byte>(restored,original)) restoredDataArray testDataArray
        } 

    [<Fact>]
    member s.``Directory artefact content is preserved (1 stream)`` () =
        s.DirectoryArtefactContentCheck 1 |> toAsyncFact

    [<Fact>]
    member s.``Directory artefact content is preserved (32 stream)`` () =
        s.DirectoryArtefactContentCheck 32 |> toAsyncFact

/// Applies the tests defined in CommonTests to the local storage
type Local(output) =
    inherit CommonTests()

    let singleUseDir = new SingleUseOneTimeDirectory(output)
    let localStorage = StorageLocal.Storage(singleUseDir.Path)

    override s.Storage
        with get() = upcast localStorage

    interface IDisposable with
        member s.Dispose() =
            (singleUseDir :> IDisposable).Dispose()