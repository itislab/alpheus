module ItisLab.Alpheus.Tests.ArtefactArchiver

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.ArtefactArchiver

type ArchiverTests() =
    inherit SingleUseOneTimeDirectory()

    let assertTextFilesContentEqualAsync expectedFile actualFile =
        async {
            let! origContent = Async.AwaitTask(File.ReadAllTextAsync expectedFile)
            let! restoredContent = Async.AwaitTask(File.ReadAllTextAsync actualFile)

            Assert.Equal(origContent,restoredContent)
        }

    [<Fact>]
    member s.``File Artefact round archiving``() =
        async {
            let origPath = @"data/texturalData.txt"

            use memStream = new MemoryStream()
            do! archiveSingleFileToStreamAsync origPath memStream
            
            memStream.Seek(0L,SeekOrigin.Begin) |> ignore
    
            let outPath = Path.Combine(s.Path,"restored.txt")

            do! artefactFromArchiveStreamAsync outPath memStream  true

            do! assertTextFilesContentEqualAsync origPath outPath
        } |> toAsyncFact

    [<Fact>]
    member s.``Directory artefact round archiving``() =
        async {
            let files = 
                [|
                    @"folder_with_files/TextFile3.txt"
                    @"folder_with_files/subfolder/TextFile1.txt"
                    @"folder_with_files/subfolder/TextFile2.txt"
                |]

            let origFiles = Array.map (fun x -> Path.Combine("data",x)) files

            let directoryOrigArtefact = "data"

            use memStream = new MemoryStream()
            do! archiveDirFilesToStreamAsync (fun _ -> ()) directoryOrigArtefact origFiles memStream
            
            memStream.Seek(0L,SeekOrigin.Begin) |> ignore
    
            let outPath = Path.Combine(s.Path,"restored")

            let outFiles = Array.map (fun x -> Path.Combine(outPath,x)) files

            do! artefactFromArchiveStreamAsync outPath memStream false

            let asyncChecks = Array.map2 (fun expected actual -> assertTextFilesContentEqualAsync expected actual) origFiles outFiles |> Async.Parallel

            let! dummy = asyncChecks
            
            ()
        } |> toAsyncFact

    [<Fact>]
    member s.``Directory artefact archiving reports filenames``() =
        async {
            let files = 
                [|
                    @"folder_with_files/TextFile3.txt"
                    @"folder_with_files/subfolder/TextFile1.txt"
                    @"folder_with_files/subfolder/TextFile2.txt"
                |]

            let origFiles = Array.map (fun x -> Path.Combine("data",x)) files

            let directoryOrigArtefact = "data"

            use memStream = new MemoryStream()

            let origNameSet = Set.ofArray origFiles

            let syncObj = obj()

            let mutable reportedNames = Set.empty

            let callback filename =
                lock syncObj (fun () -> reportedNames <- Set.add filename reportedNames)

            do! archiveDirFilesToStreamAsync callback directoryOrigArtefact origFiles memStream
                
            Assert.Equal<string>(origNameSet, reportedNames)

            ()
        } |> toAsyncFact