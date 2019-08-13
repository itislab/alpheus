namespace ItisLab.Alpheus.Tests

open System
open Xunit
open System.IO
open ItisLab.Alpheus
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.PathUtils
open Angara.Data

type PathUtilsTests(output)=
    inherit ItisLab.Alpheus.Tests.SampleExperiment.SampleExperiment(output)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\temp\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\Мои данные\")>]
    [<InlineData(TargetPlatform.Linux, "/с/data/")>]
    member s.``isDirectory returns true if a path point a directory``(targetPlatform: TargetPlatform, pathToDirectory : string) =
        if targetPlatform = s.Platform then
            Assert.True(isDirectory pathToDirectory)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\temp\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\Мои данные\Файл с данными без расширения")>]
    [<InlineData(TargetPlatform.Linux, "/с/data/my data.csv")>]
    [<InlineData(TargetPlatform.Linux, "/с/data/file")>]
    member s.``isDirectory returns false if a path points a file``(targetPlatform: TargetPlatform, pathToFile : string) =
        if targetPlatform = s.Platform then
            Assert.False(isDirectory pathToFile)
        
    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\data.alph", true)>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", false)>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\data.csv", false)>]
    [<InlineData(TargetPlatform.Linux, "/с/experiment/my data.alph", true)>]
    [<InlineData(TargetPlatform.Linux, "/с/experiment/my data.csv", false)>]
    [<InlineData(TargetPlatform.Linux, "/с/experiment/", false)>]
    member s.``isAlph file checks if the given path points an alph file``(targetPlatform: TargetPlatform, pathToFile : string, actualIsAlph: bool) =
        if targetPlatform = s.Platform then
            Assert.Equal(actualIsAlph, isAlphFile pathToFile)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\data.csv", @"data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"experiment\", @"experiment\data.csv", @"data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"experiment\", @"experiment2\data.csv", @"..\experiment2\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\my data\data.csv", @"my data\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\data\", @"data\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\my data\data\", @"my data\data\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\my data\mydata.alph", @"c:\experiment\my data\data.csv", @"data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/data.csv", @"data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"experiment/", @"experiment/data.csv", @"data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"experiment/", @"experiment2/data.csv", @"../experiment2/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/my data/data.csv", @"my data/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/data/", @"data/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/my data/data/", @"my data/data/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/my data/mydata.alph", @"/experiment/my data/data.csv", @"data.csv")>]
    member s.``relativePath makes the given path relative to a base path``(targetPlatform: TargetPlatform, basePath: string, targetPath: string, actualRelativePath: string) =
        if targetPlatform = s.Platform then
            let relative = relativePath basePath targetPath
            Assert.Equal(actualRelativePath, relative)

    [<Theory>]
    [<InlineData(@"experiment", @"c:\experiment\data.csv")>]
    [<InlineData(@"D:\experiment", @"C:\experiment\data.csv")>]
    [<InlineData(@"experiment", @"/experiment/data.csv")>]
    member s.``relativePath throws if an input is incorrect`` (basePath: string, targetPath:string) =
        try 
            relativePath basePath targetPath |> ignore 
            failwith "Expected exception, but not thrown"
        with 
            | :? ArgumentException as ae -> () // ok (we can't use Assert.Throws since it requires the exact exception type, not parent)
            | _ -> failwith "Unexpected exception type"
      
    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"my data\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"dir2\subdir\test.txt")>]
    [<InlineData(TargetPlatform.Windows, @"data\")>]
    [<InlineData(TargetPlatform.Windows, @"my data\data\")>]
    [<InlineData(TargetPlatform.Windows, @"dir2\subdir\test\")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/my data/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/dir2/subdir/test.txt")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/data/")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/my data/data/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/dir2/subdir/test/")>]
    member s.``idToExperimentPath returns a path to an artefact relative to the experiment root, given the artefact id``(targetPlatform: TargetPlatform, artefactPath: ExperimentRelativePath) =
        if targetPlatform = s.Platform then
            let id = ArtefactId.Path artefactPath
            let expPath = idToExperimentPath id
            Assert.Equal(artefactPath, expPath)


    [<Theory>]
    [<InlineData(@".\experiment\")>]
    [<InlineData(@"./experiment/")>]
    [<InlineData("experiment")>]
    member s.``idToFullPath throws if an experiment path is incorrect`` (experimentPath: string) =
        let artefactId = ArtefactId.Path "test.txt"
        Assert.Throws<ArgumentException>(fun () -> idToFullPath experimentPath artefactId |> ignore)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"data.csv", @"c:\experiment\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"my data\data.csv", @"c:\experiment\my data\data.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"dir2/subdir/test.txt", @"c:\experiment\dir2\subdir\test.txt")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"data\", @"c:\experiment\data\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"my data\data\", @"c:\experiment\my data\data\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"dir2/subdir/test/", @"c:\experiment\dir2\subdir\test\")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/", @"data.csv", @"/c/experiment/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/", @"my data/data.csv", @"/c/experiment/my data/data.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment", @"dir2/subdir/test.txt", @"/experiment/dir2/subdir/test.txt")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/", @"data/", @"/c/experiment/data/")>]
    [<InlineData(TargetPlatform.Linux, @"/c/experiment/", @"my data/data/", @"/c/experiment/my data/data/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment", @"dir2/subdir/test/", @"/experiment/dir2/subdir/test/")>]
    member s.``idToFullPath returns the full path to the artefact, given the artefact id`` (targetPlatform: TargetPlatform, experimentPath: string, artefactPath: ExperimentRelativePath, actualFullPath: string) =
        if targetPlatform = s.Platform then
            let id = ArtefactId.Path artefactPath
            let fullPath = idToFullPath experimentPath id
            Assert.Equal(actualFullPath, fullPath)
            
    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\test.alph", @"c:\experiment\", @"test.dat", @"source\test.dat")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\test.alph", @"c:\experiment\", @"test\", @"source\test\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\vector-test.alph", @"c:\experiment\", @"*\test.dat", @"source\*\test.dat")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\vector-test.alph", @"c:\experiment\", @"*.dat", @"source\*.dat")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\vector-test.alph", @"c:\experiment\", @"*\test\", @"source\*\test\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\vector-test.alph", @"c:\experiment\", @"*\", @"source\*\")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/test.alph", @"/experiment/", @"test.dat", @"source/test.dat")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/test.alph", @"/experiment/", @"test/", @"source/test/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/vector-test.alph", @"/experiment/", @"*/test.dat", @"source/*/test.dat")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/vector-test.alph", @"/experiment/", @"*.dat", @"source/*.dat")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/vector-test.alph", @"/experiment/", @"*/test/", @"source/*/test/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/vector-test.alph", @"/experiment/", @"*/", @"source/*/")>]
    member s.``alphRelativePathToId transforms a path relative to the alph file into ArtefactId``(targetPlatform: TargetPlatform, alphFilePath:string, experimentRoot:string, alphRelativeArtefactPath:AlphRelativePath, actualArtefactPath:ExperimentRelativePath) =
        if targetPlatform = s.Platform then
            let artefactId = alphRelativePathToId alphFilePath experimentRoot alphRelativeArtefactPath
            Assert.Equal(actualArtefactPath, idToExperimentPath artefactId)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\source\test.csv", @"source\test.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\source\*.csv", @"source\*.csv")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\source\test\", @"source\test\")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\", @"c:\experiment\source\test\*\", @"source\test\*\")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/source/test.csv", @"source/test.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/source/*.csv", @"source/*.csv")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/source/test/", @"source/test/")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"/experiment/source/test/*/", @"source/test/*/")>]
    member s.``pathToId creates ArtefactId from a path to an artefact``(targetPlatform: TargetPlatform, experimentRoot:string, artefactPath:string, actualPath: ExperimentRelativePath) =
        if targetPlatform = s.Platform then
            let id = pathToId experimentRoot artefactPath
            Assert.Equal(actualPath, idToExperimentPath id)

    [<Fact>]
    member s.``pathToId creates ArtefactId from a path to an alph file``() =
        let alphFilePath = Path.Combine(s.Path, "source", "test.alph")
        Directory.CreateDirectory(Path.GetDirectoryName(alphFilePath)) |> ignore
        let artefactPath = Path.Combine(s.Path, "source", "test.dat")
        let relativeArtefactPath     = relativePath alphFilePath artefactPath
        let snapshortSection : AlphFiles.VersionedArtefact = { RelativePath = relativeArtefactPath; Hash = MdMap.scalar (Some "0000") }
        let alphFile : AlphFiles.AlphFile = {
            IsTracked = true
            Origin = AlphFiles.SourceOrigin snapshortSection
        }
        AlphFiles.saveAsync alphFile alphFilePath |> Async.RunSynchronously

        let artefactId = pathToId s.Path alphFilePath 
        Assert.Equal(relativePath s.Path artefactPath, idToExperimentPath artefactId)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\test.csv", @"c:\experiment\source\test.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\test\", @"c:\experiment\source\test.alph")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\*.csv", @"c:\experiment\source\vector.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\*\", @"c:\experiment\source\vector.alph")>]
    [<InlineData(TargetPlatform.Windows, @"c:\experiment\source\*\data\*\*.csv", @"c:\experiment\source\vector-data-vector-vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/test.csv", @"/experiment/source/test.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/test/", @"/experiment/source/test.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/*.csv", @"/experiment/source/vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/*/", @"/experiment/source/vector.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/source/*/data/*/*.csv", @"/experiment/source/vector-data-vector-vector.csv.alph")>]
    member s.``pathToAlphFile returns the path of the corresponding alph file``(targetPlatform: TargetPlatform, artefactPath: string, actualPath: string) =
        if targetPlatform = s.Platform then 
            Assert.Equal(actualPath, pathToAlphFile artefactPath)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"source\test\", @"source\test.alph")>]
    [<InlineData(TargetPlatform.Windows, @"test", @"test.alph")>]
    [<InlineData(TargetPlatform.Windows, @"test.csv", @"test.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"source\test.csv", @"source\test.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"source\*.csv", @"source\vector.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"source\*\", @"source\vector.alph")>]
    [<InlineData(TargetPlatform.Windows, @"source\*\*\data\*.csv", @"source\vector-vector-data-vector.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"*\", @"vector.alph")>]
    [<InlineData(TargetPlatform.Linux, @"source/test/", @"source/test.alph")>]
    [<InlineData(TargetPlatform.Linux, @"source/test.csv", @"source/test.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"source/*.csv", @"source/vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"source/*/", @"source/vector.alph")>]
    [<InlineData(TargetPlatform.Linux, @"source/*/*/data/*.csv", @"source/vector-vector-data-vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"*/", @"vector.alph")>]
    member s.``idToAlphFilePath returns the path to the corresponding alph file``(targetPlatform: TargetPlatform, artefactPath: ExperimentRelativePath, expectedAlphFilePath: ExperimentRelativePath) =
        if targetPlatform = s.Platform then 
            let id = ArtefactId.Path artefactPath
            Assert.Equal(expectedAlphFilePath, idToAlphFilePath id)

    [<Theory>]
    [<InlineData(@".\experiment\")>]
    [<InlineData(@"./experiment/")>]
    [<InlineData("experiment")>]
    member s.``idToAlphFileFullPath throws if the experiment path is incorrect`` (experimentPath: string) =
        let artefactId = ArtefactId.Path "test.txt"
        Assert.Throws<ArgumentException>(fun () -> idToAlphFileFullPath experimentPath artefactId |> ignore)

    [<Theory>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"source\test\", @"C:\experiment\source\test.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"test", @"C:\experiment\test.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"test.csv", @"C:\experiment\test.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"source\test.csv", @"C:\experiment\source\test.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"source\*.csv", @"C:\experiment\source\vector.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"source\*\", @"C:\experiment\source\vector.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"source\*\*\data\*.csv", @"C:\experiment\source\vector-vector-data-vector.csv.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"*\", @"C:\experiment\vector.alph")>]
    [<InlineData(TargetPlatform.Windows, @"C:\experiment\", @"dir1/test2/", @"C:\experiment\dir1\test2.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"source/test/", @"/experiment/source/test.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"source/test.csv", @"/experiment/source/test.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"source/*.csv", @"/experiment/source/vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"source/*/", @"/experiment/source/vector.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"source/*/*/data/*.csv", @"/experiment/source/vector-vector-data-vector.csv.alph")>]
    [<InlineData(TargetPlatform.Linux, @"/experiment/", @"*/", @"/experiment/vector.alph")>]
    member s.``idToAlphFileFullPath returns the full path to the corresponding alph file``(targetPlatform: TargetPlatform, experimentRoot: string, artefactPath: ExperimentRelativePath, expectedAlphFilePath: string) =
        if targetPlatform = s.Platform then 
            let id = ArtefactId.Path artefactPath
            let alpFileFullPath = idToAlphFileFullPath experimentRoot id
            Assert.Equal(expectedAlphFilePath, alpFileFullPath)


    [<Theory>]
    [<MemberData("FileArtefactsForEnumerate")>]
    member s.``enumerateItems enumerates files according to a pattern`` (artefactId:string, expected: (string list * string)[]) =
        let artefactId = ArtefactId.Path artefactId
        let items = enumerateItems s.FullPath artefactId
        let simplify = relativePath s.FullPath >> unixPath
        let actual = items |> MdMap.toSeq |> Seq.map(fun (keyPaths, valuePath) -> keyPaths |> List.map simplify, simplify valuePath) |> Seq.toArray
        Assert.Equal<string list * string>(expected, actual)
        //let files = enumerateItems s.FullPath artefactId |> Seq.map (fun () relativePath s.FullPath >> unixPath) |> Seq.toArray
        //Assert.Equal<string>(expectedFiles, files)

    static member FileArtefactsForEnumerate : obj[][] = 
        [| [| "dir1/test1.txt"; [| List.empty<string>, "dir1/test1.txt" |] |]
           [| "dir1/*.txt"; [| [ "dir1/test1.txt" ],"dir1/test1.txt" |] |]
           [| "dir*/test1.txt"; [| ["dir1"; "dir1/test1.txt" ], "dir1/test1.txt" |] |]
           [| "*/*.txt"; [| ["dir1"; "dir1/test1.txt" ], "dir1/test1.txt"; ["dir2"; "dir2/test3.txt" ], "dir2/test3.txt" |] |]
           [| "dir3/*/*.txt"; [| ["dir3/dir5"; "dir3/dir5/test5.txt"], "dir3/dir5/test5.txt" |] |]
           [| "*/*/*.txt"; [| ["dir3"; "dir3/dir5"; "dir3/dir5/test5.txt"], "dir3/dir5/test5.txt" |] |]
           [| "*/"; [| ["dir1"], "dir1"; ["dir2"], "dir2"; ["dir3"], "dir3" |] |]
           [| "*/*/"; [| ["dir1";"dir1/test2"], "dir1/test2"; ["dir3";"dir3/dir5"], "dir3/dir5" |] |]
        |]