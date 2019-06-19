module ItisLab.Alpheus.Tests.ConvertionHelpers

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.AlphFiles

[<Fact>]
let ``fullIDtoFullPath handles windows file artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID "test.txt"
    let fullPath = fullIDtoFullPath @"C:\dir1" id1
    Assert.Equal(fullPath,@"C:\dir1\test.txt")

[<Fact>]
let ``fullIDtoFullPath handles windows long id file artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"dir2\subdir\test.txt"
    let fullPath = fullIDtoFullPath @"C:\dir1" id1
    Assert.Equal(fullPath,@"C:\dir1\dir2\subdir\test.txt")


[<Fact>]
let ``fullIDtoFullPath handles windows directory artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"test\"
    let fullPath = fullIDtoFullPath @"C:\dir1" id1
    Assert.Equal(fullPath,@"C:\dir1\test\")

[<Fact>]
let ``fullIDtoFullPath checks that rootPath is fully qualified`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"test\"
    Assert.Throws<Exception>(fun () -> fullIDtoFullPath @".\dir1" id1 |> ignore)
    

[<Fact>]
let ``fullIDtoRelative handles file artefact`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\file1.alph"
    let artefactFullID = ArtefactFullID.ID @"subrid1\text.txt"

    let relID = fullIDtoRelative root alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID, @"subrid1\text.txt")

[<Fact>]
let ``fullIDtoRelative handles file artefact with parent dir refererence`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir2\file1.alph"
    let artefactFullID = ArtefactFullID.ID @"subrid1\text.txt"

    let relID = fullIDtoRelative root alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID, @"..\subrid1\text.txt")

[<Fact>]
let ``fullIDtoRelative handles directory artefact`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\file1.alph"
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"

    let relID = fullIDtoRelative root alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID, @"subrid1\test\")

[<Fact>]
let ``fullIDtoRelative handles directory artefact with parent dir refererence`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir2\file1.alph"
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"

    let relID = fullIDtoRelative root alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID, @"..\subrid1\test\")

[<Fact>]
let ``fullIDtoRelative checkes that alph file path is full path`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @".\file1.alph" // <-- this path is relative, that't the problem
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative root alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that alph file path leads to dot-alph file`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\file1.txt" // <-- here is not .alph file. Thats the problem,
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative root alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that rootPath is fully qualified path`` () =
    let root = @".\dir1" // <-- here is relative path, thats the problem
    let alphFileFullPath = @"C:\file1.alph"
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative root alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that alph file is under the root path`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\file1.alph" // <- thats the problem, the .alph file is out of root path
    let artefactFullID = ArtefactFullID.ID @"subrid1\test\"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative root alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that rootPath is fully qualified`` () =
    let root = @".\dir1" // <-- here is relative path, thats the problem
    let alphFileFullPath = @"C:\dir\subdir1\file1.alph"
    let artefactRelID = RelativeArtefactID.ID @"..\test\"
    Assert.Throws<Exception>(fun () -> relIDtoFullID root alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that dot-alph file path is fully qualified`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @".\subdir1\file1.alph" // <-- here is relative path, thats the problem
    let artefactRelID = RelativeArtefactID.ID @"..\test\"
    Assert.Throws<Exception>(fun () -> relIDtoFullID root alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that dot-alph file path is under the root path`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\subdir1\file1.alph" // <- thats the problem, the .alph file is out of root path
    let artefactRelID = RelativeArtefactID.ID @"..\test\"
    Assert.Throws<Exception>(fun () -> relIDtoFullID root alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID handles file artefact`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir1\file1.alph"
    let artefactRelID = RelativeArtefactID.ID @"test.txt"
    let fullID = relIDtoFullID root alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"subdir1\test.txt")

[<Fact>]
let ``relIDtoFullID handles directory artefact`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir1\file1.alph"
    let artefactRelID = RelativeArtefactID.ID @"test\"
    let fullID = relIDtoFullID root alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"subdir1\test\")

[<Fact>]
let ``relIDtoFullID handles file artefact with parent folder reference`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir1\file1.alph"
    let artefactRelID = RelativeArtefactID.ID @"..\test.txt"
    let fullID = relIDtoFullID root alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"test.txt")

[<Fact>]
let ``relIDtoFullID handles directory artefact with parent folder reference`` () =
    let root = @"C:\dir1"
    let alphFileFullPath = @"C:\dir1\subdir1\file1.alph"
    let artefactRelID = RelativeArtefactID.ID @"..\test\"
    let fullID = relIDtoFullID root alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"test\")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact`` () =
    let artefactPath = @"dir1\test.txt"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile,@"dir1\test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact with parent relative reference`` () =
    let artefactPath = @"..\test.txt"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile, @"..\test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact with absolute path`` () =
    let artefactPath = @"C:\dir1\test.txt"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile, @"C:\dir1\test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact`` () =
    let artefactPath = @"dir1\test\"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile,@"dir1\test.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact with parent relative reference`` () =
    let artefactPath = @"..\test\"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile, @"..\test.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact with absolute path`` () =
    let artefactPath = @"C:\dir1\test\"
    let alphFile = artefactPathToAlphFilePath artefactPath
    Assert.Equal(alphFile, @"C:\dir1\test.alph")