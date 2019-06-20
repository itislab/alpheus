﻿module ItisLab.Alpheus.Tests.ConvertionHelpers

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.AlphFiles

[<Fact>]
let ``fullIDtoFullPath handles file artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID "test.txt"
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let fullPath = fullIDtoFullPath rootPath id1
    if isTestRuntimeWindows then
        Assert.Equal(fullPath,@"C:\dir1\test.txt")
    else
        Assert.Equal(fullPath,@"/dir/test.txt")

[<Fact>]
let ``fullIDtoFullPath handles windows long id file artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"dir2/subdir/test.txt"
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let fullPath = fullIDtoFullPath rootPath id1
    if isTestRuntimeWindows then
        Assert.Equal(fullPath,@"C:\dir1\dir2\subdir\test.txt")
    else
        Assert.Equal(fullPath,@"/dir1/dir2/subdir/test.txt")


[<Fact>]
let ``fullIDtoFullPath handles directory artefact`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"test/"
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let fullPath = fullIDtoFullPath rootPath id1
    if isTestRuntimeWindows then
        Assert.Equal(fullPath,@"C:\dir1\test\")
    else
        Assert.Equal(fullPath,@"/dir1/test/")

[<Fact>]
let ``fullIDtoFullPath checks that rootPath is fully qualified`` () =
    let id1:ArtefactFullID = ArtefactFullID.ID @"test/"
    let rootPath = Path.Combine(".","dir1")
    Assert.Throws<Exception>(fun () -> fullIDtoFullPath rootPath id1 |> ignore)
    

[<Fact>]
let ``fullIDtoRelative handles file artefact`` () =
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","file1.alph")
    let artefactFullID = ArtefactFullID.ID @"subrid1/text.txt"

    let relID = fullIDtoRelative rootPath alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID,@"subrid1/text.txt")

[<Fact>]
let ``fullIDtoRelative handles file artefact with parent dir refererence`` () =
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir2","file1.alph")
    let artefactFullID = ArtefactFullID.ID @"subrid1/text.txt"

    let relID = fullIDtoRelative rootPath alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID,@"../subrid1/text.txt")

[<Fact>]
let ``fullIDtoRelative handles directory artefact`` () =
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","file1.alph")
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"

    let relID = fullIDtoRelative rootPath alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID,@"subrid1/test/")

[<Fact>]
let ``fullIDtoRelative handles directory artefact with parent dir refererence`` () =
    let rootPath = Path.Combine( [|testRuntimeRootPath;"dir1"|] )
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir2","file1.alph")
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"

    let relID = fullIDtoRelative rootPath alphFileFullPath artefactFullID |> relIDtoString

    Assert.Equal(relID, @"../subrid1/test/")

[<Fact>]
let ``fullIDtoRelative checkes that alph file path is full path`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(".","file1.alph") // <-- this path is relative, that's the problem
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative rootPath alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that alph file path leads to dot-alph file`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","test.txt") // <-- here is not .alph file. Thats the problem
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative rootPath alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that rootPath is fully qualified path`` () =
    let rootPath = Path.Combine(".","dir1") // <-- here is relative path, thats the problem
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","test.txt.alph")
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative rootPath alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``fullIDtoRelative checkes that alph file is under the root path`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"test.txt.alph") // <- thats the problem, the .alph file is out of root path
    let artefactFullID = ArtefactFullID.ID @"subrid1/test/"
    Assert.Throws<Exception>(fun () -> fullIDtoRelative rootPath alphFileFullPath artefactFullID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that rootPath is fully qualified`` () =
    let rootPath = Path.Combine(".","dir1") // <-- here is relative path, thats the problem
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir","subdir1","file1.alph")
    let artefactRelID = RelativeArtefactID.ID @"../test/"
    Assert.Throws<Exception>(fun () -> relIDtoFullID rootPath alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that dot-alph file path is fully qualified`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(".","subdir1","file1.alph") // <-- here is relative path, thats the problem
    let artefactRelID = RelativeArtefactID.ID @"../test/"
    Assert.Throws<Exception>(fun () -> relIDtoFullID rootPath alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID checks that dot-alph file path is under the root path`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"subdir1","file1.alph") // <- thats the problem, the .alph file is out of root path
    let artefactRelID = RelativeArtefactID.ID @"../test/"
    Assert.Throws<Exception>(fun () -> relIDtoFullID rootPath alphFileFullPath artefactRelID |> ignore)

[<Fact>]
let ``relIDtoFullID handles file artefact`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir1","file1.alph")
    let artefactRelID = RelativeArtefactID.ID @"test.txt"
    let fullID = relIDtoFullID rootPath alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"subdir1/test.txt")

[<Fact>]
let ``relIDtoFullID handles directory artefact`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir1","file1.alph")
    let artefactRelID = RelativeArtefactID.ID @"test/"
    let fullID = relIDtoFullID rootPath alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"subdir1/test/")

[<Fact>]
let ``relIDtoFullID handles file artefact with parent folder reference`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir1","file1.alph")
    let artefactRelID = RelativeArtefactID.ID @"../test.txt"
    let fullID = relIDtoFullID rootPath alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"test.txt")

[<Fact>]
let ``relIDtoFullID handles directory artefact with parent folder reference`` () =
    let rootPath = Path.Combine(testRuntimeRootPath,"dir1")
    let alphFileFullPath = Path.Combine(testRuntimeRootPath,"dir1","subdir1","file1.alph")
    let artefactRelID = RelativeArtefactID.ID @"../test/"
    let fullID = relIDtoFullID rootPath alphFileFullPath artefactRelID |> fullIDtoString
    Assert.Equal(fullID, @"test/")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact`` () =
    let artefactPath = Path.Combine("dir1","test.txt")
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"dir1\test.txt.alph")
    else
        Assert.Equal(alphFile, @"dir1/test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact with parent relative reference`` () =
    let artefactPath = Path.Combine("..","test.txt")
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"..\test.txt.alph")
    else
        Assert.Equal(alphFile, @"../test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles file artefact with absolute path`` () =
    let artefactPath = Path.Combine(testRuntimeRootPath,"dir1","test.txt")
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"C:\dir1\test.txt.alph")
    else
        Assert.Equal(alphFile, @"/dir1/test.txt.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact`` () =
    let artefactPath = if isTestRuntimeWindows then @"dir1\test\" else @"dir1/test/"
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"dir1\test.alph")
    else
        Assert.Equal(alphFile, @"dir1/test.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact with parent relative reference`` () =
    let artefactPath = if isTestRuntimeWindows then @"..\test\" else @"../test/"
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"..\test.alph")
    else
        Assert.Equal(alphFile, @"../test.alph")

[<Fact>]
let ``artefactPathToAlphFilePath handles directory artefact with absolute path`` () =
    let artefactPath = if isTestRuntimeWindows then @"C:\dir1\test\" else @"/dir1/test/"
    let alphFile = artefactPathToAlphFilePath artefactPath
    if isTestRuntimeWindows then
        Assert.Equal(alphFile, @"C:\dir1\test.alph")
    else
        Assert.Equal(alphFile, @"/dir1/test.alph")
    