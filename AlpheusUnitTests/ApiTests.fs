module ItisLab.Alpheus.Tests.ApiTests

open Xunit
open ItisLab.Alpheus.Tests
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.API
open System.Collections
open System.Collections.Generic
open ItisLab.Alpheus.AlphFiles
open System.IO
open ItisLab.Alpheus.DependencyGraph
open System
open ItisLab.Alpheus

/// can be used as calssData for XUnit theory. returns all of the artefactIDs for the sample experiment
type ArtefactIdSource() =
    let sequence : IEnumerable<obj array> = SampleExperiment.artIdsStr |> Seq.map ArtefactId.Path |> Seq.map (fun x -> x :> obj) |> Seq.map (fun x -> [|x|])
    interface IEnumerable<obj array> with
        member s.GetEnumerator() =
            sequence.GetEnumerator()

    interface IEnumerable with
        member s.GetEnumerator() =
            sequence.GetEnumerator() :> IEnumerator

type BuildCommandTestCase =
    {
        inputsIndices: int array // an index inside "FullArtIds" array of SampleExperiment type
        OutputIDs: string array // an output artefact path
        ExpectedArtefactsInGraph: int // after adding artefact at "Path" and trying to build the dep graph
        ExpectedMethodsInGraph: int // after adding artefact at "Path" and trying to build the dep graph
    }

type GraphBuildDataSource() =
    let sequence : IEnumerable<obj array> = 
        seq {
            yield [| {inputsIndices=[|0|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|1|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|2|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|3|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|4|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 6; ExpectedMethodsInGraph = 6} |]
            yield [| {inputsIndices=[|0|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|1|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|2|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|3|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|4|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 6; ExpectedMethodsInGraph = 6} |]
        }
    interface IEnumerable<obj array> with
        member s.GetEnumerator() =
            sequence.GetEnumerator()

    interface IEnumerable with
        member s.GetEnumerator() =
            sequence.GetEnumerator() :> IEnumerator            

type DepGraphConstruction(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Theory>]
    [<ClassData(typedefof<ArtefactIdSource>)>]
    member s.``dependencyGraph loads for separate artefacts``(artefactId:ArtefactId) =
        async {
            Logger.logInfo Logger.Test (sprintf "testing %A" artefactId)
            let! graph = buildDependencyGraphAsync s.RootPath [artefactId]
            Assert.True(graph.ArtefactsCount>0,"Graph must be non-empty")
            } |> toAsyncFact

    [<Fact>]
    member s.``dependencyGraph loads for all at once``() =
        async {
            Logger.logInfo Logger.Test (sprintf "testing %A" s.ArtefactIds)
            let! graph = buildDependencyGraphAsync s.RootPath (List.ofArray s.ArtefactIds)
            Assert.True(graph.ArtefactsCount>0,"Graph must be non-empty")
            } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Ensures that build command finishes successfully for all test cases
    member s.``build: finishes``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks the artefact and method counts in the graph after the build command for all test cases
    member s.``build: vertex count check``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult

            //let alphFiles = List.map artefactPathToAlphFilePath outputPaths
            // checking grpah for each newly created output artefact
            let checkGraphAsync outputId expectedMethodCount expectedArtefactCount = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath [outputId]
                    Assert.Equal(expectedMethodCount,graph.MethodsCount)
                    Assert.Equal(expectedArtefactCount,graph.ArtefactsCount)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.Path x) testCase.ExpectedMethodsInGraph testCase.ExpectedArtefactsInGraph)
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks that newly built artefact vertices have expected inputs
    member s.``build: new artefact deps check``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx]) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x.ToString()))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult
            Logger.logInfo Logger.Test "Graph is build via API call successfuly"

            // checking graph for each newly created output artefact
            let checkGraphAsync outputId = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath [outputId]
                    let output = graph.Artefacts |> Seq.find (fun a -> a.Id = outputId)
                    let expectedDependecies = inputIDs |> Set.ofList
                    let actualDependecies =
                        match output.ProducedBy with
                        |   MethodVertex.Source(s) -> Assert.True(false, "the artefact must be produced by compute method, not source"); failwith ":("
                        |   MethodVertex.Command(comp) -> comp.Inputs |> Seq.map (fun x -> x.Artefact.Id) |> Set.ofSeq
                    Assert.Equal<ArtefactId>(expectedDependecies, actualDependecies)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.Path x))
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

type DepGraphLocalComputation(output) =
    inherit SampleExperiment.SampleExperiment(output)

    
    [<Fact>]
    member s.``API Compute: concat 2 files``() =
        let expRoot = s.FullPath
        let savedWD = Environment.CurrentDirectory
        try
            // we will concat these files
            File.Copy("data/cat.cmd",Path.Combine(s.Path,"cat.cmd"))
            File.Copy("data/texturalData.txt",Path.Combine(s.Path,"1.txt"))
            File.Copy("data/texturalData2.txt",Path.Combine(s.Path,"2.txt"))
        
            // setting proper working directory
            Environment.CurrentDirectory <- expRoot
            
            let concatCommand = 
                if isTestRuntimeWindows then
                    "cmd /C \"cat.cmd $out1 $in1 $in2\""
                else
                    "/bin/sh -c \"cat 1.txt > cat_test.txt; cat 2.txt >> cat_test.txt\""
            let buildResult = API.buildAsync expRoot ["1.txt"; "2.txt"] ["cat_test.txt"] concatCommand false |> Async.RunSynchronously
            assertResultOk buildResult

            // graph is constructed. Now executing
            printfn "Graph constructed"

            Assert.False(File.Exists("cat_test.txt"))
            let computeResult = result {
                let! target = API.artefactFor "cat_test.txt"
                return! API.compute target
            }
            assertResultOk computeResult
            Assert.True(File.Exists("cat_test.txt"))

            let f1 = File.ReadAllText("1.txt")
            let f2 = File.ReadAllText("2.txt")
            let f3 = File.ReadAllText("cat_test.txt")

            // And verifying the content
            let len = (f1+f2).Length
            Assert.Equal(f1+f2,f3.Substring(0,len)) // constraining the length as windows copy command writes strange SUB symbol et the end of the file

        finally
            Environment.CurrentDirectory <- savedWD



type DepGraphSaveRestore(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Fact>]
    member s.``API Save-Restore: file artefact saves & restores``() =
        async {
            let artId = s.ArtefactIds.[0]
            let path = artId |> PathUtils.idToFullPath s.FullPath

            // local storage is available by default
            let! saveResult =  API.saveAsync (s.FullPath, artId) "local" false
            assertResultOk saveResult
             
            // saving the content for later verification
            let! origContent = File.ReadAllTextAsync(path) |> Async.AwaitTask

            // deleting the file to restore it
            File.Delete(path)

            Assert.False(File.Exists(path))

            let! restoreResult = API.restoreAsync (s.FullPath, artId)
            assertResultOk restoreResult

            // checking that it actually restored and it's content matches
            Assert.True(File.Exists(path),"File is not restored")
            let! restoredContent = File.ReadAllTextAsync(path) |> Async.AwaitTask
            Assert.Equal(origContent,restoredContent)

        } |> toAsyncFact
    
    [<Theory>] // gitgnore must always hold the entries with straight slashes (even on windows)
    [<InlineData(@"dir1/test2/")>]
    [<InlineData(@"dir2/test3.txt")>]
    [<InlineData(@"dir1/test1.txt")>] // alph file is missing for this artefact
    [<InlineData(@"dir3/dir5/test5.txt")>]
    member s.``API Save: gitignore entry is added for single file``(artIdStr) =
        async {
            let artefactId = ArtefactId.Path artIdStr
            // local storage is available by default
            let! saveResult =  API.saveAsync (s.FullPath, artefactId) "local" false
            assertResultOk saveResult

            let gitIgnorePath = Path.Combine(s.Path,".gitignore")

            Assert.True(File.Exists(gitIgnorePath), ".gitignore file must be created")

            let! entries = File.ReadAllLinesAsync gitIgnorePath |> Async.AwaitTask
            Assert.True(Seq.exists (fun s -> s = artIdStr) entries, ".gitignore must contain newly added artefact record")

        } |> toAsyncFact