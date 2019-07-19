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

/// can be used as calssData for XUnit theory. returns all of the artefactIDs for the sample experiment
type ArtefactIdSource() =
    let sampleExperiment = new SampleExperiment.SampleExperiment()
    let sequence : IEnumerable<obj array> = sampleExperiment.FullArtIds |> Seq.map (fun x -> x :> obj) |> Seq.map (fun x -> [|x|])
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
            yield [| {inputsIndices=[|3|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
        }
    interface IEnumerable<obj array> with
        member s.GetEnumerator() =
            sequence.GetEnumerator()

    interface IEnumerable with
        member s.GetEnumerator() =
            sequence.GetEnumerator() :> IEnumerator            

type DepGraphConstruction() =
    inherit SampleExperiment.SampleExperiment()

    [<Theory>]
    [<ClassData(typedefof<ArtefactIdSource>)>]
    member s.``dependencyGraph loads``(artefactId:ArtefactId) =
        async {
            traceInfo (sprintf "testing %A" artefactId)
            let! graph = buildDependencyGraphAsync s.RootPath artefactId
            Assert.True(graph.ArtefactsCount>0,"Graph must be non-empty")
            } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Ensures that build command finishes successfuly for all test cases
    member s.``build: finishes``(testCase) =
        async {
            // preparing and eunning build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.FullArtIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            do! buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks the artefact and method counts in the graph after the build command for all test cases
    member s.``build: vertex count check``(testCase) =
        async {
            // preparing and eunning build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.FullArtIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            do! buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false

            //let alphFiles = List.map artefactPathToAlphFilePath outputPaths
            // checking grpah for each newly created output artefact
            let checkGraphAsync outputId expectedMethodCount expectedArtefactCount = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath outputId
                    Assert.Equal(expectedMethodCount,graph.MethodsCount)
                    Assert.Equal(expectedArtefactCount,graph.ArtefactsCount)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.ID x) testCase.ExpectedMethodsInGraph testCase.ExpectedArtefactsInGraph)
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks that newly built artefact vertices have expected inputs
    member s.``build: new artefact deps check``(testCase) =
        async {
            // preparing and eunning build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.FullArtIds.[idx]) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x.ToString()))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            do! buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false

            // checking graph for each newly created output artefact
            let checkGraphAsync outputId = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath outputId
                    let output = graph.GetOrAllocateArtefact outputId
                    let expectedDependecies = inputIDs |> Set.ofList
                    let actualDependecies =
                        match output.ProducedBy with
                        |   MethodVertex.Source(s) -> Assert.True(false, "the artefact must be produced by compute method, not source"); failwith ":("
                        |   MethodVertex.Command(comp) -> comp.Inputs |> Seq.map (fun (x:VersionedArtefact) -> x.Artefact.Id) |> Set.ofSeq
                    Assert.Equal<ArtefactId>(expectedDependecies, actualDependecies)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.ID x))
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

