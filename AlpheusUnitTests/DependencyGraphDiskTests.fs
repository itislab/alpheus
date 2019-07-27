module ItisLab.Alpheus.Tests.DependencyGraphDisk

open System.IO
open Xunit
open ItisLab.Alpheus
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.PathUtils
open Utils

type Tests(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Fact>]
    member s.``LoadDependenciesAsync loads single source file artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact s.FullArtIds.[0]
            let! artefactVertices = g.LoadDependenciesAsync [artefact] s.RootPath

            Assert.Equal(1,artefactVertices.Count)
            Assert.Equal(artefact,Seq.exactlyOne artefactVertices)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads single source dir artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact s.FullArtIds.[1]
            let! artefactVertices = g.LoadDependenciesAsync [artefact] s.RootPath

            Assert.Equal(1,artefactVertices.Count)
            Assert.Equal(artefact,Seq.exactlyOne artefactVertices)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads two source artefacts``() =
        async {
            let g = DependencyGraph.Graph()
            let artefactsIdsToTest = Array.take 2 s.FullArtIds
            let artefactToTest = Array.map g.GetOrAllocateArtefact artefactsIdsToTest

            let! resultingVertices = g.LoadDependenciesAsync (List.ofArray artefactToTest) s.RootPath

            Assert.Equal(2,resultingVertices.Count)
            Assert.Equal<DependencyGraph.ArtefactVertex>(Set.ofArray artefactToTest, resultingVertices)
        } |> toAsyncFact
    
    [<Fact>]
    member s.``LoadDependenciesAsync loads single computed file artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact s.FullArtIds.[2]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] s.RootPath

            Assert.Equal(3,resultingVertices.Count)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads single computed dir artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact s.FullArtIds.[3]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] s.RootPath

            Assert.Equal(3,resultingVertices.Count)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads full graph``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact s.FullArtIds.[4]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] s.RootPath

            let resultingIDs = Seq.map (fun (x:DependencyGraph.ArtefactVertex) -> x.Id) resultingVertices |> Set.ofSeq

            Assert.Equal(5,resultingVertices.Count)
            Assert.Equal<ArtefactId>(resultingIDs,Set.ofSeq s.FullArtIds)
        } |> toAsyncFact

