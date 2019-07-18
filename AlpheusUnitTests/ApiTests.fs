module ItisLab.Alpheus.Tests.ApiTests

open Xunit
open ItisLab.Alpheus.Tests
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.API
open System.Collections
open System.Collections.Generic
open ItisLab.Alpheus.AlphFiles

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

