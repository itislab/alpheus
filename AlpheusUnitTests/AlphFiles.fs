module ItisLab.Alpheus.Tests.AlphFiles

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.AlphFiles

type AlphFileSerializationTests()=
    inherit SingleUseOneTimeDirectory()

    [<Fact>]
    member s.``Alph file round serialization-deserialization``() =
        async {
            let snapshortSection : SnapshotSection = {Type= ArtefactType.FileArtefact ; Version = "0000"}
            let alphFile : AlphFile = {
                IsTracked = true
                Origin = DataOrigin.Snapshot snapshortSection
            }
            let path = Path.Combine(s.Path,"test.json")
            do! saveAsync alphFile path
            let! loaded = tryLoadAsync path
            match loaded with
            |   Some loadedAlphFile ->
                Assert.Equal(alphFile,loadedAlphFile)
            |   None ->
                Assert.True(false,"Failed to deserialized serialized file")
        } |> toAsyncFact

    [<Fact>]
    member s.``alphFilePathToArtefactPathAsync handles file artefact``() =
        async {
            let snapshortSection : SnapshotSection = {Type= ArtefactType.FileArtefact ; Version = "0000"}
            let fileArtefactAlphFile : AlphFile = {
                IsTracked = true
                Origin = DataOrigin.Snapshot snapshortSection
            }
            let path = Path.Combine(s.Path,"test.txt.alph")
            do! saveAsync fileArtefactAlphFile path
            
            let! loaded = alphFilePathToArtefactPathAsync path
            Assert.Equal(loaded,Path.Combine(s.Path,"test.txt"))

        } |> toAsyncFact

    [<Fact>]
    member s.``alphFilePathToArtefactPathAsync handles directory artefact``() =
        async {
            let snapshortSection : SnapshotSection = {Type= ArtefactType.DirectoryArtefact ; Version = "0000"}
            let fileArtefactAlphFile : AlphFile = {
                IsTracked = true
                Origin = DataOrigin.Snapshot snapshortSection
            }
            let path = Path.Combine(s.Path,"test.alph")
            do! saveAsync fileArtefactAlphFile path
            
            let! loaded = alphFilePathToArtefactPathAsync path
            Assert.Equal(loaded,Path.Combine(s.Path,@"test\"))
        } |> toAsyncFact

