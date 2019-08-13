namespace ItisLab.Alpheus.Tests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus
open Angara.Data

type AlphFileSerializationTests(output)=
    inherit SingleUseOneTimeDirectory(output)

    [<Fact>]
    member s.``Alph file round serialization-deserialization``() =
        async {
            let artefactPath = Path.Combine(s.Path, "test.dat")
            let snapshotSection : VersionedArtefact = { RelativePath = artefactPath; Hash = MdMap.scalar (Some "0000") }
            let alphFile : AlphFile = {
                IsTracked = true
                Origin = DataOrigin.SourceOrigin snapshotSection
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

   
