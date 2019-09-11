namespace ItisLab.Alpheus.Tests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus
open Angara.Data

module AlphFileUtils = 
    let private assertEqual<'t when 't:equality> (a: 't) (b: 't) message =
        let equal = a = b
        Assert.True(equal, sprintf "%s not equal: expected: %A, actual %A" message a b)

    let private assertEqualVersion a b = 
        let va = MdMap.toSeq a |> Seq.toList
        let vb = MdMap.toSeq b |> Seq.toList
        let equal = List.fold (&&) true (List.zip va vb |> List.map (fun (u,v) -> u = v)) 
        Assert.True(equal, sprintf "Versions are not equal: expected: %A, actual %A" a b)

    let equal (a:AlphFile) (b:AlphFile) =
        assertEqual a.IsTracked b.IsTracked "IsTracked" 

        match a.Origin, b.Origin with
        | SourceOrigin sa, SourceOrigin sb ->
            assertEqual sa.RelativePath sb.RelativePath "Origin" 
            assertEqualVersion sa.Hash sb.Hash
        | CommandOrigin ca, CommandOrigin cb ->
            failwith "Not implemented"
        | _ -> failwith "Origins have different type"

type AlphFileSerializationTests(output)=
    inherit SingleUseOneTimeDirectory(output)

    [<Fact>]
    member s.``Alph file round serialization-deserialization for source origin``() =
        let artefactPath = Path.Combine(s.Path, "test.dat")
        let snapshotSection : VersionedArtefact = { RelativePath = artefactPath; Hash = MdMap.scalar (Some "0000") }
        let alphFile : AlphFile = {
            FileFormatVersion = Versioning.AlphFileCurrentVersion
            IsTracked = true
            Origin = DataOrigin.SourceOrigin snapshotSection
        }
        let path = Path.Combine(s.Path,"test.json")
        save alphFile path
        let loaded = tryLoad path
        match loaded with
        | Some loadedAlphFile ->
            AlphFileUtils.equal alphFile loadedAlphFile  
        | None ->
            Assert.True(false,"Failed to deserialized serialized file")

   
