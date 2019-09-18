namespace ItisLab.Alpheus.Tests

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
open Angara.Data

type ``Vector scenarios through API``(output) =
    inherit SingleUseOneTimeDirectory(output)

    let concatCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"copy $in1 /A + $in2 /A $out1 /A\""
        else
            "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""



    let prepareSources(path) =
        async {
            let path = Path.GetFullPath path
            let! _ = API.createExperimentDirectory path
            Directory.CreateDirectory(Path.Combine(path, "data")) |> ignore
            File.WriteAllText(Path.Combine(path, "base.txt"), "Base file\\r\\n") 
            File.WriteAllText(Path.Combine(path, "data", "1.txt"), "File 1\\r\\n")
            File.WriteAllText(Path.Combine(path, "data", "2.txt"), "File 2\\r\\n")
            File.WriteAllText(Path.Combine(path, "data", "3.txt"), "File 3\\r\\n")

            return ()
        }

    let assertNonEmptyFile path = 
        async {
            let! content = File.ReadAllTextAsync(path) |> Async.AwaitTask
            Assert.True(content.Length > 0)
        }

    [<Fact>]
    member s.``Runs same method for multiple input files``() =
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand false
            assertResultOk res

            let outputId = ArtefactId.Path "output/out*.txt"
            let res = API.compute (root, outputId)
            assertResultOk res

            do! assertNonEmptyFile(Path.Combine(root, "output", "out1.txt"))
            do! assertNonEmptyFile(Path.Combine(root, "output", "out2.txt"))
            do! assertNonEmptyFile(Path.Combine(root, "output", "out3.txt"))

            // Checks the output alph file:
            let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
            match alph.Origin with 
            | DataOrigin.CommandOrigin cmd ->
                Assert.True(cmd.Inputs.[0].Hash.IsScalar, "base.txt is scalar")
                Assert.Equal(3, cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length)
                Assert.Equal(3, cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length)
            | _ -> failwith "Unexpected origin"
        }

   