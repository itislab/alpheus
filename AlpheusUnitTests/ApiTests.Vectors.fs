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
            let! _ = API.createExperimentDirectoryAsync path
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
            let savedWD = Environment.CurrentDirectory
            try            
                let root = s.FullPath
                Environment.CurrentDirectory <- root
                do! prepareSources(root)

                let! res = API.buildAsync root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand false
                assertResultOk res

                let res = API.compute (root, ArtefactId.Path "output/*.txt")
                assertResultOk res

                do! assertNonEmptyFile(Path.Combine(root, "output", "base_1.txt"))
                do! assertNonEmptyFile(Path.Combine(root, "output", "base_2.txt"))
                do! assertNonEmptyFile(Path.Combine(root, "output", "base_3.txt"))
            finally
                Environment.CurrentDirectory <- savedWD            
        } |> toAsyncFact

   