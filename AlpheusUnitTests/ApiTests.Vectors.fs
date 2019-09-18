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
open FluentAssertions

type ``Vector scenarios``(output) =
    inherit SingleUseOneTimeDirectory(output)

    let concatCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"copy $in1 + $in2 $out1 /b\""
        else
            "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""

    let concatVectorCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"copy $in1 $out1 /b\""
        else
            "todo"

    let prepareSources(path) =
        async {
            let path = Path.GetFullPath path
            let! _ = API.createExperimentDirectory path
            Directory.CreateDirectory(Path.Combine(path, "data")) |> ignore
            File.WriteAllText(Path.Combine(path, "base.txt"), "Base file") 
            File.WriteAllText(Path.Combine(path, "data", "1.txt"), "File 1")
            File.WriteAllText(Path.Combine(path, "data", "2.txt"), "File 2")
            File.WriteAllText(Path.Combine(path, "data", "3.txt"), "File 3")

            return ()
        }

    let assertFileContent path content =
        let actualContent = File.ReadAllText path
        actualContent.Should().Be(content, sprintf "it is a content of the file %s" path) |> ignore


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

                "Base fileFile 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
                "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
                "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

                // Checks the output alph file:
                let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
                match alph.Origin with 
                | DataOrigin.CommandOrigin cmd ->
                    cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                    (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                    (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                | _ -> failwith "Unexpected origin"
            finally
                Environment.CurrentDirectory <- savedWD      
        }

    [<Fact>]
    member s.``Runs two methods one after another for multiple input files``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try   
                let root = s.ExperimentRoot
                Environment.CurrentDirectory <- root
                do! prepareSources(root)

                let! res = API.buildAsync root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand false
                assertResultOk res

                let! res = API.buildAsync root ["base.txt"; "output/out*.txt"] ["output2/*.txt"] concatCommand false
                assertResultOk res

                let outputId = ArtefactId.Path "output/out*.txt"
                let output2Id = ArtefactId.Path "output2/*.txt"
                let res = API.compute (root, output2Id)
                assertResultOk res

                "Base fileBase fileFile 1" |> assertFileContent (Path.Combine(root, "output2", "1.txt"))
                "Base fileBase fileFile 2" |> assertFileContent (Path.Combine(root, "output2", "2.txt"))
                "Base fileBase fileFile 3" |> assertFileContent (Path.Combine(root, "output2", "3.txt"))

                // Checks the output alph file:
                let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
                match alph.Origin with 
                | DataOrigin.CommandOrigin cmd ->
                    cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                    (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                    (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                | _ -> failwith "Unexpected origin"

                let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root output2Id) |> Option.get
                match alph.Origin with 
                | DataOrigin.CommandOrigin cmd ->
                    cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                    (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                    (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                | _ -> failwith "Unexpected origin"
            finally
                Environment.CurrentDirectory <- savedWD      
        } 

    [<Fact>]
    member s.``Aggregates results of a vector operation``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try   
                let root = s.ExperimentRoot
                Environment.CurrentDirectory <- root
                do! prepareSources(root)

                let! res = API.buildAsync root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand false
                assertResultOk res

                let! res = API.buildAsync root ["output/out*.txt"] ["summary.txt"] concatVectorCommand false
                assertResultOk res

                let summaryId = ArtefactId.Path "summary.txt"
                let res = API.compute (root, summaryId)
                assertResultOk res

                //"Base fileBase fileFile 1" |> assertFileContent (Path.Combine(root, "output2", "1.txt"))
                //"Base fileBase fileFile 2" |> assertFileContent (Path.Combine(root, "output2", "2.txt"))
                //"Base fileBase fileFile 3" |> assertFileContent (Path.Combine(root, "output2", "3.txt"))

                //// Checks the output alph file:
                //let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
                //match alph.Origin with 
                //| DataOrigin.CommandOrigin cmd ->
                //    cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                //    (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                //    (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                //| _ -> failwith "Unexpected origin"

                //let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root output2Id) |> Option.get
                //match alph.Origin with 
                //| DataOrigin.CommandOrigin cmd ->
                //    cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                //    (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                //    (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                //| _ -> failwith "Unexpected origin"
            finally
                Environment.CurrentDirectory <- savedWD      
        } 

   