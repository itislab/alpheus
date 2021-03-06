﻿namespace ItisLab.Alpheus.Tests

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

type ``Vector scenarios``(output) as this =
    inherit SingleUseOneTimeDirectory(output)

    let createManyFilesCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"FOR %i IN (1,2,3) DO (echo sample%i > sample%i.txt)\""
        else
            "/bin/sh -c \"for i in $(seq 1 3); do echo sample$i > sample$i.txt; done\""

            
    let createManyFilesWithInputCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"CD $in1 & FOR %i IN (1,2,3) DO (echo sample%i > sample%i.txt)\""
        else
            "/bin/sh -c \"cd $in1; for i in $(seq 1 3); do echo sample$i > sample$i.txt; done\""

    let createManyFoldersCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"FOR %i IN (1,2,3) DO (mkdir sample%i)\""
        else
            "/bin/sh -c \"for i in $(seq 1 3); do mkdir sample$i; done\""

    let concatCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"copy $in1 + $in2 $out1 /b\""
        else
            "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""
    
    /// enumerate all files in $in1 dir and concat them into single file $out1
    let concatVectorCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"copy $in1 $out1 /b\"" // this one looks like invalid, but id does the correct thing!
        else
            "/bin/sh -c \"for file in $in1; do echo $file; cat $file >> $out1; done;\""

    let concatVectorForSummariesVectorCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"type NUL > $out1 && for /D %d in (*) DO copy \"$out1\" + \"%d\summary.txt\" \"$out1\" /b\""
        else
            "/bin/sh -c \"for file in $in1; do echo $file; cat $file >> $out1; done;\""

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

    let concatStrings (strings: string seq) =
        match this.Platform with
        | TargetPlatform.Windows -> strings |> Seq.map(fun s -> s + " \r\n") |> String.concat ""
        | TargetPlatform.Linux -> strings |> Seq.map(fun s -> s + "\n") |> String.concat ""
        | _ -> failwith "Unknown platform"


    [<Fact>]
    member s.``Runs same method for multiple input files``() =
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
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
        }

    [<Fact>]
    member s.``Runs same method for shrinked index``() =
        // also see issue #89
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/out*.txt"
            let res = API.compute (root, outputId)
            assertResultOk res

            "Base fileFile 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
            "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
            "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

            Logger.logInfo Logger.Test "Deleting data/3.txt"
            // shrinking input index: instead of 3 files in data, we will retain only 2
            File.Delete(Path.Combine(root,"data","3.txt"))

            //running again
            Logger.logInfo Logger.Test "computing output/out*.txt again"
            let res = API.compute (root, outputId)
            assertResultOk res

            Assert.False(File.Exists(Path.Combine(root,"output","out3.txt")))

            // Checks the output alph file:
            let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
            match alph.Origin with 
            | DataOrigin.CommandOrigin cmd ->
                cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(2, "2nd input is a vector of 2 elements") |> ignore
                (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(2, "The output is a vector of 2 elements") |> ignore
            | _ -> failwith "Unexpected origin"
        }

    [<Fact>]
    member s.``Runs same method for changed index``() =
        // also see issue #89
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/out*.txt"
            let res = API.compute (root, outputId)
            assertResultOk res

            "Base fileFile 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
            "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
            "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

            Logger.logInfo Logger.Test "Renaming data/2.txt into data/4.txt"
            File.Move(Path.Combine(root,"data","2.txt"),Path.Combine(root,"data","4.txt"))
            
            //running again
            Logger.logInfo Logger.Test "computing output/out*.txt again"
            let res = API.compute (root, outputId)
            assertResultOk res

            Assert.False(File.Exists(Path.Combine(root,"output","out2.txt")))
            Assert.True(File.Exists(Path.Combine(root,"output","out4.txt")))

            // Checks the output alph file:
            let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root outputId) |> Option.get
            match alph.Origin with 
            | DataOrigin.CommandOrigin cmd ->
                cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
                Assert.True(cmd.Outputs.[0].Hash |> MdMap.toSeq |> Seq.exists (fun x -> let k,_ = x in k = ["4"]))
                Assert.False(cmd.Outputs.[0].Hash |> MdMap.toSeq |> Seq.exists (fun x -> let k,_ = x in k = ["2"]))
            | _ -> failwith "Unexpected origin"
        }

    [<Fact>]
    member s.``Does not issue vector elements delete if currently no vector source elems on disk``() =
        // consider the situation
        // I added source vector artefact
        // I saved it to storage
        // I cloned the repo on another machine
        // I run the experiment
        // Expected: the system download best known saved vector indices to disk
        // Possible wrong behavior: the system considers the vector as empty (as nothing on disk matches the * pattern)
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let dataId = ArtefactId.Path "data/*.txt"
            let outputId = ArtefactId.Path "output/out*.txt"
            let res = API.compute (root, outputId)
            assertResultOk res

            "Base fileFile 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
            "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
            "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

            Logger.logInfo Logger.Test "saving data/out*.txt"

            let! res = API.saveAsync (root,dataId) None false
            assertResultOk res
            
            Logger.logInfo Logger.Test "Deleting data/*.txt and output/out*.txt"
            File.Delete(Path.Combine(root,"data","1.txt"))
            File.Delete(Path.Combine(root,"data","2.txt"))
            File.Delete(Path.Combine(root,"data","3.txt"))
            File.Delete(Path.Combine(root,"output","out1.txt"))
            File.Delete(Path.Combine(root,"output","out2.txt"))
            File.Delete(Path.Combine(root,"output","out3.txt"))
            
            //running again
            Logger.logInfo Logger.Test "computing output/out*.txt again. data/*.txt restore must happen with previously existed indices [1;2;3]"
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
                Assert.True(cmd.Outputs.[0].Hash |> MdMap.toSeq |> Seq.exists (fun x -> let k,_ = x in k = ["1"]))
                Assert.True(cmd.Outputs.[0].Hash |> MdMap.toSeq |> Seq.exists (fun x -> let k,_ = x in k = ["2"]))
                Assert.True(cmd.Outputs.[0].Hash |> MdMap.toSeq |> Seq.exists (fun x -> let k,_ = x in k = ["3"]))
            | _ -> failwith "Unexpected origin"
        }

    [<Fact>]
    member s.``Re-Runs same method upon input changes``() =
        async {
            let root = s.ExperimentRoot 
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/out*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/out*.txt"
            let res = API.compute (root, outputId)
            assertResultOk res

            "Base fileFile 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
            "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
            "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

            do! File.WriteAllTextAsync(Path.Combine(root, "data", "1.txt"),"Changed File 1") |> Async.AwaitTask

            let res = API.compute (root, outputId)
            assertResultOk res

            "Base fileChanged File 1" |> assertFileContent (Path.Combine(root, "output", "out1.txt"))
            "Base fileFile 2" |> assertFileContent (Path.Combine(root, "output", "out2.txt"))
            "Base fileFile 3" |> assertFileContent (Path.Combine(root, "output", "out3.txt"))

        }

    [<Fact>]
    member s.``Runs two methods one after another for multiple input files``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let! res = API.buildAsync root root ["base.txt"; "output/*.txt"] ["output2/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/*.txt"
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
        }

    [<Fact>]
    /// also tests the issue #85
    member s.``up-to-date remote are not restored if not needed``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let! res = API.buildAsync root root ["base.txt"; "output/*.txt"] ["output2/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let! res = API.buildAsync root root ["base.txt"; "output2/*.txt"] ["output3/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/*.txt"
            let output2Id = ArtefactId.Path "output2/*.txt"
            let output3Id = ArtefactId.Path "output3/*.txt"
            API.compute (root, output3Id) |> assertResultOk
            
            Logger.logInfo Logger.Test "Chain computed first time"

            // now saving "output/*.txt"
            let! saveRes = API.saveAsync (root,outputId) None false
            assertResultOk saveRes
            Logger.logInfo Logger.Test "output/*.txt saved to storage"
            
            // deleting "output3/*.txt" and "output/*.txt"
            File.Delete <| Path.Combine(root, "output", "1.txt")
            File.Delete <| Path.Combine(root, "output", "2.txt")
            File.Delete <| Path.Combine(root, "output", "3.txt")
            File.Delete <| Path.Combine(root, "output3", "1.txt")
            File.Delete <| Path.Combine(root, "output3", "2.txt")
            File.Delete <| Path.Combine(root, "output3", "3.txt")

            Logger.logInfo Logger.Test "Deleted output/*.txt and output3/*.txt"

            Logger.logInfo Logger.Test "Test begins. Second time compute..."
            API.compute (root, output3Id) |> assertResultOk
            Logger.logInfo Logger.Test "Second time computed"

            Assert.False(File.Exists(Path.Combine(root, "output", "1.txt")))
            Assert.False(File.Exists(Path.Combine(root, "output", "2.txt")))
            Assert.False(File.Exists(Path.Combine(root, "output", "3.txt")))
            

            "Base fileBase fileBase fileFile 1" |> assertFileContent (Path.Combine(root, "output3", "1.txt"))
            "Base fileBase fileBase fileFile 2" |> assertFileContent (Path.Combine(root, "output3", "2.txt"))
            "Base fileBase fileBase fileFile 3" |> assertFileContent (Path.Combine(root, "output3", "3.txt"))

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

            let alph = AlphFiles.tryLoad (PathUtils.idToAlphFileFullPath root output3Id) |> Option.get
            match alph.Origin with 
            | DataOrigin.CommandOrigin cmd ->
                cmd.Inputs.[0].Hash.IsScalar.Should().BeTrue("base.txt is scalar") |> ignore
                (cmd.Inputs.[1].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "2nd input is a vector of 3 elements") |> ignore
                (cmd.Outputs.[0].Hash |> MdMap.toShallowSeq |> Seq.length).Should().Be(3, "The output is a vector of 3 elements") |> ignore
            | _ -> failwith "Unexpected origin"
        }

    [<Fact>]
    /// also triggers issue #78
    member s.``Vector elements are restored from storage during comupute if needed``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let! res = API.buildAsync root root ["base.txt"; "output/*.txt"] ["output2/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let outputId = ArtefactId.Path "output/*.txt"
            let output2Id = ArtefactId.Path "output2/*.txt"
            API.compute (root, output2Id) |> assertResultOk
            
            Logger.logInfo Logger.Test "First time computed"

            // now saving "output/*.txt"
            let! saveRes = API.saveAsync (root,outputId) None false
            assertResultOk saveRes

            Logger.logInfo Logger.Test "output/*.txt saved to storage"
            
            // deleting "output2/*.txt" and "output/*.txt"
            File.Delete <| Path.Combine(root, "output", "1.txt")
            File.Delete <| Path.Combine(root, "output", "2.txt")
            File.Delete <| Path.Combine(root, "output", "3.txt")
            File.Delete <| Path.Combine(root, "output2", "1.txt")
            File.Delete <| Path.Combine(root, "output2", "2.txt")
            File.Delete <| Path.Combine(root, "output2", "3.txt")

            Logger.logInfo Logger.Test "Deleted output/*.txt and output2/*.txt"

            // now asking again to compute
            Logger.logInfo Logger.Test "Now, restore should happen"
            API.compute (root, output2Id) |> assertResultOk

            Logger.logInfo Logger.Test "Second time computed"

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
        }

    [<Fact>]
    member s.``Resource groups work``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let settings = {
                CommandExecutionSettings.Default with
                    ResourceGroups = Set.add "res1" CommandExecutionSettings.Default.ResourceGroups
            }

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand settings
            assertResultOk res

            let! res = API.buildAsync root root ["base.txt"; "output/*.txt"] ["output2/*.txt"] concatCommand settings
            assertResultOk res

            let outputId = ArtefactId.Path "output/*.txt"
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
        } 

    [<Fact>]
    member s.``Scatter: Creates many files and the processes them as a vector``() =
        async {
            let root = s.ExperimentRoot
            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*.txt"] createManyFilesCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let res = API.compute (root, Path "samples/*.txt")
            assertResultOk res

            ["sample1"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample1.txt"))
            ["sample2"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample2.txt"))
            ["sample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample3.txt"))
        }

    [<Fact>]
    member s.``Reduce: Aggregates results of a vector operation``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            // this one is gather
            let! res = API.buildAsync root root ["output/*.txt"] ["summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res
            
            "Base fileFile 1Base fileFile 2Base fileFile 3" |> assertFileContent (Path.Combine(root, "summary.txt"))
        }
    
    [<Fact>]
    member s.``Issue 96: Reduce operation fails if the index from prev run does not exist on disk``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root root ["base.txt"; "data/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            // this one is gather
            let! res = API.buildAsync root root ["output/*.txt"] ["summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res

            // now removing index 2
            File.Delete(Path.Combine(root,"data","2.txt"))

            let res = API.compute (root, summaryId)
            assertResultOk res

                        
            "Base fileFile 1Base fileFile 3" |> assertFileContent (Path.Combine(root, "summary.txt"))
        }

    [<Fact>]
    member s.``Scatter-reduce: Creates many files and then reduces``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*.txt"] createManyFilesCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["base.txt"; "samples/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["output/*.txt"] ["summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res
                        
            ["Base filesample1"; "Base filesample2"; "Base filesample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "summary.txt"))
        }

    [<Fact>]
    member s.``Scatter-vector-reduce: Creates many files and the processes them as a vector and then reduces``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*.txt"] createManyFilesCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["base.txt"; "samples/*.txt"] ["output/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["samples/*.txt"; "output/*.txt"] ["output2/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["output2/*.txt"] ["summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res
                        
            ["sample1"; "Base filesample1"; "sample2"; "Base filesample2"; "sample3"; "Base filesample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "summary.txt"))
        }

    [<Fact>]
    member s.``Scatter 2d``() =
        async {
            let root = s.ExperimentRoot
            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*/"] createManyFoldersCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root (Path.Combine(root, "samples")) ["*/"] ["*/*.txt"] createManyFilesWithInputCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let res = API.compute (root, Path "samples/*/*.txt")
            assertResultOk res


            for i in 1..3 do            
                ["sample1"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", sprintf "sample%d" i, "sample1.txt"))
                ["sample2"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", sprintf "sample%d" i, "sample2.txt"))
                ["sample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", sprintf "sample%d" i, "sample3.txt"))
        }

    [<Fact>]
    member s.``Scatter-scatter``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let root = s.ExperimentRoot
            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*/"] createManyFoldersCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root (Path.Combine(root, "samples")) ["*/"] ["*/*.txt"] createManyFilesWithInputCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["base.txt"; "samples/*/*.txt"] ["output/*/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "output/*/*.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res

            for i in 1..3 do            
                ["Base filesample1"] |> concatStrings |> assertFileContent (Path.Combine(root, "output", sprintf "sample%d" i, "sample1.txt"))
                ["Base filesample2"] |> concatStrings |> assertFileContent (Path.Combine(root, "output", sprintf "sample%d" i, "sample2.txt"))
                ["Base filesample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "output", sprintf "sample%d" i, "sample3.txt"))
        }

    [<Fact>]
    member s.``Scatter-scatter-reduce``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let root = s.ExperimentRoot
            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*/"] createManyFoldersCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root (Path.Combine(root, "samples")) ["*/"] ["*/*.txt"] createManyFilesWithInputCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["base.txt"; "samples/*/*.txt"] ["output/*/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let! res = API.buildAsync root root ["output/*/*.txt"] ["summaries/*/summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let summaryId = ArtefactId.Path "summaries/*/summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res
                        
            for i in 1..3 do   
                ["Base filesample1"; "Base filesample2"; "Base filesample3"] 
                |> concatStrings |> assertFileContent (Path.Combine(root, "summaries", sprintf "sample%d" i, "summary.txt"))
        }

    [<Fact>]
    member s.``Scatter-scatter-vector-reduce-reduce``() =
        async {
            let root = s.ExperimentRoot
            do! prepareSources(root)

            let root = s.ExperimentRoot
            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*/"] createManyFoldersCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root (Path.Combine(root, "samples")) ["*/"] ["*/*.txt"] createManyFilesWithInputCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["base.txt"; "samples/*/*.txt"] ["output/*/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["samples/*/*.txt"; "output/*/*.txt"] ["output2/*/*.txt"] concatCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root root ["output2/*/*.txt"] ["summaries/*/summary.txt"] concatVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res
            let! res = API.buildAsync root (Path.Combine(root, "summaries")) ["*/summary.txt"] ["../summary.txt"] concatVectorForSummariesVectorCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res


            let summaryId = ArtefactId.Path "summary.txt"
            let res = API.compute (root, summaryId)
            assertResultOk res
                        
            ["sample1"; "Base filesample1"; "sample2"; "Base filesample2"; "sample3"; "Base filesample3"] 
            |> List.replicate 3
            |> List.collect id
            |> concatStrings 
            |> assertFileContent (Path.Combine(root, "summary.txt"))
        }

    [<Fact>]
    /// 
    member s.``Scatter with several outputs succeeds``() =
        async {
            // issue 73: Complex extensions are not supported (e.g. *.tar.gz)
            let root = s.ExperimentRoot
            
            let createManyFilesCommand = 
                if isTestRuntimeWindows then
                    "cmd /C \"FOR %i IN (1,2,3) DO (echo sample%i-1 > %i.1sample.txt && echo sample%i-2 > %i.2sample.txt)\""
                else
                    "/bin/bash -c \"for i in $(seq 1 3); do echo sample$i-1 > $i.1sample.txt && echo sample$i-2 > $i.2sample.txt ; done\""

            let! res = API.buildAsync root (Path.Combine(root, "samples")) [] ["*.1sample.txt";"*.2sample.txt"] createManyFilesCommand DependencyGraph.CommandExecutionSettings.Default
            assertResultOk res

            let res = API.compute (root, Path "samples/*.1sample.txt")
            assertResultOk res

            //["sample1"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample1.txt"))
            //["sample2"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample2.txt"))
            //["sample3"] |> concatStrings |> assertFileContent (Path.Combine(root, "samples", "sample3.txt"))
        }
                           
             

   