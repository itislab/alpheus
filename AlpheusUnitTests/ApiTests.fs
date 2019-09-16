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
open System
open ItisLab.Alpheus
open Angara.Data
open System.Linq.Expressions

/// can be used as calssData for XUnit theory. returns all of the artefactIDs for the sample experiment
type ArtefactIdSource() =
    let sequence : IEnumerable<obj array> = SampleExperiment.artIdsStr |> Seq.map ArtefactId.Path |> Seq.map (fun x -> x :> obj) |> Seq.map (fun x -> [|x|])
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
            yield [| {inputsIndices=[|3|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|4|]; OutputIDs=[|"dir1/test6.txt"|]; ExpectedArtefactsInGraph = 6; ExpectedMethodsInGraph = 6} |]
            yield [| {inputsIndices=[|0|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|1|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 2; ExpectedMethodsInGraph = 2} |]
            yield [| {inputsIndices=[|2|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|3|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 4; ExpectedMethodsInGraph = 4} |]
            yield [| {inputsIndices=[|4|]; OutputIDs=[|"dir1/test6/"|]; ExpectedArtefactsInGraph = 6; ExpectedMethodsInGraph = 6} |]
        }
    interface IEnumerable<obj array> with
        member s.GetEnumerator() =
            sequence.GetEnumerator()

    interface IEnumerable with
        member s.GetEnumerator() =
            sequence.GetEnumerator() :> IEnumerator            

type DepGraphConstruction(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Theory>]
    [<ClassData(typedefof<ArtefactIdSource>)>]
    member s.``dependencyGraph loads for separate artefacts``(artefactId:ArtefactId) =
        async {
            Logger.logInfo Logger.Test (sprintf "testing %A" artefactId)
            let! graph = buildDependencyGraphAsync s.RootPath [artefactId]
            Assert.True(graph.ArtefactsCount>0,"Graph must be non-empty")
            } |> toAsyncFact

    [<Fact>]
    member s.``dependencyGraph loads for all at once``() =
        async {
            Logger.logInfo Logger.Test (sprintf "testing %A" s.ArtefactIds)
            let! graph = buildDependencyGraphAsync s.RootPath (List.ofArray s.ArtefactIds)
            Assert.True(graph.ArtefactsCount>0,"Graph must be non-empty")
            } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Ensures that build command finishes successfully for all test cases
    member s.``build: finishes``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks the artefact and method counts in the graph after the build command for all test cases
    member s.``build: vertex count check``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx].ToString()) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult

            //let alphFiles = List.map artefactPathToAlphFilePath outputPaths
            // checking grpah for each newly created output artefact
            let checkGraphAsync outputId expectedMethodCount expectedArtefactCount = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath [outputId]
                    Assert.Equal(expectedMethodCount,graph.MethodsCount)
                    Assert.Equal(expectedArtefactCount,graph.ArtefactsCount)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.Path x) testCase.ExpectedMethodsInGraph testCase.ExpectedArtefactsInGraph)
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<GraphBuildDataSource>)>]
    /// Checks that newly built artefact vertices have expected inputs
    member s.``build: new artefact deps check``(testCase) =
        async {
            // preparing and running build command
            let inputIDs = testCase.inputsIndices |> Array.map (fun idx -> s.ArtefactIds.[idx]) |> List.ofArray
            let inputPaths = inputIDs |> List.map (fun x -> Path.Combine(s.Path,x.ToString()))
            let outputPaths = testCase.OutputIDs |> Array.map (fun x -> Path.Combine(s.Path,x)) |> List.ofArray
            let! buildResult = buildAsync s.RootPath inputPaths outputPaths "../copy_prog $in1 $out1" false
            assertResultOk buildResult
            Logger.logInfo Logger.Test "Graph is build via API call successfuly"

            // checking graph for each newly created output artefact
            let checkGraphAsync outputId = 
                async {
                    let! graph = buildDependencyGraphAsync s.RootPath [outputId]
                    let output = graph.Artefacts |> Seq.find (fun a -> a.Id = outputId)
                    let expectedDependecies = inputIDs |> Set.ofList
                    let actualDependecies =
                        match output.ProducedBy with
                        |   MethodVertex.Source(s) -> Assert.True(false, "the artefact must be produced by compute method, not source"); failwith ":("
                        |   MethodVertex.Command(comp) -> comp.Inputs |> Seq.map (fun x -> x.Artefact.Id) |> Set.ofSeq
                    Assert.Equal<ArtefactId>(expectedDependecies, actualDependecies)
                }

            let checks = testCase.OutputIDs |> Array.map (fun x -> checkGraphAsync (ArtefactId.Path x))
            checks |> Array.iter Async.RunSynchronously
        } |> toAsyncFact

type DepGraphLocalComputation(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Fact>]
    member s.``API Compute: concat 2 files``() =
        let expRoot = s.FullPath
        let savedWD = Environment.CurrentDirectory
        try
            // we will concat these files
            File.Copy("data/cat.cmd",Path.Combine(s.Path,"cat.cmd"))
            File.Copy("data/texturalData.txt",Path.Combine(s.Path,"1.txt"))
            File.Copy("data/texturalData2.txt",Path.Combine(s.Path,"2.txt"))
        
            // setting proper working directory
            Environment.CurrentDirectory <- expRoot
            
            let concatCommand = 
                if isTestRuntimeWindows then
                    "cmd /C \"cat.cmd $out1 $in1 $in2\""
                else
                    "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""
            let buildResult = API.buildAsync expRoot ["1.txt"; "2.txt"] ["cat_test.txt"] concatCommand false |> Async.RunSynchronously
            assertResultOk buildResult

            // graph is constructed. Now executing
            printfn "Graph constructed"

            Assert.False(File.Exists("cat_test.txt"))
            let computeResult = result {
                let! target = API.artefactFor "cat_test.txt"
                return! API.compute target
            }
            assertResultOk computeResult
            Assert.True(File.Exists("cat_test.txt"))

            let f1 = File.ReadAllText("1.txt")
            let f2 = File.ReadAllText("2.txt")
            let f3 = File.ReadAllText("cat_test.txt")

            // And verifying the content
            let len = (f1+f2).Length
            Assert.Equal(f1+f2,f3.Substring(0,len)) // constraining the length as windows copy command writes strange SUB symbol et the end of the file

        finally
            Environment.CurrentDirectory <- savedWD

type DepGraphSaveRestore(output) =
    inherit SampleExperiment.SampleExperiment(output)

    [<Fact>]
    member s.``API Save-Restore: file artefact saves & restores``() =
        async {
            let artId = s.ArtefactIds.[0]
            let path = artId |> PathUtils.idToFullPath s.FullPath

            // local storage is available by default
            let! saveResult =  API.saveAsync (s.FullPath, artId) (Some "local") false
            assertResultOk saveResult
             
            // saving the content for later verification
            let! origContent = File.ReadAllTextAsync(path) |> Async.AwaitTask

            // deleting the file to restore it
            File.Delete(path)

            Assert.False(File.Exists(path))

            let! restoreResult = API.restoreAsync (s.FullPath, artId)
            assertResultOk restoreResult

            // checking that it actually restored and it's content matches
            Assert.True(File.Exists(path),"File is not restored")
            let! restoredContent = File.ReadAllTextAsync(path) |> Async.AwaitTask
            Assert.Equal(origContent,restoredContent)

        }


    [<Fact>]
    member s.``API storage list: default is local``() =
        async {
            let! storages,defStorage = API.configListStoragesAsync s.FullPath

            Assert.Equal(1,storages.Count)

            let localKey = storages |> Map.toSeq |> Seq.map fst |> Seq.head

            Assert.Equal("local",localKey)

            Assert.Equal(localKey,defStorage)
        }

    [<Fact>]
    member s.``API storage add local: addes second storage``() =
        async {
            let newPath = Path.Combine(s.FullPath, "storage2")

            Directory.CreateDirectory(newPath) |> ignore
            
            do! API.configAddDirectoryStorageAsync s.FullPath "local2"  newPath

            let! storages2,defStorage2 = API.configListStoragesAsync s.FullPath

            Assert.Equal(2,storages2.Count)
            Assert.Equal("local",defStorage2)

            let secondDef = Map.find "local2" storages2
            match secondDef with
            |   Config.Directory p -> Assert.Equal(newPath, p)
            |   _ -> Assert.True(false, "expected to see local dir storage definition")
        }

    [<Fact>]
    member s.``API storage set default: changes default storage``() =
        async {
            let newPath = Path.Combine(s.FullPath, "storage2")

            Directory.CreateDirectory(newPath) |> ignore
            
            do! API.configAddDirectoryStorageAsync s.FullPath "local2" newPath
            
            let! res = API.configStorageSetDefault s.FullPath "local2"

            assertResultOk res

            let! _,defStorage = API.configListStoragesAsync s.FullPath

            Assert.Equal("local2", defStorage)
        }
    
    [<Fact>]
    member s.``API storage remove: default is removed``() =
        async {            
            do! API.configRemoveStorageAsync s.FullPath "local"

            let! storages,defStorage = API.configListStoragesAsync s.FullPath

            Assert.Equal(0,storages.Count)

            // default is still set to local
            Assert.Equal("local",defStorage)
        }

    [<Theory>] // gitgnore must always hold the entries with straight slashes (even on windows)
    [<InlineData(@"dir1/test2/")>]
    [<InlineData(@"dir2/test3.txt")>]
    [<InlineData(@"dir1/test1.txt")>] // alph file is missing for this artefact
    [<InlineData(@"dir3/dir5/test5.txt")>]
    member s.``API Save: gitignore entry is added for single file``(artIdStr) =
        async {
            let artefactId = ArtefactId.Path artIdStr
            // local storage is available by default
            let! saveResult =  API.saveAsync (s.FullPath, artefactId) (Some "local") false
            assertResultOk saveResult

            let gitIgnorePath = Path.Combine(s.Path,".gitignore")

            Assert.True(File.Exists(gitIgnorePath), ".gitignore file must be created")

            let! entries = File.ReadAllLinesAsync gitIgnorePath |> Async.AwaitTask
            Assert.True(Seq.exists (fun s -> s = artIdStr) entries, ".gitignore must contain newly added artefact record")

        } |> toAsyncFact

let equalStatuses expected actual =
    let s1 = Map.toSeq expected
    let s2 = Map.toSeq actual
    Seq.forall2 (fun x y -> let idx1,v1 = x in let idx2,v2 = y in (idx1=idx2) && MdMap.equal (fun _ elem1 elem2 -> elem1=elem2) v1 v2) s1 s2

type ScalarScenarios(output) =
    inherit SingleUseOneTimeDirectory(output)

    let concatCommand = 
        if isTestRuntimeWindows then
            "cmd /C \"cat.cmd $out1 $in1 $in2\""
        else
            "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""

    // first file is duplicated
    let concatCommand2 = 
        if isTestRuntimeWindows then
            "cmd /C \"cat.cmd $out1 $in1 $in1\""
        else
            "/bin/sh -c \"cat $in1 > $out1; cat $in1 >> $out1\""


    let buildExperiment(path) =
        async {
            let path = Path.GetFullPath path
            let! _ = API.createExperimentDirectory path
            File.Copy("../../../data/cat.cmd",Path.Combine(path,"cat.cmd"))

            do! File.WriteAllTextAsync(Path.Combine(path,"1.txt"),"File 1\\r\\n") |> Async.AwaitTask
            do! File.WriteAllTextAsync(Path.Combine(path,"2.txt"),"File 2\\r\\n") |> Async.AwaitTask
            do! File.WriteAllTextAsync(Path.Combine(path,"3.txt"),"File 3\\r\\n") |> Async.AwaitTask

            let! res1 =  API.buildAsync path ["1.txt";"2.txt"] ["1_2.txt"] concatCommand false
            assertResultOk res1
            let! res2 =  API.buildAsync path ["1_2.txt";"3.txt"] ["1_2_3.txt"] concatCommand false
            assertResultOk res2
            return ()
        }

    let assertNonEmptyFile path = 
        async {
            let! content = File.ReadAllTextAsync(path) |> Async.AwaitTask
            Assert.True(content.Length > 0)
        }

    [<Fact>]
    member s.``Uncomputed chain is computed``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                let res = API.compute(path, ArtefactId.Path "1_2_3.txt")
                assertResultOk res

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                do! assertNonEmptyFile(Path.Combine(path,"1_2_3.txt"))
            finally
                Environment.CurrentDirectory <- savedWD            
        } |> toAsyncFact

    [<Fact>]
    member s.``Status: Uncomputed chain``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated);
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)

            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: Computed chain``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")
                
                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                                
                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)

            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: changed input content of computed graph``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: Artefacts are computed")

                // now changing 3.txt
                do! File.WriteAllTextAsync(Path.Combine(path,"3.txt"),"File 3 changed\\r\\n") |> Async.AwaitTask

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local); // anyway up to date
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated); // but this is invalid now
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
            finally
                Environment.CurrentDirectory <- savedWD            
        } |> toAsyncFact

    [<Fact>]
    member s.``Changed input content invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                let! content1 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                // now changing 3.txt
                do! File.WriteAllTextAsync(Path.Combine(path,"3.txt"),"File 3 changed\\r\\n") |> Async.AwaitTask

                // this should change 1_2_3.txt
                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                output.WriteLine("TEST: second time computed 1_2_3.txt")
                let! content2 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                Assert.True(content1.Length > 0)
                Assert.True(content2.Length > 0)
                Assert.NotEqual<string>(content1,content2)
            finally
                Environment.CurrentDirectory <- savedWD            
        } |> toAsyncFact

    [<Fact>]
    member s.``Changed output content invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                let! content1 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                // now changing 3.txt
                do! File.WriteAllTextAsync(Path.Combine(path,"1_2_3.txt"),"manually changed\\r\\n") |> Async.AwaitTask

                // this should change 1_2_3.txt
                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                output.WriteLine("TEST: second time computed 1_2_3.txt")
                let! content2 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                Assert.True(content1.Length > 0)
                Assert.True(content2.Length > 0)
                Assert.Equal<string>(content1,content2)
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: Changed output content invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                    
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                    
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                // now changing 1_2_3.txt
                do! File.WriteAllTextAsync(Path.Combine(path,"1_2_3.txt"),"manually changed\\r\\n") |> Async.AwaitTask

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.OutputsOutdated); // but this is invalid now
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    
    [<Fact>]
    member s.``Changed command invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                let! content1 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                // now changing 1_2_3.txt producing command
                let! res2 =  API.buildAsync path ["1_2.txt";"3.txt"] ["1_2_3.txt"] concatCommand2 false // double 
                assertResultOk res2

                // this should change 1_2_3.txt
                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                output.WriteLine("TEST: second time computed 1_2_3.txt")
                let! content2 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                Assert.True(content1.Length > 0)
                Assert.True(content2.Length > 0)
                Assert.NotEqual<string>(content1,content2)
            finally
                Environment.CurrentDirectory <- savedWD            
        } |> toAsyncFact

    [<Fact>]
    member s.``Status: changed command invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                
                // now changing 1_2_3.txt producing command
                let! res2 =  API.buildAsync path ["1_2.txt";"3.txt"] ["1_2_3.txt"] concatCommand2 false // double 
                assertResultOk res2

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated); // Inputs, not outputs, because rebuilding command resets expected input versions
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Changed inputs order invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                let! content1 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                // now changing inputs order 
                let! res2 =  API.buildAsync path ["3.txt";"1_2.txt"] ["1_2_3.txt"] concatCommand false // double 
                assertResultOk res2

                // this should change 1_2_3.txt
                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                output.WriteLine("TEST: second time computed 1_2_3.txt")
                let! content2 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                Assert.True(content1.Length > 0)
                Assert.True(content2.Length > 0)
                Assert.NotEqual<string>(content1,content2)
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: changed inputs order invalidates vertex``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                
                do! buildExperiment(path)
                output.WriteLine("TEST: experiment graph is constructed")

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt")
                
                output.WriteLine("TEST: first time computed 1_2_3.txt")

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                let! content1 = File.ReadAllTextAsync(Path.Combine(path,"1_2_3.txt")) |> Async.AwaitTask

                // now changing inputs order 
                let! res2 =  API.buildAsync path ["3.txt";"1_2.txt"] ["1_2_3.txt"] concatCommand false // double 
                assertResultOk res2

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated); // Inputs, not outputs, because rebuilding command resets expected input versions
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)

            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``recompute missing intermediate while computing final``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                let res = API.compute(path, ArtefactId.Path "1_2_3.txt") // first compute all
                assertResultOk res

                File.Delete(Path.Combine(path,"1_2.txt")) // then delete intermediate 

                let res = API.compute(path, ArtefactId.Path "1_2_3.txt") // this computation should rebuild intermediate (as it is not in storage)
                assertResultOk res

                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt"))
                
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: missing intermediate``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                let res = API.compute(path, ArtefactId.Path "1_2_3.txt") // first compute all
                assertResultOk res

                File.Delete(Path.Combine(path,"1_2.txt")) // then delete intermediate 

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.OutputsOutdated); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.InputsOutdated);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
                
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``skip missing (storage present) intermediate while computing final``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // first compute all

                let! res = API.saveAsync(path, ArtefactId.Path "1_2.txt") (Some "local") false // saving intermediate
                assertResultOk res

                File.Delete(Path.Combine(path,"1_2.txt")) // then delete intermediate 

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // this computation must not recompute intermediate, as it is stored in storage
                // if needed by further methods, the methods execution can restore the intermediate during inpute restore computation phase
                
                Assert.False(File.Exists(Path.Combine(path,"1_2.txt")))

                do! assertNonEmptyFile(Path.Combine(path,"1_2_3.txt"))
                
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: intermediate is remotely available``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // first compute all

                let! res = API.saveAsync(path, ArtefactId.Path "1_2.txt") (Some "local") false // saving intermediate
                assertResultOk res

                File.Delete(Path.Combine(path,"1_2.txt")) // then delete intermediate 

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
                
            finally
                Environment.CurrentDirectory <- savedWD            
        }


    [<Fact>]
    member s.``restore only intermediate while computing final``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // computing all

                // saving all except final
                let! res = API.saveAsync(path, ArtefactId.Path "1_2.txt") (Some "local") false // saving intermediate
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "1.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "2.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "3.txt") (Some "local") false // saving initial
                assertResultOk res


                //Deleting all
                [
                    Path.Combine(path,"1_2_3.txt");
                    Path.Combine(path,"1_2.txt");
                    Path.Combine(path,"1.txt");
                    Path.Combine(path,"2.txt");
                    Path.Combine(path,"3.txt");
                ] |> List.iter File.Delete

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // computing all

                do! assertNonEmptyFile(Path.Combine(path,"1_2_3.txt")) // final is recomputed
                do! assertNonEmptyFile(Path.Combine(path,"1_2.txt")) // intermediate is restored
                do! assertNonEmptyFile(Path.Combine(path,"3.txt")) // intermediate is restored
                Assert.False(File.Exists(Path.Combine(path,"1.txt"))) // but initials are not restored, a intermediate is sufficient
                Assert.False(File.Exists(Path.Combine(path,"2.txt"))) // but initials are not restored, a intermediate is sufficient
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: all but final exist remotely``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // computing all

                // saving all except final
                let! res = API.saveAsync(path, ArtefactId.Path "1_2.txt") (Some "local") false // saving intermediate
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "1.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "2.txt") (Some "local") false // saving initial
                assertResultOk res


                //Deleting all
                [
                    Path.Combine(path,"1_2_3.txt");
                    Path.Combine(path,"1_2.txt");
                    Path.Combine(path,"1.txt");
                    Path.Combine(path,"2.txt");
                ] |> List.iter File.Delete

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate DependencyGraph.Local);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.NeedsRecomputation OutdatedReason.OutputsOutdated);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
            finally
                Environment.CurrentDirectory <- savedWD            
        }

    [<Fact>]
    member s.``Status: all exist remotely``() =
        async {
            let savedWD = Environment.CurrentDirectory
            try            
                let path = Path.GetFullPath s.Path
                Environment.CurrentDirectory <- s.Path
                do! buildExperiment(path)

                assertResultOk <| API.compute(path, ArtefactId.Path "1_2_3.txt") // computing all

                // saving all except final
                let! res = API.saveAsync(path, ArtefactId.Path "1_2.txt") (Some "local") false // saving intermediate
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "1.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "2.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "3.txt") (Some "local") false // saving initial
                assertResultOk res
                let! res = API.saveAsync(path, ArtefactId.Path "1_2_3.txt") (Some "local") false 
                assertResultOk res


                //Deleting all
                [
                    Path.Combine(path,"1_2_3.txt");
                    Path.Combine(path,"1_2.txt");
                    Path.Combine(path,"1.txt");
                    Path.Combine(path,"2.txt");
                    Path.Combine(path,"3.txt");
                ] |> List.iter File.Delete

                let res = API.status(path, ArtefactId.Path "1_2_3.txt")
                match res with
                |   Ok r ->
                    let expectedStatuses:Map<ArtefactId,MdMap<string,StatusGraph.ArtefactStatus>> = 
                        [ 
                            ArtefactId.Path "1.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                            ArtefactId.Path "2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                            ArtefactId.Path "3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                            ArtefactId.Path "1_2.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote); 
                            ArtefactId.Path "1_2_3.txt",MdMap.scalar (StatusGraph.ArtefactStatus.UpToDate ArtefactLocation.Remote);
                        ] |> Map.ofList
                    Assert.True(equalStatuses expectedStatuses r)
                |   Error e->
                        Assert.True(false, sprintf "Error: %A" e)
            finally
                Environment.CurrentDirectory <- savedWD            
        }
        