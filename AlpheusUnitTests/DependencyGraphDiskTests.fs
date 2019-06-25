module ItisLab.Alpheus.Tests.DependencyGraphDisk

open System.IO
open ItisLab.Alpheus.AlphFiles
open Xunit
open ItisLab.Alpheus
open Utils

type SampleExperiment() =
    inherit SingleUseOneTimeDirectory()

    // creating sample experiment folder for tests
    let artIdsStr = 
        [|
            @"dir1/test1.txt"
            @"dir1/test2/"
            @"dir2/test3.txt"
            @"dir2/test4/"
            @"dir3/dir5/test5.txt"
        |]

    let fullArtIds = Array.map ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID artIdsStr

    let rootPath = Path.GetFullPath(``base``.Path)

    do

        ItisLab.Alpheus.Config.createExperimentDirectoryAsync rootPath |> Async.RunSynchronously |> ignore

        let fullPaths = Array.map (fun x -> fullIDtoFullPath rootPath x) fullArtIds
        let fullAlphFilePaths = Array.map (fun x -> artefactPathToAlphFilePath x) fullPaths

        // creating dirs
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir1")) |> ignore
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir1","test2")) |> ignore
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir2")) |> ignore
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir3")) |> ignore
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir2","test4")) |> ignore
        Directory.CreateDirectory(Path.Combine(``base``.Path,"dir3","dir5")) |> ignore

        // creating files with content
        fullPaths
            |> Array.filter (fun (x:string) -> x.EndsWith("txt"))
            |> Array.iteri (fun i x -> File.WriteAllText(x,sprintf "data file %d" i) )

        // creating graph
        let g = DependencyGraph.Graph()

        // allocating artefact verticeis
        let artefacts = Array.map g.GetOrAllocateArtefact fullArtIds

        // filling in actual hashes
        ItisLab.Alpheus.DependencyGraph.fillinActualHashesAsync artefacts rootPath |> Async.RunSynchronously

        let versionedArtefacts = Array.map ItisLab.Alpheus.DependencyGraph.getVersionedArtefact artefacts

        // connecting artefacts with methods
        // actual dependency links are created here
        let source1 = DependencyGraph.Source(g.AllocateSnapshotVertex fullArtIds.[0])
        artefacts.[0].Input <- source1
        let source2 = DependencyGraph.Source(g.AllocateSnapshotVertex fullArtIds.[1])
        artefacts.[1].Input <- source2
        let method3 = g.AddMethod [versionedArtefacts.[0]; versionedArtefacts.[1]] [versionedArtefacts.[2]]
        let method4 = g.AddMethod [versionedArtefacts.[0]; versionedArtefacts.[1]] [versionedArtefacts.[3]]
        let method5 = g.AddMethod [versionedArtefacts.[2]; versionedArtefacts.[3]] [versionedArtefacts.[4]]

        // alph file content
        let alphFiles =
            let artToAlphFile artefact alphFilePath =
                ItisLab.Alpheus.DependencyGraph.artefactToAlphFile artefact alphFilePath rootPath
            Array.map2 artToAlphFile artefacts fullAlphFilePaths

        // dumping alph files to disk
        Array.iter2 (fun alph path -> saveAsync alph path |> Async.RunSynchronously) alphFiles fullAlphFilePaths

        // deleting first artefact alph file emulating the file without alph files
        File.Delete(fullAlphFilePaths.[0])

        // delete forth artefact to emulate dir absence
        Directory.Delete(fullPaths.[3])

    [<Fact>]
    member s.``LoadDependenciesAsync loads single source file artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact fullArtIds.[0]
            let! artefactVertices = g.LoadDependenciesAsync [artefact] rootPath

            Assert.Equal(1,artefactVertices.Count)
            Assert.Equal(artefact,Seq.exactlyOne artefactVertices)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads single source dir artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact fullArtIds.[1]
            let! artefactVertices = g.LoadDependenciesAsync [artefact] rootPath

            Assert.Equal(1,artefactVertices.Count)
            Assert.Equal(artefact,Seq.exactlyOne artefactVertices)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads two source artefacts``() =
        async {
            let g = DependencyGraph.Graph()
            let artefactsIdsToTest = Array.take 2 fullArtIds
            let artefactToTest = Array.map g.GetOrAllocateArtefact artefactsIdsToTest

            let! resultingVertices = g.LoadDependenciesAsync (List.ofArray artefactToTest) rootPath

            Assert.Equal(2,resultingVertices.Count)
            Assert.Equal<DependencyGraph.ArtefactVertex>(Set.ofArray artefactToTest, resultingVertices)
        } |> toAsyncFact
    
    [<Fact>]
    member s.``LoadDependenciesAsync loads single computed file artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact fullArtIds.[2]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] rootPath

            Assert.Equal(3,resultingVertices.Count)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads single computed dir artefact``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact fullArtIds.[3]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] rootPath

            Assert.Equal(3,resultingVertices.Count)
        } |> toAsyncFact

    [<Fact>]
    member s.``LoadDependenciesAsync loads full graph``() =
        async {
            let g = DependencyGraph.Graph()
            let artefact = g.GetOrAllocateArtefact fullArtIds.[4]
            let! resultingVertices = g.LoadDependenciesAsync [artefact] rootPath

            let resultingIDs = Seq.map (fun (x:DependencyGraph.ArtefactVertex) -> x.FullID) resultingVertices |> Set.ofSeq

            Assert.Equal(5,resultingVertices.Count)
            Assert.Equal<ArtefactFullID>(resultingIDs,Set.ofSeq fullArtIds)
        } |> toAsyncFact

