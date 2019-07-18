module ItisLab.Alpheus.Tests.SampleExperiment

open ItisLab.Alpheus.AlphFiles
open Xunit
open ItisLab.Alpheus
open Utils
open System.IO

type SampleExperiment() =
    inherit SingleUseOneTimeDirectory()

    // creating sample experiment folder for tests
    let artIdsStr = 
        [|
            @"dir1/test1.txt" // there will be no alph file for this artefact
            @"dir1/test2/"
            @"dir2/test3.txt" // depends on test1.txt and test2/
            @"dir2/test4/" // also depends on test11.txt and test2/ ; data for this artefact will be missing
            @"dir3/dir5/test5.txt" // depends on test3.txt and test4/
        |]

    let fullArtIds = Array.map ItisLab.Alpheus.AlphFiles.ArtefactId.ID artIdsStr

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
        let source1 = DependencyGraph.Source(g.AllocateSourceMethod fullArtIds.[0])
        artefacts.[0].ProducedBy <- source1
        let source2 = DependencyGraph.Source(g.AllocateSourceMethod fullArtIds.[1])
        artefacts.[1].ProducedBy <- source2
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

    member s.FullArtIds
        with get() =
            fullArtIds

    member s.RootPath
        with get() =
            rootPath