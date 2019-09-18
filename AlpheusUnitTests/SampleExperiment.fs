module ItisLab.Alpheus.Tests.SampleExperiment

open ItisLab.Alpheus.AlphFiles
open Xunit
open ItisLab.Alpheus
open ItisLab.Alpheus.PathUtils
open Utils
open System.IO

/// strings that represent artefact IDs
let artIdsStr = 
    [|
        @"dir1/test1.txt" // there will be no alph file for this artefact
        @"dir1/test2/"
        @"dir2/test3.txt" // depends on test1.txt and test2/
        @"dir2/test4/" // also depends on test11.txt and test2/ ; data for this artefact will be missing
        @"dir3/dir5/test5.txt" // depends on test3.txt and test4/
    |]

type SampleExperiment(output) as this =
    inherit SingleUseOneTimeDirectory(output)

    // creating sample experiment folder for tests

    let artefactIds = Array.map ItisLab.Alpheus.ArtefactId.Path artIdsStr

    do
        async{
            let rootPath = this.ExperimentRoot
            do! ItisLab.Alpheus.Config.createExperimentDirectoryAsync rootPath |> Async.Ignore

            let fullPaths = Array.map (fun x -> idToFullPath rootPath x) artefactIds
            let fullAlphFilePaths = Array.map (fun x -> idToAlphFileFullPath rootPath x) artefactIds

            // creating dirs
            Directory.CreateDirectory(Path.Combine(rootPath,"dir1")) |> ignore
            Directory.CreateDirectory(Path.Combine(rootPath,"dir1","test2")) |> ignore
            Directory.CreateDirectory(Path.Combine(rootPath,"dir2")) |> ignore
            Directory.CreateDirectory(Path.Combine(rootPath,"dir3")) |> ignore
            Directory.CreateDirectory(Path.Combine(rootPath,"dir2","test4")) |> ignore
            Directory.CreateDirectory(Path.Combine(rootPath,"dir3","dir5")) |> ignore

            // creating files with content
            fullPaths
                |> Array.filter (fun (x:string) -> x.EndsWith("txt"))
                |> Array.iteri (fun i x -> File.WriteAllText(x,sprintf "data file %d" i) )

            // creating graph
            let g = DependencyGraph.Graph.Build(rootPath, [])
            let! method3 = g.AddMethod "" [artefactIds.[0]; artefactIds.[1]] [artefactIds.[2]] "./" DependencyGraph.DefaultExecutionSettings
            let! method4 = g.AddMethod "" [artefactIds.[0]; artefactIds.[1]] [artefactIds.[3]] "./" DependencyGraph.DefaultExecutionSettings
            let! method5 = g.AddMethod "" [artefactIds.[2]; artefactIds.[3]] [artefactIds.[4]] "./" DependencyGraph.DefaultExecutionSettings

            let outputs = List.concat [method3.Outputs; method4.Outputs; method5.Outputs] |> List.map(fun link -> link.Artefact)
            g.LoadDependencies outputs
            g.Artefacts |> Seq.iter(fun a -> a.SaveAlphFile()) 

            // deleting first artefact alph file emulating the file without alph files
            File.Delete(fullAlphFilePaths.[0])

            // delete forth artefact to emulate dir absence
            Directory.Delete(fullPaths.[3])
        } |> Async.RunSynchronously

    member s.ArtefactIds
        with get() =
            artefactIds
