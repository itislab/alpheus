namespace ItisLab.Alpheus.Tests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus
open ItisLab.Alpheus.DependencyGraph
open FluentAssertions


type ``Dependency graph tests``(output) =
    inherit SingleUseOneTimeDirectory(output)
    
    let workingDir = sprintf ".%c" Path.DirectorySeparatorChar

    let getArtefact id (g:Graph) = 
        match g.GetArtefact(id) with
        | Result.Ok a -> a
        | Result.Error error -> failwithf "%A" error

    [<Fact>]
    member s.``Concurrency issue test``() =
    
        //let link = LinkToArtefact(artefact, expectedVersion)
        ()
    

    [<Fact>]
    member s.``DependencyGraph is empty initially``() =
        let g = DependencyGraph.Graph(".")
        Assert.Equal(0,g.Artefacts.Length)
        Assert.Equal(0,g.Methods.Length)
        Assert.Equal(0,g.MethodsCount)
        Assert.Equal(0,g.ArtefactsCount)

    [<Fact>]
    member s.``AddMethod adds new command line method and corresponding source artefacts`` () =
        async {
            let g = DependencyGraph.Graph(s.ExperimentRoot)
            let inArtefact = ArtefactId.Path "dir/testfile.txt"
            let outArtefact = ArtefactId.Path "output.txt"
        
            let! method = g.AddMethod "command" [inArtefact] [outArtefact] workingDir false
    
            Assert.NotNull method
            g.ArtefactsCount.Should().Be(2, "there must be one output and one input (source) artefact") |> ignore
            g.Artefacts.Should().Contain((fun (a:ArtefactVertex) -> a.Id = inArtefact), "there must be one input (source) artefact") |> ignore
            g.Artefacts.Should().Contain((fun (a:ArtefactVertex) -> a.Id = outArtefact), "there must be one output artefact") |> ignore

            g.MethodsCount.Should().Be(1, "there must be one added command method") |> ignore
            g.Methods.Should().OnlyContain((fun (m:MethodVertex) -> m.MethodId = method.MethodId), "there must be one command method") |> ignore
        }

    [<Fact>]
    member s.``AddMethod is idempotent`` () =
        async {
            let g = DependencyGraph.Graph(s.ExperimentRoot)
            let inArtefact = ArtefactId.Path "dir/testfile.txt"
            let outArtefact = ArtefactId.Path "output.txt"
        
            let! method = g.AddMethod "command" [inArtefact] [outArtefact] workingDir false    
            let! method2 = g.AddMethod "command" [inArtefact] [outArtefact] workingDir false

            method.Should().BeSameAs(method2, "AddMethod returns the existing method if it is already added") |> ignore
        }

    [<Fact>]
    member s.``LoadDependencies creates source method if there is no alph file`` () =
        async {
            let g = DependencyGraph.Graph(s.ExperimentRoot)
            let inArtefact = ArtefactId.Path "dir/testfile.txt"
            let outArtefact = ArtefactId.Path "output.txt"
           
            let! method = g.AddMethod "command" [inArtefact] [outArtefact] workingDir false
            g.LoadDependencies [method.Outputs.[0].Artefact]
       
            g.MethodsCount.Should().Be(2, "there must be one command method and one source method") |> ignore
            g.Methods.Should().Contain((fun (m:MethodVertex) -> m.MethodId = method.MethodId), "there must be the added command method") |> ignore
            g.Methods.Should().Contain((fun (m:MethodVertex) -> 
                match m with
                | Source src -> src.Output.Artefact.Id = inArtefact
                | _ -> false), "there must be a source method") |> ignore
        }

    [<Fact>]
    member s.``LoadDependencies loads an alph file for the dependency of the added method and correctly adds the corresponding method`` () =
        async {
            let g = DependencyGraph.Graph(s.ExperimentRoot)
            let inArtefact = ArtefactId.Path "dir/testfile.txt"
            let outArtefact = ArtefactId.Path "output.txt"
              
            let! method = g.AddMethod "command" [inArtefact] [outArtefact] workingDir false 
            File.Exists(outArtefact |> PathUtils.idToAlphFileFullPath s.ExperimentRoot).Should().BeTrue("AddMethod creates alph files for the outputs") |> ignore

            let g2 = DependencyGraph.Graph(s.ExperimentRoot)
            let out2Artefact = ArtefactId.Path "output2.txt"
            let! method2 = g2.AddMethod "command2" [outArtefact] [out2Artefact] workingDir false

            g2.MethodsCount.Should().Be(1, "there must be just one added method") |> ignore               
               
            g2.LoadDependencies [method2.Outputs.[0].Artefact]

            g2.MethodsCount.Should().Be(3, "there must be one added method, one loaded method and one source method") |> ignore
            g2.Methods.Should().Contain((fun (m:MethodVertex) -> m.MethodId = method.MethodId), "there must be the loaded command method") |> ignore
            g2.Methods.Should().Contain((fun (m:MethodVertex) -> m.MethodId = method2.MethodId), "there must be the added command method") |> ignore
            g2.Methods.Should().Contain((fun (m:MethodVertex) -> m.MethodId = "dir/testfile.txt"), "there must be the source method") |> ignore
        }
    
    [<Fact>]
    member s.``AddMethod and LoadDependencies create correct Artefacts`` () =
        async {
            let g = DependencyGraph.Graph(s.ExperimentRoot)
            let inArtefactId = ArtefactId.Path "dir/testfile.txt"
            let outArtefactId = ArtefactId.Path "output.txt"
         
            let! method = g.AddMethod "command" [inArtefactId] [outArtefactId] workingDir false
     
            let inArtefact = g |> getArtefact inArtefactId
            let outArtefact = g |> getArtefact outArtefactId
            g.LoadDependencies [outArtefact]

            inArtefact.UsedIn.Should().BeEquivalentTo(method) |> ignore
            inArtefact.ProducedBy.MethodId.Should().Be("dir/testfile.txt", "it is produced by the source method") |> ignore
            inArtefact.Rank.Should().Be(0, "it is a scalar operation") |> ignore

            outArtefact.UsedIn.Should().BeEmpty("no method using this artefact") |> ignore
            match outArtefact.ProducedBy with
            | Command cmd -> cmd.Should().Be(method, "produced by that method") |> ignore
            | _ -> failwith "Wrong method type"
            outArtefact.Rank.Should().Be(0, "it is a scalar operation") |> ignore
        }
    
