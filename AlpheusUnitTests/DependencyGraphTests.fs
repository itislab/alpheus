module ItisLab.Alpheus.Tests.DependencyGraph

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus
open ItisLab.Alpheus.DependencyGraph

[<Fact>]
let ``GetOrAllocateArtefact returns single vertex for the same artefact ID`` () =
    let g = DependencyGraph.Graph()
    let ident = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile.txt"
    let vertex1 = g.GetOrAllocateArtefact ident
    let vertex2 = g.GetOrAllocateArtefact ident
    Assert.Equal(vertex1,vertex2)

[<Fact>]
let ``DependencyGraph is emplty initially``() =
    let g = DependencyGraph.Graph()
    Assert.Equal(0,g.Artefacts.Length)
    Assert.Equal(0,g.Methods.Length)
    Assert.Equal(0,g.MethodsCount)
    Assert.Equal(0,g.ArtefactsCount)

[<Fact>]
let ``GetOrAllocateComputeMethod returns single vertex for the same artefact ID`` () =
    let g = DependencyGraph.Graph()
    let ident = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile.txt"
    let vertex1 = g.GetOrAllocateComputeMethod ident
    let vertex2 = g.GetOrAllocateComputeMethod ident
    Assert.Equal(vertex1,vertex2)

[<Fact>]
let ``AllocateSnapshotVertex creates single vertex`` () =
    let g = DependencyGraph.Graph()
    let ident = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile.txt"
    let vertex1 = g.AllocateSnapshotVertex ident
    let producerVertex = ProducerVertex.Source vertex1
    
    Assert.Equal(1,g.ArtefactsCount) // as AllocateSnapshotVertex allocates artefact if it is not allocated
    Assert.Equal(1,g.MethodsCount)
    Assert.Equal(producerVertex, Seq.exactlyOne g.Methods)

[<Fact>]
let ``ConnectArtefactAsInput actually connects the vertices``() =
    let g = DependencyGraph.Graph()
    let artId = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile.txt"
    let methodId = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID @"dir1/testfile.txt2"
    let artVertex = g.GetOrAllocateArtefact artId
    let methodVertex = g.GetOrAllocateComputeMethod methodId
    let versionedArtVertex : DependencyGraph.VersionedArtefact =
        {
            Artefact= artVertex
            Version = Some "12345"
            StoragesContainingVersion = []
        }

    // before connecting
    Assert.Equal(0,artVertex.UsedIn.Count)
    Assert.Equal(0,methodVertex.Inputs.Count)
    Assert.Equal(0,methodVertex.Outputs.Count)

    // connecting
    g.ConnectArtefactAsInput versionedArtVertex methodVertex


    // checking
    Assert.Equal(1,artVertex.UsedIn.Count)
    Assert.Equal(1,methodVertex.Inputs.Count)
    Assert.Equal(0,methodVertex.Outputs.Count)

    Assert.Equal(versionedArtVertex, Seq.exactlyOne methodVertex.Inputs)
    Assert.Equal(methodVertex, Seq.exactlyOne artVertex.UsedIn)

[<Fact>]
let ``ConnectArtefactAsOutput actually connects the vertices``() =
    let g = DependencyGraph.Graph()
    let artId = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile.txt"

    let artVertex = g.GetOrAllocateArtefact artId
    let methodVertex = g.GetOrAllocateComputeMethod artId
    
    let versionedArtVertex : VersionedArtefact =
        {
            Artefact= artVertex
            Version = Some "5345345"
            StoragesContainingVersion = ["storage 1"]
        }

    let producerVertex = ProducerVertex.Computed methodVertex

    // before connecting
    Assert.Equal(0,artVertex.UsedIn.Count)
    Assert.Equal(0,methodVertex.Inputs.Count)
    Assert.Equal(0,methodVertex.Outputs.Count)

    // connecting 
    g.ConnectArtefactAsOutput versionedArtVertex producerVertex

    // testing
    match artVertex.ProducedBy with
    |   Computed cv -> Assert.Equal(methodVertex,cv)
    |   _ -> Assert.True(false,"artefact input must be the computed vertex")
    Assert.Equal(1,methodVertex.Outputs.Count)
    Assert.Equal(0,artVertex.UsedIn.Count)
    Assert.Equal(versionedArtVertex, Seq.exactlyOne methodVertex.Outputs)

[<Fact>]
let ``AddMethod handles 2 inputs, 1 output``() =
    let art1Id = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile1.txt"
    let art2Id = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile2.txt"
    let art3Id = ItisLab.Alpheus.AlphFiles.ArtefactFullID.ID  @"dir1/testfile3.txt"

    let g = DependencyGraph.Graph()

    let art1 = g.GetOrAllocateArtefact art1Id
    let art2 = g.GetOrAllocateArtefact art2Id
    let art3 = g.GetOrAllocateArtefact art3Id

    let vart1 = {Artefact = art1; Version=Some "1"; StoragesContainingVersion=[]}
    let vart2 = {Artefact = art2; Version=Some "2"; StoragesContainingVersion=[]}
    let vart3 = {Artefact = art3; Version=Some  "3"; StoragesContainingVersion=[]}

    let compVertex = g.AddMethod [vart1;vart2] [vart3]
    let producerVertex = Computed compVertex

    Assert.Equal(1,compVertex.Outputs.Count)
    Assert.Equal(2,compVertex.Inputs.Count)

    Assert.Equal(vart3, Seq.exactlyOne compVertex.Outputs)
    Assert.True(Set.contains vart1 compVertex.Inputs)
    Assert.True(Set.contains vart2 compVertex.Inputs)

    Assert.Equal(producerVertex, art3.ProducedBy)
    Assert.Equal(compVertex, Seq.exactlyOne art1.UsedIn)
    Assert.Equal(compVertex, Seq.exactlyOne art2.UsedIn)