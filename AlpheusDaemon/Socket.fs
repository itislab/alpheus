module Socket

open Elmish
open Elmish.Bridge
open Shared
open ItisLab.Alpheus
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.DependencyGraph
open System.IO
//open Giraffe

let nodeA = { id = "A"; label = Some "A"; kind = NodeKind.Method }
let nodeB = { id = "B"; label = Some "B"; kind = NodeKind.Artefact }

let edgeAB = {
    id = "AB"
    label = None
    source = nodeA
    target = nodeB
}

let mutable globalState = {
    graph = {
        nodes = [ nodeA; nodeB ]
        edges = [ edgeAB ]
    }
}

let mutable nextId = "X"

let hub =
    ServerHub()

let init clientDispatch () = 
    clientDispatch( StateUpdated globalState )
    globalState, Cmd.none

let update clientDispatch msg state =
    match msg with
    | DoNothing ->
        let newNode = { id = nextId; label = Some nextId; kind = NodeKind.Artefact }
        let newEdge = { id = "A" + nextId; label = None; source = nodeA; target = newNode }
        nextId <- nextId + "X"
        // race condition below
        globalState <- { graph = { nodes = newNode :: globalState.graph.nodes; edges = newEdge :: globalState.graph.edges } }
        hub.BroadcastClient( StateUpdated globalState )
        globalState, Cmd.none


let buildDependencyGraphAsync experimentRoot artefactFullIDs =
            async {
                let! config = Config.openExperimentDirectoryAsync experimentRoot

                let g = DependencyGraph.Graph()
                let vertices = artefactFullIDs |> List.map g.GetOrAllocateArtefact 
                let! _ = g.LoadDependenciesAsync vertices experimentRoot
                //traceVerbose(sprintf "Dependency graph is built (%d artefacts; %d methods)" g.ArtefactsCount g.MethodsCount)
                // Filling in actual hashes
                do! fillinActualHashesAsync g.Artefacts experimentRoot
                //traceVerbose("Actual hashes are loaded")

                let storages = config.ConfigFile.Storage
                let storagePresenceChecker = StorageFactory.getPresenseChecker experimentRoot (Map.toSeq storages)

                // Filling in with information about which storages hold the versions of the artefacts
                let! _ = Async.Parallel [|fillinArtefactContainingStoragesAsync g.Artefacts storagePresenceChecker; fillinMethodEdgeContainingStoragesAsync g.Methods storagePresenceChecker|]
                //traceVerbose("Artefact presence in storages checked")
                return g
                }

let loadGraph path = async {
    let methodIdPrefix = "5F3E69FF"
    match Config.tryLocateExpereimentRoot path with
    | None ->
        globalState <- { graph = { nodes = [ { id = "COULD NOT LOAD GRAPH"; label = Some "COULD NOT LOAD GRAPH"; kind = NodeKind.Artefact } ]; edges = [] } }
        failwith "COULD NOT LOAD GRAPH"
    | Some experimentRoot ->
        let allAlphFiles = Directory.GetFiles(experimentRoot, "*.alph")
        let artefactIDs = allAlphFiles |> Seq.map (fun fn -> ArtefactFullID.ID(Path.GetRelativePath(experimentRoot, if fn.EndsWith(".alph") then fn.Substring(0, fn.Length - 5) else fn))) |> List.ofSeq
        let! depGraph = buildDependencyGraphAsync experimentRoot artefactIDs
        let artefactNodes = depGraph.Artefacts |> Seq.map (fun art -> { id = (fullIDtoString art.FullID); label = Some (fullIDtoString art.FullID); kind = NodeKind.Artefact })
        let methodNodes = depGraph.Methods
                            |> Seq.choose
                                (fun m -> 
                                    match m with
                                    | Source _ -> None
                                    | Computed cv -> Some { id = methodIdPrefix + (fullIDtoString cv.FirstOutputFullID); label = Some cv.Command; kind = NodeKind.Method }
                                    | NotSetYet -> None)
        let nodes = Seq.append artefactNodes methodNodes |> List.ofSeq
        let idToNode = nodes |> List.map (fun n -> (n.id, n)) |> Map.ofList
        let edges = depGraph.Methods
                        |> Seq.collect
                            (fun m ->
                                match m with
                                | Source _ -> Seq.empty
                                | Computed cv ->
                                    let methodId = methodIdPrefix + (fullIDtoString cv.FirstOutputFullID)
                                    seq {
                                        for inp in cv.Inputs -> { 
                                            id = (fullIDtoString inp.Artefact.FullID) + " -> " + methodId
                                            label = Some "input"
                                            source = idToNode.[fullIDtoString inp.Artefact.FullID]
                                            target = idToNode.[methodId]
                                        }
                                        for outp in cv.Outputs -> { 
                                            id = methodId + " -> " + (fullIDtoString outp.Artefact.FullID)
                                            label = Some "input"
                                            source = idToNode.[methodId]
                                            target = idToNode.[fullIDtoString outp.Artefact.FullID]
                                        }
                                    }
                                | NotSetYet -> Seq.empty)
                        |> List.ofSeq
        let graph = { nodes = nodes; edges = edges }
        globalState <- { graph = graph }
        hub.BroadcastClient( StateUpdated globalState )
        ()

    return ()
}

let server path: (Giraffe.Core.HttpFunc -> Microsoft.AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult) =
    loadGraph path |> Async.Start 
    Bridge.mkServer Shared.BridgeInfo.endpoint init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server