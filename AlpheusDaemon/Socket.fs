﻿module Socket

open Elmish
open Elmish.Bridge
open Shared
open ItisLab.Alpheus
open ItisLab.Alpheus.AlphFiles
open ItisLab.Alpheus.DependencyGraph
open System.IO
//open Giraffe

let nodeA = { id = "A"; label = Some "Loading"; kind = NodeKind.Method }
let nodeB = { id = "B"; label = Some "graph"; kind = NodeKind.Artefact }

let edgeAB = {
    id = "AB"
    label = None
    source = nodeA
    target = nodeB
}

let vertexLoading = Shared.Computed { id = "A"; label = Some "Loading"; inputs = Set.empty; outputs = (Set.ofList [ "B" ]); command = ""; workingDirectory = "" }
let vertexGraph = { ArtefactVertex.id ="B"; label = Some "graph"; source = "A"; dependants = Set.empty }
let graphLoadingGraph = {
    artefacts = Set.ofList [ vertexGraph ]
    methods = Set.ofList [ vertexLoading ]
}

let vertexCouldNotLoad = Shared.Computed { id = "A"; label = Some "COULD NOT LOAD GRAPH"; inputs = Set.empty; outputs = Set.empty; command = ""; workingDirectory = "" }
let graphCouldNotLoad = {
    artefacts = Set.empty
    methods = Set.ofList [ vertexCouldNotLoad ]
} // { nodes = [ { id = "COULD NOT LOAD GRAPH"; label = Some "COULD NOT LOAD GRAPH"; kind = NodeKind.Artefact } ]; edges = [] }

let mutable globalState = {
    graph = graphLoadingGraph
}

let mutable nextId = "X"

let hub =
    ServerHub()

let init clientDispatch () = 
    clientDispatch (StateUpdated globalState)
    globalState, Cmd.none

let update clientDispatch msg state =
    match msg with
    | DoNothing ->
        //let newNode = { id = nextId; label = Some nextId; kind = NodeKind.Artefact }
        //let newEdge = { id = "A" + nextId; label = None; source = nodeA; target = newNode }
        //nextId <- nextId + "X"
        //// race condition below
        //globalState <- { graph = { nodes = newNode :: globalState.graph.nodes; edges = newEdge :: globalState.graph.edges } }
        //hub.BroadcastClient( StateUpdated globalState )
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

let strcut (str: string) maxlen = if (str.Length > maxlen) then "..." + str.Substring(str.Length - maxlen, maxlen) else str

let formatCaption (label: string) =
    let cutLabel = strcut label 20
    if cutLabel.Length > 11 then cutLabel.Substring(0, 12) + "\n" + cutLabel.Substring(12) else cutLabel

let methodIdPrefix = "5F3E69FF"
let idForArtefact (art: ArtefactVertex) = fullIDtoString art.FullID
let idForSource (source: SourceVertex) = methodIdPrefix + (idForArtefact source.Artefact.Artefact)
let idForComputed (computed: ComputedVertex) = methodIdPrefix + (fullIDtoString computed.FirstOutputFullID)
let idForMethod (method: ProducerVertex) =
    match method with
    | Source s -> idForSource s
    | Computed cv -> idForComputed cv
    | NotSetYet -> failwith "Invalid method vertex state" 

let loadGraph path = async {
    
    match Config.tryLocateExpereimentRoot path with
    | None ->
        globalState <- { graph = graphCouldNotLoad }
        failwith "COULD NOT LOAD GRAPH"
    | Some experimentRoot ->
        let allAlphFiles = Directory.GetFiles(experimentRoot, "*.alph", SearchOption.AllDirectories)
        let! artefactPaths = allAlphFiles |> Array.map alphFilePathToArtefactPathAsync |> Async.Parallel
        let relPaths = artefactPaths |> Array.map (fun fn -> Path.GetRelativePath(experimentRoot, fn))
        let artefactIDs = relPaths |> Seq.map ArtefactFullID.ID |> List.ofSeq
        let! depGraph = buildDependencyGraphAsync experimentRoot artefactIDs
        let artefactNodes =
            depGraph.Artefacts
            |> Seq.map (fun art -> 
                let id = idForArtefact art
                { 
                    ArtefactVertex.id = id
                    label = Some (formatCaption id)
                    source = (idForMethod art.Input)
                    dependants = art.Outputs |> Set.map idForComputed
                })
            |> Set.ofSeq
        let methodNodes = depGraph.Methods
                            |> Seq.choose
                                (fun m -> 
                                    match m with
                                    | Source s ->
                                        Some (Shared.Source {
                                            id = idForSource s
                                            label = None
                                            artefact = idForArtefact s.Artefact.Artefact
                                        })
                                    | Computed cv ->
                                        let foid = fullIDtoString cv.FirstOutputFullID
                                        Some (Shared.Computed {
                                            id = idForComputed cv
                                            label = Some "Produce"
                                            inputs = cv.Inputs |> Set.map (fun va -> idForArtefact va.Artefact)
                                            outputs = cv.Outputs |> Set.map (fun va -> idForArtefact va.Artefact)
                                            command = cv.Command
                                            workingDirectory = cv.WorkingDirectory
                                        })
                                    | NotSetYet -> None)
                            |> Set.ofSeq
        //let artefactNodes =
        //    depGraph.Artefacts
        //    |> Seq.map (fun art -> 
        //        let id = fullIDtoString art.FullID
        //        { 
        //            id = id
        //            label = Some (formatCaption id)
        //            kind = NodeKind.Artefact
        //        })
        //let methodNodes = depGraph.Methods
        //                    |> Seq.choose
        //                        (fun m -> 
        //                            match m with
        //                            | Source _ -> None
        //                            | Computed cv ->
        //                                let foid = fullIDtoString cv.FirstOutputFullID
        //                                Some {
        //                                    id = methodIdPrefix + foid
        //                                    label = Some ("Produce\n" + (formatCaption foid))
        //                                    kind = NodeKind.Method
        //                                }
        //                            | NotSetYet -> None)
        //let nodes = Seq.append artefactNodes methodNodes |> List.ofSeq
        //let idToNode = nodes |> List.map (fun n -> (n.id, n)) |> Map.ofList
        //let edges = depGraph.Methods
        //                |> Seq.collect
        //                    (fun m ->
        //                        match m with
        //                        | Source _ -> Seq.empty
        //                        | Computed cv ->
        //                            let methodId = methodIdPrefix + (fullIDtoString cv.FirstOutputFullID)
        //                            seq {
        //                                for inp in cv.Inputs -> { 
        //                                    id = (fullIDtoString inp.Artefact.FullID) + " -> " + methodId
        //                                    label = Some ""
        //                                    source = idToNode.[fullIDtoString inp.Artefact.FullID]
        //                                    target = idToNode.[methodId]
        //                                }
        //                                for outp in cv.Outputs -> { 
        //                                    id = methodId + " -> " + (fullIDtoString outp.Artefact.FullID)
        //                                    label = Some ""
        //                                    source = idToNode.[methodId]
        //                                    target = idToNode.[fullIDtoString outp.Artefact.FullID]
        //                                }
        //                            }
        //                        | NotSetYet -> Seq.empty)
        //                |> List.ofSeq
        //let graph = { nodes = nodes; edges = edges }
        let graph = { 
            artefacts = artefactNodes
            methods = methodNodes
        }
        globalState <- { graph = graph }
        hub.BroadcastClient (StateUpdated globalState)
        ()

    return ()
}

let server path: (Giraffe.Core.HttpFunc -> Microsoft.AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult) =
    loadGraph path |> Async.Start 
    Bridge.mkServer Shared.BridgeInfo.endpoint init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server