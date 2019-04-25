namespace Shared

type VertexId = string

type ArtefactVertex = { 
    id: VertexId
    label: string option
    source : VertexId
    dependants : VertexId list
}
and ProducerVertex =
    /// The vertex produces single artefact out of void
    |   Source of SourceVertex
    /// The vertex corresponds to invocation of single CLI command
    |   Computed of ComputedVertex
and ComputedVertex = {
    id: VertexId
    label: string option
    inputs : VertexId list
    outputs : VertexId list
    command: string
    workingDirectory: string
}
and SourceVertex = {
    id: string
    label: string option
    artefact: VertexId
}

type AlpheusGraph = {
    artefacts: ArtefactVertex list
    methods: ProducerVertex list
}

type [<RequireQualifiedAccess>] NodeKind =
    | Artefact
    | Method

type Node = {
    id: string
    label: string option
    kind: NodeKind
}

type Edge = {
    id: string
    label: string option
    source: Node
    target: Node
}

type Graph = {
    nodes: Node list
    edges: Edge list
}

type State = {
    graph: AlpheusGraph
}

type ServerMsg =
    | DoNothing

type ClientMsg =
    | Init
    | StateUpdated of State

module BridgeInfo =
    let endpoint = "/socket"