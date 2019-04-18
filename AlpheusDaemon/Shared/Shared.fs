namespace Shared

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
    graph: Graph
}

type ServerMsg =
    | DoNothing

type ClientMsg =
    | Init
    | StateUpdated of State

module BridgeInfo =
    let endpoint = "/socket"