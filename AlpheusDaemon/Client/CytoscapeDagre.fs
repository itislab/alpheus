module CytoscapeDagre

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-dagre")>] dagre: Cytoscape.Ext = jsNative

type [<StringEnum>] DagreRankDir =
    | [<CompiledName("TB")>] TopToBottom
    | [<CompiledName("LR")>] LeftToRight

type [<StringEnum>] DagreRanker =
    | [<CompiledName("network-simplex")>] NetworkSimplex
    | [<CompiledName("tight-tree")>] TightTree
    | [<CompiledName("longest-path")>] LongestPath

type [<AllowNullLiteral>] DagreLayoutOptions =
        inherit LayoutOptions
        inherit BaseLayoutOptions
        inherit ShapedLayoutOptions
        /// the separation between adjacent nodes in the same rank
        abstract nodeSep: float option with get, set
        /// the separation between adjacent edges in the same rank
        abstract edgeSep: float option with get, set
        /// the separation between adjacent nodes in the same rank
        abstract rankSep: float option with get, set
        /// 'TB' for top to bottom flow, 'LR' for left to right
        abstract rankDir: DagreRankDir option with get, set
        /// Type of algorithm to assign a rank to each node in the input graph. Possible values: 'network-simplex', 'tight-tree' or 'longest-path'
        abstract ranker: DagreRanker option with get, set
        //abstract minLen: function( edge ){ return 1; }, // number of ranks to keep between the source and target of the edge
        //abstract edgeWeight: function( edge ){ return 1; }, // higher weight edges are generally made shorter and straighter than lower weight edges
        abstract name: string with get, set

