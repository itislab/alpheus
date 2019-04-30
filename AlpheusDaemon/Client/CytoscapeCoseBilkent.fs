module CytoscapeCoseBilkent

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-cose-bilkent")>] coseBilkent: Cytoscape.Ext = jsNative

type [<StringEnum>] [<RequireQualifiedAccess>] LayoutAnimaton =
    | [<CompiledName("during")>] During
    | [<CompiledName("end")>] End
    | [<CompiledName("")>] False // Hey, !!"" == false in js

type [<AllowNullLiteral>] CoseBilkentLayoutOptions =
        inherit LayoutOptions
        inherit BaseLayoutOptions
        inherit ShapedLayoutOptions
        abstract name: string with get, set // 'cose-bilkent'
        /// Whether to include labels in node dimensions. Useful for avoiding label overlap (default false)
        abstract nodeDimensionsIncludeLabels: bool with get, set
        /// number of ticks per frame; higher is faster but more jerky (default 30)
        abstract refresh: float option with get, set
        /// Whether to fit the network view after when done (default true)
        abstract fit: bool with get, set
        /// Padding on fit (default 10)
        abstract padding: float option with get, set
        /// Whether to enable incremental mode (default true)
        abstract randomize: bool option with get, set
        /// Node repulsion (non overlapping) multiplier (default )
        abstract nodeRepulsion: float option with get, set
        /// Ideal (intra-graph) edge length (default 50)
        abstract idealEdgeLength: float option with get, set
        /// Divisor to compute edge forces (default 0.45)
        abstract edgeElasticity: float option with get, set
        /// Nesting factor (multiplier) to compute ideal edge length for inter-graph edges (default 0.1)
        abstract nestingFactor: float option with get, set
        /// Gravity force (constant) (default 0.25)
        abstract gravity: float option with get, set
        /// Maximum number of iterations to perform (default 2500)
        abstract numIter: float option with get, set
        /// Whether to tile disconnected nodes (default true)
        abstract tile: bool option with get, set
        /// Type of layout animation. The option set is {'during', 'end', false} (default 'end')
        abstract animate: LayoutAnimaton option with get, set
        /// Amount of vertical space to put between degree zero nodes during tiling (can also be a function) (default 10)
        abstract tilingPaddingVertical: float option with get, set
        /// Amount of horizontal space to put between degree zero nodes during tiling (can also be a function) (default 10)
        abstract tilingPaddingHorizontal: float option with get, set
        /// Gravity range (constant) for compounds (default 1.5)
        abstract gravityRangeCompound: float option with get, set
        /// Gravity force (constant) for compounds (default 1.0)
        abstract gravityCompound: float option with get, set
        /// Gravity range (constant) (default 3.8)
        abstract gravityRange: float option with get, set
        /// Initial cooling factor for incremental layout (default 0.5)
        abstract initialEnergyOnIncremental: float option with get, set



