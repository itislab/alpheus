module CytoscapeFCose

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-fcose")>] fCose: Cytoscape.Ext = jsNative

type [<StringEnum>] [<RequireQualifiedAccess>] Quality =
    | [<CompiledName("draft")>] Draft
    | [<CompiledName("default")>] Default
    | [<CompiledName("proof")>] Proof

type [<AllowNullLiteral>] FCoseLayoutOptions =
        inherit LayoutOptions
        inherit BaseLayoutOptions
        abstract name: string with get, set // 'fcose'

        /// 'draft', 'default' or 'proof' 
        /// - "draft" only applies spectral layout 
        /// - "default" improves the quality with incremental layout (fast cooling rate)
        /// - "proof" improves the quality with incremental layout (slow cooling rate) 
        abstract quality: Quality option with get, set
        /// use random node positions at beginning of layout
        /// if this is set to false, then quality option must be "proof"
        abstract randomize: bool option with get, set
        /// whether or not to animate the layout (default true)
        abstract animate: bool option with get, set
        /// duration of animation in ms, if enabled (default 1000)
        abstract animationDuration: float option with get, set
        // easing of animation, if enabled
        //animationEasing: undefined, 
        /// fit the viewport to the repositioned nodes (default true)
        abstract fit: bool option with get, set
        /// padding around layout (default 10)
        abstract padding: float option with get, set
        /// whether to include labels in node dimensions. Valid in "proof" quality (default false)
        abstract nodeDimensionsIncludeLabels: bool option with get, set

        (* spectral layout options *)

        /// false for random, true for greedy sampling (default true)
        abstract samplingType: bool option with get, set
        /// sample size to construct distance matrix (default 25)
        abstract sampleSize: float option with get, set
        /// separation amount between nodes (default 75)
        abstract nodeSeparation: float option with get, set
        /// power iteration tolerance (default 0.0000001)
        abstract piTol: float option with get, set

        (* incremental layout options *)

        /// Node repulsion (non overlapping) multiplier (default 4500)
        abstract nodeRepulsion: float option with get, set
        /// Ideal edge (non nested) length (default 50)
        abstract idealEdgeLength: float option with get, set
        /// Divisor to compute edge forces (default 0.45)
        abstract edgeElasticity: float option with get, set
        /// Nesting factor (multiplier) to compute ideal edge length for nested edges (default 0.1)
        abstract nestingFactor: float option with get, set
        /// Gravity force (constant) (default 0.25)
        abstract gravity: float option with get, set
        /// Maximum number of iterations to perform (default 2500)
        abstract numIter: float option with get, set
        /// For enabling tiling (default false)
        abstract tile: bool option with get, set
        /// Represents the amount of the vertical space to put between the zero degree members during the tiling operation(can also be a function) (default 10)
        abstract tilingPaddingVertical: float option with get, set
        /// Represents the amount of the horizontal space to put between the zero degree members during the tiling operation(can also be a function) (default 10)
        abstract tilingPaddingHorizontal: float option with get, set
        /// Gravity range (constant) for compounds (default 1.5)
        abstract gravityRangeCompound: float option with get, set
        /// Gravity force (constant) for compounds (default 1.0)
        abstract gravityCompound: float option with get, set
        /// Gravity range (constant) (default 3.8)
        abstract gravityRange: float option with get, set
        /// Initial cooling factor for incremental layout  (default 0.3)
        abstract initialEnergyOnIncremental: float option with get, set
