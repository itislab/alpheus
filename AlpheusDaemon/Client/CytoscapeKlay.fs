module CytoscapeKlay

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-klay")>] klay: Cytoscape.Ext = jsNative

type [<StringEnum>] [<RequireQualifiedAccess>] CrossingMinimizationAlgorithm =
    | [<CompiledName "LAYER_SWEEP">] LayerSweep
    | [<CompiledName "INTERACTIVE">] Interactive
    
type [<StringEnum>] [<RequireQualifiedAccess>] CycleBreakingAlgorithm =
    | [<CompiledName "GREEDY">] Greedy
    | [<CompiledName "INTERACTIVE">] Interactive
    
type [<StringEnum>] [<RequireQualifiedAccess>] Direction =
    | [<CompiledName "UNDEFINED">] Undefined
    | [<CompiledName "RIGHT">] Right
    | [<CompiledName "LEFT">] Left
    | [<CompiledName "DOWN">] Down
    | [<CompiledName "UP">] Up
    
type [<StringEnum>] [<RequireQualifiedAccess>] EdgeRouting =
    | [<CompiledName "POLYLINE">] Polyline
    | [<CompiledName "ORTHOGONAL">] Orthogonal
    | [<CompiledName "SPLINES">] Splines
    
type [<StringEnum>] [<RequireQualifiedAccess>] Alignment =
    | [<CompiledName "NONE">] None
    | [<CompiledName "LEFTUP">] LeftUp
    | [<CompiledName "RIGHTUP">] RightUp
    | [<CompiledName "LEFTDOWN">] LeftDown
    | [<CompiledName "RIGHTDOWN">] RightDown
    | [<CompiledName "BALANCED">] Balanced
    
type [<StringEnum>] [<RequireQualifiedAccess>] NodeLayeringStrategy =
    | [<CompiledName "NETWORK_SIMPLEX">] NetworkSimplex
    | [<CompiledName "LONGEST_PATH">] LongestPath
    | [<CompiledName "INTERACTIVE">] Interactive
    
type [<StringEnum>] [<RequireQualifiedAccess>] NodePlacementStrategy =
    | [<CompiledName "BRANDES_KOEPF">] BrandesKoepf
    | [<CompiledName "LINEAR_SEGMENTS">] LinearSegments
    | [<CompiledName "INTERACTIVE">] Interactive
    | [<CompiledName "SIMPLE">] Simple

type [<AllowNullLiteral>] KlayOptions =
    // Following descriptions taken from http://layout.rtsys.informatik.uni-kiel.de:9444/Providedlayout.html?algorithm=de.cau.cs.kieler.klay.layered
    /// Adds bend points even if an edge does not change direction.
    /// default: false
    abstract addUnnecessaryBendpoints: bool option with get, set
    /// The aimed aspect ratio of the drawing, that is the quotient of width by height
    /// default: 1.6
    abstract aspectRatio: float option with get, set
    /// Minimal amount of space to be left to the border
    /// default: 20
    abstract borderSpacing: float option with get, set
    /// Tries to further compact components (disconnected sub-graphs).
    /// default: false
    abstract compactComponents: bool option with get, set
    /// Strategy for crossing minimization.
    /// LAYER_SWEEP The layer sweep algorithm iterates multiple times over the layers, trying to find node orderings that minimize the number of crossings. The algorithm uses randomization to increase the odds of finding a good result. To improve its results, consider increasing the Thoroughness option, which influences the number of iterations done. The Randomization seed also influences results.
    /// INTERACTIVE Orders the nodes of each layer by comparing their positions before the layout algorithm was started. The idea is that the relative order of nodes as it was before layout was applied is not changed. This of course requires valid positions for all nodes to have been set on the input graph before calling the layout algorithm. The interactive layer sweep algorithm uses the Interactive Reference Point option to determine which reference point of nodes are used to compare positions.
    /// default: 'LAYER_SWEEP'
    abstract crossingMinimization: CrossingMinimizationAlgorithm option with get, set
    /// Strategy for cycle breaking. Cycle breaking looks for cycles in the graph and determines which edges to reverse to break the cycles. Reversed edges will end up pointing to the opposite direction of regular edges (that is, reversed edges will point left if edges usually point right).
    /// GREEDY This algorithm reverses edges greedily. The algorithm tries to avoid edges that have the Priority property set.
    /// INTERACTIVE The interactive algorithm tries to reverse edges that already pointed leftwards in the input graph. This requires node and port coordinates to have been set to sensible values.
    /// default: 'GREEDY'
    abstract cycleBreaking: CycleBreakingAlgorithm option with get, set
    /// Overall direction of edges: horizontal (right / left) or vertical (down / up)
    /// Default: 'UNDEFINED'
    abstract direction: Direction option with get, set
    /// Defines how edges are routed (POLYLINE, ORTHOGONAL, SPLINES)
    /// Default: 'ORTHOGONAL'
    abstract edgeRouting: EdgeRouting option with get, set
    /// Factor by which the object spacing is multiplied to arrive at the minimal spacing between edges.
    /// default: 0.5
    abstract edgeSpacingFactor: float option with get, set
    /// Whether feedback edges should be highlighted by routing around the nodes.
    /// default: false
    abstract feedbackEdges: bool option with get, set
    /// Tells the BK node placer to use a certain alignment instead of taking the optimal result. This option should usually be left alone.
    /// NONE Chooses the smallest layout from the four possible candidates.
    /// LEFTUP Chooses the left-up candidate from the four possible candidates.
    /// RIGHTUP Chooses the right-up candidate from the four possible candidates.
    /// LEFTDOWN Chooses the left-down candidate from the four possible candidates.
    /// RIGHTDOWN Chooses the right-down candidate from the four possible candidates.
    /// BALANCED Creates a balanced layout from the four possible candidates.
    /// default: 'NONE'
    abstract fixedAlignment: Alignment option with get, set
    /// Factor by which the usual spacing is multiplied to determine the in-layer spacing between objects.
    /// default: 1.0
    abstract inLayerSpacingFactor: float option with get, set
    /// Whether the selected layouter should consider the full hierarchy
    /// default: false
    abstract layoutHierarchy: bool option with get, set
    /// Dampens the movement of nodes to keep the diagram from getting too large.
    /// default: 0.3
    abstract linearSegmentsDeflectionDampening: float option with get, set
    /// Edges that have no ports are merged so they touch the connected nodes at the same points.
    /// default: false
    abstract mergeEdges: bool option with get, set
    /// If hierarchical layout is active, hierarchy-crossing edges use as few hierarchical ports as possible.
    /// default: true
    abstract mergeHierarchyCrossingEdges: bool option with get, set
    /// Strategy for node layering.
    /// NETWORK_SIMPLEX This algorithm tries to minimize the length of edges. This is the most computationally intensive algorithm. The number of iterations after which it aborts if it hasn't found a result yet can be set with the Maximal Iterations option.
    /// LONGEST_PATH A very simple algorithm that distributes nodes along their longest path to a sink node.
    /// INTERACTIVE Distributes the nodes into layers by comparing their positions before the layout algorithm was started. The idea is that the relative horizontal order of nodes as it was before layout was applied is not changed. This of course requires valid positions for all nodes to have been set on the input graph before calling the layout algorithm. The interactive node layering algorithm uses the Interactive Reference Point option to determine which reference point of nodes are used to compare positions.
    /// default: 'NETWORK_SIMPLEX'
    abstract nodeLayering: NodeLayeringStrategy option with get, set
    /// Strategy for Node Placement
    /// BRANDES_KOEPF Minimizes the number of edge bends at the expense of diagram size: diagrams drawn with this algorithm are usually higher than diagrams drawn with other algorithms.
    /// LINEAR_SEGMENTS Computes a balanced placement.
    /// INTERACTIVE Tries to keep the preset y coordinates of nodes from the original layout. For dummy nodes, a guess is made to infer their coordinates. Requires the other interactive phase implementations to have run as well.
    /// SIMPLE Minimizes the area at the expense of... well, pretty much everything else.
    /// default: 'BRANDES_KOEPF'
    abstract nodePlacement: NodePlacementStrategy option with get, set
    /// Seed used for pseudo-random number generators to control the layout algorithm; 0 means a new seed is generated
    /// default: 1
    abstract randomizationSeed: float option with get, set
    /// Whether a self-loop is routed around or inside its node.
    /// default: false
    abstract routeSelfLoopInside: bool option with get, set
    /// Whether each connected component should be processed separately
    /// default: true
    abstract separateConnectedComponents: bool option with get, set
    /// Overall setting for the minimal amount of space to be left between objects
    /// default: 20
    abstract spacing: float option with get, set
    /// How much effort should be spent to produce a nice layout..
    /// default: 7
    abstract thoroughness: float option with get, set

type [<AllowNullLiteral>] KlayLayoutOptions =
        inherit LayoutOptions
        inherit BaseLayoutOptions
        inherit ShapedLayoutOptions
        abstract name: string with get, set
        abstract klay: KlayOptions option with get, set