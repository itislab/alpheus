module CytoscapeCola

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-cola")>] cola: Cytoscape.Ext = jsNative

type [<StringEnum>] [<RequireQualifiedAccess>] Axis =
    | [<CompiledName("x")>] X
    | [<CompiledName("y")>] Y

type [<AllowNullLiteral>] FlowOptions =
        abstract axis: Axis with get, set // 'x'
        abstract minSeparation: float option with get, set // 30

type [<AllowNullLiteral>] ColaLayoutOptions =
        inherit LayoutOptions
        inherit BaseLayoutOptions
        abstract name: string with get, set // 'cola'
        /// whether to show the layout as it's running (default true)
        abstract animate: bool option with get, set
        /// number of ticks per frame; higher is faster but more jerky (default 1)
        abstract refresh: float option with get, set
        /// max length in ms to run the layout (default 4000)
        abstract maxSimulationTime: float option with get, set
        /// so you can't drag nodes during layout (default false)
        abstract ungrabifyWhileSimulating: bool option with get, set
        /// on every layout reposition of nodes, fit the viewport (default true)
        abstract fit: bool option with get, set
        /// padding around the simulation (default 30)
        abstract padding: float option with get, set
        /// constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }
        abstract boundingBox: U2<BoundingBox12, BoundingBoxWH> option with get, set
        /// whether labels should be included in determining the space used by a node (default false)
        abstract nodeDimensionsIncludeLabels: bool option with get, set
        /// positioning options
        /// use random node positions at beginning of layout (default false)
        abstract randomize:  bool option with get, set
        /// if true, prevents overlap of node bounding boxes (default true)
        abstract avoidOverlap: bool option with get, set
        /// if true, avoids disconnected components from overlapping (default true)
        abstract handleDisconnected: bool option with get, set
        /// when the alpha value (system energy) falls below this value, the layout stops (derfault 0.01)
        abstract convergenceThreshold: float option with get, set
        /// extra spacing around nodes
        abstract nodeSpacing: (NodeDefinition -> float) option with get, set
        /// use DAG/tree flow layout if specified, e.g. { axis: 'y', minSeparation: 30 }
        abstract flow: FlowOptions option with get, set
        /// relative alignment constraints on nodes, e.g. function( node ){ return { x: 0, y: 1 } }
        abstract alignment: (NodeDefinition -> Position) option with get, set
        /// list of inequality constraints for the gap between the nodes, e.g. [{"axis":"y", "left":node1, "right":node2, "gapct ":25}]
        //abstract gapInequalities: undefined, 

        /// different methods of specifying edge length
        /// each can be a constant numerical value or a function like `function( edge ){ return 2; }`
        /// sets edge length directly in simulation
        abstract edgeLength: U2<float, (EdgeDefinition -> float)> option with get, set
        /// different methods of specifying edge length
        /// each can be a constant numerical value or a function like `function( edge ){ return 2; }`
        /// symmetric diff edge length in simulation
        abstract edgeSymDiffLength: U2<float, (EdgeDefinition -> float)> option with get, set
        /// different methods of specifying edge length
        /// each can be a constant numerical value or a function like `function( edge ){ return 2; }`
        /// jaccard edge length in simulation
        abstract edgeJaccardLength: U2<float, (EdgeDefinition -> float)> option with get, set

        /// iterations of cola algorithm; uses default values on undefined
        /// unconstrained initial layout iterations
        abstract unconstrIter: float option with get, set
        /// iterations of cola algorithm; uses default values on undefined
        /// initial layout iterations with user-specified constraints
        abstract userConstIter: float option with get, set
        /// iterations of cola algorithm; uses default values on undefined
        /// initial layout iterations with all constraints including non-overlap
        abstract allConstIter: float option with get, set

        /// infinite layout options
        /// overrides all other options for a forces-all-the-time mode (default false)
        abstract infinite: bool option with get, set 