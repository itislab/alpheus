module CytoscapeExpandCollapse

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

//let [<Import("default","cytoscape-expand-collapse")>] expandCollapse: Cytoscape.Ext = jsNative
//?options: Cytoscape.CytoscapeOptions -> Cytoscape.Core

type [<AllowNullLiteral>] JQueryForCytoscapeExpandCollapse = interface end

let [<Import("default","jquery")>] jquery: JQueryForCytoscapeExpandCollapse = jsNative
let [<Import("default","cytoscape-expand-collapse")>] expandCollapse: System.Action<(Cytoscape.CytoscapeOptions -> Cytoscape.Core), JQueryForCytoscapeExpandCollapse> = jsNative

type [<StringEnum>] [<RequireQualifiedAccess>] CuePosition =
    | [<CompiledName "top-left">] TopLeft
    | [<CompiledName "top-right">] TopRight
    | [<CompiledName "bottom-left">] BottomLeft
    | [<CompiledName "bottom-right">] BottomRight

type [<AllowNullLiteral>] Options =
    /// to rearrange after expand/collapse. It's just layout options or whole layout function. Choose your side!
    /// recommended usage: use cose-bilkent layout with randomize: false to preserve mental map upon expand/collapse
    /// default: null
    abstract layoutBy: LayoutOptions option with get, set
    /// whether to perform fisheye view after expand/collapse you can specify a function too
    /// default: true
    abstract fisheye: bool option with get, set
    /// whether to animate on drawing changes you can specify a function too
    /// default: true
    abstract animate: bool option with get, set
    /// callback when expand/collapse initialized
    /// default: do nothing
    //abstract ready: function () { }, 
    /// and if undoRedoExtension exists,
    /// default: true
    abstract undoable: bool option with get, set
    /// Whether cues are enabled
    /// default: true
    abstract cueEnabled: bool option with get, set
    /// default cue position is top left you can specify a function per node too
    /// default: 'top-left'
    abstract expandCollapseCuePosition: CuePosition option with get, set
    /// size of expand-collapse cue
    /// default: 12
    abstract expandCollapseCueSize: float option with get, set
    /// size of lines used for drawing plus-minus icons
    /// default: 8
    abstract expandCollapseCueLineSize: float option with get, set
    /// image of expand icon if undefined draw regular expand cue
    /// default: undefined
    //abstract expandCueImage: undefined, 
    /// image of collapse icon if undefined draw regular collapse cue
    /// default: undefined
    //abstract collapseCueImage: undefined, 
    /// sensitivity of expand-collapse cues
    /// default: 1
    abstract expandCollapseCueSensitivity: float option with get, set

type [<AllowNullLiteral>] ExpandCollapseApi =
    /// Collapse given nodes, extend options with given param.
    abstract collapse: nodes: CollectionReturnValue * ?options: Options -> unit
    /// Collapse given nodes recursively, extend options with given param.
    abstract collapseRecursively: nodes: CollectionReturnValue * ?options: Options -> unit
    /// Collapse all nodes on graph (recursively), extend options with given param.
    abstract collapseAll: ?options: Options -> unit
    /// Expand given nodes, extend options with given param.
    abstract expand: nodes: CollectionReturnValue * ?options: Options -> unit
    /// Expand given nodes recursively, extend options with given param.
    abstract expandRecursively: nodes: CollectionReturnValue * ?options: Options -> unit
    /// Expand all nodes on graph (recursively), extend options with given param.
    abstract expandAll: ?options: Options -> unit
    /// Get whether node is expandable (or is collapsed)
    abstract isExpandable: node: NodeSingular -> unit
    /// Get whether node is collapsible.
    abstract isCollapsible: node: NodeSingular -> unit
    /// Get expandable ones inside given nodes if nodes parameter is not specified consider all nodes
    abstract expandableNodes: nodes: CollectionReturnValue -> unit
    /// Get collapsible ones inside given nodes if nodes parameter is not specified consider all nodes
    abstract collapsibleNodes: nodes: CollectionReturnValue -> unit
    /// Resets the options to the given parameter.
    abstract setOptions: ?options: Options -> unit
    /// Sets the value of the option given by the name to the given value.
    abstract setOption: name: string * value: string -> unit
    /// Get the children of the given collapsed node which are removed during collapse operation
    abstract getCollapsedChildren: node: NodeSingular -> unit
    /// Get collapsed children recursively including nested collapsed children. Returned value includes edges and nodes, use selector to get edges or nodes.
    abstract getCollapsedChildrenRecursively: node: NodeSingular -> unit
    /// Get collapsed children of all collapsed nodes recursively. Returned value includes edges and nodes, use selector to get edges or nodes.
    abstract getAllCollapsedChildrenRecursively: unit -> unit
    /// Forces the visual cue to be cleared. It is to be called in extreme cases.
    abstract clearVisualCue: unit -> unit

/// You can downcast core to this if use was called
type [<AllowNullLiteral>] CoreWithExpandCollapse =
    inherit Core
    abstract expandCollapse: options: Options -> ExpandCollapseApi
