module ReactCytoscape

open Fable.Core
open Fable.React
open Fable.Core.JsInterop
open Cytoscape

type [<AllowNullLiteral>] CytoscapeComponentStyle =
    abstract width: string with get, set
    abstract height: string with get, set

type CytoscapeComponentProps =
    /// The flat list of Cytoscape elements to be included in the graph, each represented as non-stringified JSON.
    | Elements of Cytoscape.ElementDefinition array
    /// The Cytoscape stylesheet.
    | Stylesheet of Cytoscape.StylesheetStyle array
    /// Use a layout to automatically position the nodes in the graph.
    | Layout of Cytoscape.LayoutOptions
    (* This prop allows for getting a reference to the Cytoscape cy reference using a React ref function. This cy reference can be used to access the Cytoscape API directly. E.g.:

class MyApp extends React.Component {
  render() {
    return <CytoscapeComponent cy={cy => this.cy = cy}>;
  }
} *)
    //| Cy
    /// The panning position of the graph.
    | Pan of Cytoscape.Position
    /// The zoom level of the graph.
    | Zoom of float
    /// Whether the panning position of the graph is mutable overall.
    | PanningEnabled of bool
    /// Whether the panning position of the graph is mutable by user gestures such as swiping.
    | UserPanningEnabled of bool
    /// The minimum zoom level of the graph.
    | MinZoom of float
    /// The maximum zoom level of the graph.
    | MaxZoom of float
    /// Whether the zoom level of the graph is mutable overall.
    | ZoomingEnabled of float
    /// Whether the zoom level of the graph is mutable by user gestures (e.g. pinch-to-zoom).
    | UserZoomingEnabled of float
    /// Whether shift+click-and-drag box selection is enabled.
    | BoxSelectionEnabled of bool
    /// If true, nodes automatically can not be grabbed regardless of whether each node is marked as grabbable
    | Autoungrabify of bool
    /// If true, nodes can not be moved at all.
    | Autolock of bool
    /// If true, elements have immutable selection state.
    | Autounselectify of bool
    /// The id attribute of the div.
    | Id of string
    /// The class attribute of the div containing space-separated class names.
    | ClassName of string
    /// The style attribute of the div containing CSS styles
    | Style of CytoscapeComponentStyle

let inline cytoscapeComponent (props: CytoscapeComponentProps list) : ReactElement =
    ofImport "default" "react-cytoscapejs/src" (keyValueList CaseRules.LowerFirst props) []

