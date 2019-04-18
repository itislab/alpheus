module App

open Elmish

open Fable
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack

open Fulma

open Shared
open Thoth.Json

open Elmish.Bridge

open Cytoscape
open Cytoscape.Cytoscape
open Fable.Core
open Fable.Import

open Fable.PowerPack

type ServerState = Idle | Loading | ServerError of string

/// The init function is called to start the message pump with an initial view.
let init () = 
    { graph = { nodes = []; edges = [] } }, Cmd.ofMsg Init
    
 
/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | state, Init ->
        //promise {
        //    do! Promise.sleep 3000
            
        //    return (Bridge.Send DoNothing)
        //}
        //|> Promise.start
        //Bridge.Send DoNothing
        state, Cmd.none
    | _, StateUpdated state -> state, Cmd.none

[<AutoOpen>]
module ViewParts =
    let basicTile title options content =
        Tile.tile options [
            Notification.notification [ Notification.Props [ Style [ Height "100%"; Width "100%" ] ] ]
                (Heading.h2 [] [ str title ] :: content)
        ]
    let childTile title content =
        Tile.child [ ] [
            Notification.notification [ Notification.Props [ Style [ Height "100%"; Width "100%" ] ] ]
                (Heading.h2 [ ] [ str title ] :: content)
        ]             

let defineNode (node: Node) =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some node.id
    dataDef.["label"] <- Some (upcast (match node.label with Some l -> l | None -> node.id))
    def.data <- dataDef
    def

let defineEdge (edge: Edge) =
    let def = createEmpty<EdgeDefinition>
    let dataDef = createEmpty<EdgeDataDefinition>
    dataDef.id <- Some edge.id
    match edge.label with
    | Some l ->
        dataDef.["label"] <- Some (upcast l)
    | None -> ()
    dataDef.source <- edge.source.id
    dataDef.target <- edge.target.id
    def.data <- dataDef
    def

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Item.div [] [
                Heading.h1 [] [ str "ALPHEUS" ] ]
            ]
        
        Container.container [] [
            
            
            let graph = model.graph
            match graph.nodes with
            | [] -> ()
            | _ ->
                let nodeDefs = graph.nodes |> Seq.map defineNode
                let edgeDefs = graph.edges |> Seq.map defineEdge
                let defs = Seq.append (Seq.cast<ElementDefinition> nodeDefs) (Seq.cast<ElementDefinition> edgeDefs) |> Array.ofSeq
                let nodeStyle = createEmpty<StylesheetStyle>
                nodeStyle.selector <- "node"
                let nodeCss = createEmpty<Css.Node>
                nodeCss.shape <- Some Css.NodeShape.Roundrectangle
                nodeCss.height <- Some (U2.Case2 "label") //Some (U2.Case1 20.0)
                nodeCss.width <- Some (U2.Case2 "label") //Some (U2.Case1 20.0)
                nodeCss.label <- Some "data(label)"
                nodeCss.``text-halign`` <- Some Css.TextHAlign.Center
                nodeCss.``text-valign`` <- Some Css.TextVAlign.Center
                nodeCss.``padding-bottom`` <- Some "3"
                nodeCss.``padding-top`` <- Some "3"
                nodeCss.``padding-left`` <- Some "3"
                nodeCss.``padding-right`` <- Some "3"
                nodeCss.``border-width`` <- Some 1.0
                nodeCss.``border-style`` <- Some Css.LineStyle.Solid
                nodeCss.``border-color`` <- Some "black"
                nodeCss.``border-opacity`` <- Some 1.0
                nodeStyle.style <- U2.Case1 nodeCss
                let edgeStyle = createEmpty<StylesheetStyle>
                edgeStyle.selector <- "edge"
                let edgeCss = createEmpty<Css.Edge>
                edgeCss.width <- Some (U2.Case1 1.0)
                edgeCss.``curve-style`` <- Some Css.CurveStyle.Bezier
                edgeCss.``target-arrow-shape`` <- Some Css.ArrowShape.Triangle
                edgeCss.``target-arrow-fill`` <- Some Css.ArrowFill.Filled
                edgeCss.``target-arrow-color`` <- Some "black"
                //edgeCss.``arrow-scale`` <- Some 5.0
                edgeCss.label <- Some "data(label)"
                edgeStyle.style <- U2.Case2 edgeCss
                let layoutOpts = createEmpty<NullLayoutOptions> // createEmpty<GridLayoutOptions>
                layoutOpts.name <- "dagre" //"grid"
                //layoutOpts.rows <- Some 1.0
                //layoutOpts.cols <- Some 2.0
                let divStyle = createEmpty<ReactCytoscape.CytoscapeComponentStyle>
                divStyle.height <- "600px"
                divStyle.width <- "600px"
                yield
                    ReactCytoscape.cytoscapeComponent [
                            ReactCytoscape.Elements defs
                            ReactCytoscape.Stylesheet [| nodeStyle; edgeStyle |]
                            ReactCytoscape.Style divStyle
                            ReactCytoscape.Layout layoutOpts
                        ]
        ]

        br [ ]

        Footer.footer [] [
            Content.content
                [ Content.Modifiers [ Fulma.Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                [ str "ALPHEUS" ]
        ]
    ]