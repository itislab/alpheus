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
    { graph = { artefacts = Set.empty; methods = Set.empty } }, Cmd.ofMsg Init
    //{ graph = { nodes = []; edges = [] } }, Cmd.ofMsg Init
    
 
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

//let defineNode (node: Node) =
//    let def = createEmpty<NodeDefinition>
//    let dataDef = createEmpty<NodeDataDefinition>
//    dataDef.id <- Some node.id
//    dataDef.["label"] <- Some (upcast (match node.label with Some l -> l | None -> node.id))
//    def.data <- dataDef
//    match node.kind with
//    | NodeKind.Artefact ->
//        def.classes <- Some "artefact"
//    | NodeKind.Method ->
//        def.classes <- Some "method"
//    def

//let defineEdge (edge: Edge) =
//    let def = createEmpty<EdgeDefinition>
//    let dataDef = createEmpty<EdgeDataDefinition>
//    dataDef.id <- Some edge.id
//    match edge.label with
//    | Some l ->
//        dataDef.["label"] <- Some (upcast l)
//    | None -> ()
//    dataDef.source <- edge.source.id
//    dataDef.target <- edge.target.id
//    def.data <- dataDef
//    def

let sourceContainerId = "341DDCD4"

let sourceContainer =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some sourceContainerId
    dataDef.["label"] <- Some (upcast "Source data")
    def.data <- dataDef
    def.classes <- Some "artefact"
    def

let defineArtefactNode (artefact: ArtefactVertex) isSource =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some artefact.id
    dataDef.["label"] <- Some (upcast (match artefact.label with Some l -> l | None -> artefact.id))
    if isSource then
        dataDef.parent <- Some sourceContainerId
    else
        dataDef.parent <- Some artefact.source
    def.data <- dataDef
    def.classes <- Some "artefact"
    def

let defineMethodNode (method: ComputedVertex) =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some method.id
    dataDef.["label"] <- Some (upcast (match method.label with Some l -> l | None -> method.id))
    def.data <- dataDef
    def.classes <- Some "method"
    def

let defineEdges (artefact: ArtefactVertex) = seq {
    for dependant in artefact.dependants ->
        let def = createEmpty<EdgeDefinition>
        let dataDef = createEmpty<EdgeDataDefinition>
        dataDef.id <- Some (artefact.id + " -> " + dependant)
        dataDef.source <- artefact.id
        dataDef.target <- dependant
        def.data <- dataDef
        def
}

let artefactIsSource (methodMap: Map<VertexId, ProducerVertex>) (artefact: ArtefactVertex) =
    let parent = methodMap.[artefact.source]
    match parent with
    | Source _ -> true
    | Computed _ -> false

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Item.div [] [
                Heading.h1 [] [ str "ALPHEUS" ] ]
            ]
        
        Container.container [] [
            
            //yield Button.button [
            //    Button.OnClick (fun _ -> Bridge.Send DoNothing)
            //    ] [str "add node"]
            
            
            let graph = model.graph
            if graph.artefacts.Count = 0 && graph.methods.Count = 0
            then
                ()
            else
                let methodMap = graph.methods |> Seq.map (fun m -> match m with Source s -> (s.id, m) | Computed c -> (c.id, m)) |> Map.ofSeq
                let artefactIsSourceMask = graph.artefacts |> Seq.map (artefactIsSource methodMap)
                let artefactNodeDefs = graph.artefacts |> Seq.map2 defineArtefactNode <| artefactIsSourceMask
                let methodNodeDefs = graph.methods |> Seq.choose (fun m-> match m with Source _ -> None | Computed c -> Some c) |> Seq.map defineMethodNode
                //let nodeDefs = graph.nodes |> Seq.map defineNode
                //let edgeDefs = graph.edges |> Seq.map defineEdge
                let nodeDefs = Seq.append artefactNodeDefs methodNodeDefs

                let edgeDefs = graph.artefacts |> Seq.collect defineEdges

                let defs = Seq.append (Seq.cast<ElementDefinition> nodeDefs) (Seq.cast<ElementDefinition> edgeDefs) |> Array.ofSeq

                let nodeStyle = createEmpty<StylesheetStyle>
                nodeStyle.selector <- "node"
                let nodeCss = createEmpty<Css.Node>
                nodeCss.shape <- Some Css.NodeShape.Roundrectangle
                nodeCss.height <- Some (U2.Case2 "label")
                nodeCss.width <- Some (U2.Case2 "label")
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
                nodeCss.``text-wrap`` <- Some Css.TextWrap.Wrap
                nodeStyle.style <- U2.Case1 nodeCss
                let edgeStyle = createEmpty<StylesheetStyle>
                edgeStyle.selector <- "edge"
                let edgeCss = createEmpty<Css.Edge>
                edgeCss.width <- Some (U2.Case1 1.0)
                edgeCss.``curve-style`` <- Some Css.CurveStyle.Bezier
                edgeCss.``target-arrow-shape`` <- Some Css.ArrowShape.Triangle
                edgeCss.``target-arrow-fill`` <- Some Css.ArrowFill.Filled
                edgeCss.``target-arrow-color`` <- Some "black"
                edgeCss.label <- Some "data(label)"
                edgeStyle.style <- U2.Case2 edgeCss
                let artefactStyle = createEmpty<StylesheetStyle>
                artefactStyle.selector <- ".artefact"
                let artefactCss = createEmpty<Css.Node>
                artefactCss.``background-color`` <- Some "gray"
                artefactStyle.style <- U2.Case1 artefactCss
                let methodStyle = createEmpty<StylesheetStyle>
                methodStyle.selector <- ".method"
                let methodCss = createEmpty<Css.Node>
                methodCss.``background-color`` <- Some "blue"
                methodStyle.style <- U2.Case1 methodCss
                let layoutOpts = createEmpty<CytoscapeDagre.DagreLayoutOptions>
                layoutOpts.name <- "dagre"
                layoutOpts.rankDir <- Some CytoscapeDagre.DagreRankDir.LeftToRight
                layoutOpts.ranker <- Some CytoscapeDagre.DagreRanker.LongestPath
                layoutOpts.rankSep <- Some 200.0
                layoutOpts.nodeDimensionsIncludeLabels <- true
                let divStyle = createEmpty<ReactCytoscape.CytoscapeComponentStyle>
                divStyle.height <- "1000px"
                divStyle.width <- "1600px"
                yield
                    ReactCytoscape.cytoscapeComponent [
                            ReactCytoscape.Elements defs
                            ReactCytoscape.Stylesheet [| nodeStyle; edgeStyle; artefactStyle; methodStyle |]
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