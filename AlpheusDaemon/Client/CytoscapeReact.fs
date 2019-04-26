module CytoscapeReact

open Fable.Core
open Fable.React
open Fable.React.Props
open Shared
open Cytoscape
open Fable.Core.JsInterop
open Cytoscape.Cytoscape

open Browser.Types

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
        dataDef.["label"] <- Some (upcast "")
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

let vertexA = Shared.Computed { id = "A"; label = Some "A"; inputs = []; outputs = [ "C" ]; command = ""; workingDirectory = "" }
let vertexB = Shared.Computed { id = "B"; label = Some "B"; inputs = [ "C" ]; outputs = []; command = ""; workingDirectory = "" }
let vertexC = { ArtefactVertex.id ="C"; label = Some "C"; source = "A"; dependants = [ (*"B"*) ] }
let graphTest = {
    artefacts = [ vertexC ]
    methods = [ vertexA; vertexB ]
}

let buildCytoscapeGraph (graph: AlpheusGraph) =
    if graph.artefacts.IsEmpty && graph.methods.IsEmpty
        then
            [||]
        else
            let methodMap = graph.methods |> Seq.map (fun m -> match m with Source s -> (s.id, m) | Computed c -> (c.id, m)) |> Map.ofSeq
            let artefactIsSourceMask = graph.artefacts |> Seq.map (artefactIsSource methodMap)
            let artefactNodeDefs = graph.artefacts |> Seq.map2 defineArtefactNode <| artefactIsSourceMask
            let methodNodeDefs = graph.methods |> Seq.choose (fun m-> match m with Source _ -> None | Computed c -> Some c) |> Seq.map defineMethodNode
            let nodeDefs = Seq.append artefactNodeDefs methodNodeDefs |> Seq.append (seq { yield sourceContainer })

            let edgeDefs = graph.artefacts |> Seq.collect defineEdges

            printf "nodes:"
            nodeDefs |> Seq.iter (fun n -> printfn "%s" n.data.id.Value)
            printf "# edges: %d" (Seq.length edgeDefs)
            edgeDefs |> Seq.iter (fun e -> printfn "%s -> %s" e.data.source e.data.target)

            if (edgeDefs |> Seq.exists (fun e -> not (nodeDefs |> Seq.exists (fun n -> n.data.id.Value = e.data.target)))) then
                printf "Bad edge(s) present"

            Seq.concat [
                Seq.cast<ElementDefinition> nodeDefs
                Seq.cast<ElementDefinition> edgeDefs ] |> Array.ofSeq
    |> ResizeArray

let styles =
    let nodeStyle = createEmpty<Stylesheet>
    nodeStyle.selector <- "node"
    let nodeCss = createEmpty<Css.Node>
    nodeCss.shape <- Some Css.NodeShape.Roundrectangle
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

    let containerStyle = createEmpty<Stylesheet>
    containerStyle.selector <- ":parent"
    let containerCss = createEmpty<Css.Node>
    containerCss.``text-valign`` <- Some Css.TextVAlign.Top
    containerCss.``background-opacity`` <- Some 0.333
    containerStyle.style <- U2.Case1 containerCss

    let edgeStyle = createEmpty<Stylesheet>
    edgeStyle.selector <- "edge"
    let edgeCss = createEmpty<Css.Edge>
    //edgeCss.width <- Some (U2.Case1 1.0)
    edgeCss.``curve-style`` <- Some Css.CurveStyle.Bezier
    edgeCss.``target-arrow-shape`` <- Some Css.ArrowShape.Triangle
    edgeCss.``target-arrow-fill`` <- Some Css.ArrowFill.Filled
    edgeCss.``target-arrow-color`` <- Some "black"
    //edgeCss.label <- Some "data(label)"
    edgeStyle.style <- U2.Case2 edgeCss

    let artefactStyle = createEmpty<Stylesheet>
    artefactStyle.selector <- ".artefact"
    let artefactCss = createEmpty<Css.Node>
    artefactCss.``background-color`` <- Some "gray"
    artefactStyle.style <- U2.Case1 artefactCss

    let methodStyle = createEmpty<Stylesheet>
    methodStyle.selector <- ".method"
    let methodCss = createEmpty<Css.Node>
    methodCss.``background-color`` <- Some "blue"
    methodStyle.style <- U2.Case1 methodCss

    ResizeArray [| nodeStyle; containerStyle; artefactStyle; methodStyle; edgeStyle |]

let layout =
    let layoutOpts = createEmpty<CytoscapeKlay.KlayLayoutOptions>
    layoutOpts.nodeDimensionsIncludeLabels <- true
    layoutOpts.name <- "klay"
    let klayOpts = createEmpty<CytoscapeKlay.KlayOptions>
    klayOpts.direction <- Some CytoscapeKlay.Direction.Right
    klayOpts.layoutHierarchy <- Some true
    klayOpts.nodePlacement <- Some CytoscapeKlay.NodePlacementStrategy.LinearSegments
    klayOpts.nodeLayering <- Some CytoscapeKlay.NodeLayeringStrategy.NetworkSimplex
    klayOpts.mergeHierarchyCrossingEdges <- Some true
    klayOpts.thoroughness <- Some 100.0
    layoutOpts.klay <- Some klayOpts
    layoutOpts

let expandCollapseOptions =
    let opts = createEmpty<CytoscapeExpandCollapse.Options>
    opts.layoutBy <- Some (upcast layout)
    opts.undoable <- Some false
    opts

type CytoscapeReactProps = { graph: AlpheusGraph }

let private _cytoscape (props: CytoscapeReactProps) =
    let selfRef = Hooks.useRef<Browser.Types.Element option> None
    let cyState = Hooks.useState<CytoscapeExpandCollapse.CoreWithExpandCollapse option> None
    Hooks.useEffectDisposable((fun () ->
        match cyState.current with
        | Some cy -> cy.destroy ()
        | None -> ()
        
        match selfRef.current with
        | Some ref ->
            let opts = createEmpty<CytoscapeOptions>
            opts.container <- Some (ref :?> HTMLElement) // ref is attached to a div, downcast is valid
            opts.elements <- Some (U4.Case2 (buildCytoscapeGraph props.graph))
            opts.style <- Some (U2.Case1 styles)
            opts.layout <- Some (upcast layout)
            let cy = Cytoscape.cytoscape opts :?> CytoscapeExpandCollapse.CoreWithExpandCollapse // necessary setup to make
            // this downcast valid is done in Client.fs
            cyState.update (Some cy)
            cy.expandCollapse expandCollapseOptions |> ignore
        | _ -> 
            cyState.update None
        

        { new System.IDisposable with
            member __.Dispose() = match cyState.current with Some cy -> cy.destroy () | None -> () }),
        [| props.graph |])
    div [ 
        RefHook selfRef
        Style [
            Width "1600px"
            Height "1000px"
        ] 
    ] []

let cytoscape =
    FunctionComponent.Of _cytoscape