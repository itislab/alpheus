module CytoscapeReact

open Fable.Core
open Fable.React
open Fable.React.Props
open Shared
open Cytoscape
open Fable.Core.JsInterop
open Cytoscape.Cytoscape

open Browser.Types

let folderIdPrefix = "24A670DC"

let folderId folder = folderIdPrefix + folder

let sourceContainerId = "341DDCD4"

let sourceContainer =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some sourceContainerId
    dataDef.["label"] <- Some (upcast "Source data")
    def.data <- dataDef
    def.classes <- Some "folder"
    def

//let defineArtefactNode (artefact: ArtefactVertex) isSource =
//    let def = createEmpty<NodeDefinition>
//    let dataDef = createEmpty<NodeDataDefinition>
//    dataDef.id <- Some artefact.id
//    dataDef.["label"] <- Some (upcast (match artefact.label with Some l -> l | None -> artefact.id))
//    if isSource then
//        dataDef.parent <- Some sourceContainerId
//    else
//        dataDef.parent <- Some artefact.source
//    def.data <- dataDef
//    def.classes <- Some "artefact"
//    def

let trySplitParentFolder (path: string) =
    let parts = path.Split([|'\\'; '/'|])
    if parts.Length > 1 then
        Some ((System.String.Join("/", parts |> Array.take (parts.Length - 1))), Array.last parts)
    else
        None

let defineArtefactNode (artefact: ArtefactVertex) isSource =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some artefact.id
    dataDef.["label"] <- Some (upcast (match artefact.label with Some l -> l | None -> artefact.id))
    if isSource then
        match trySplitParentFolder artefact.id with
        | Some (parent, _) ->
            dataDef.parent <- Some (folderId parent)
        | None -> ()
    else
        dataDef.parent <- Some artefact.source
    def.data <- dataDef
    def.classes <- Some "artefact"
    def

let defineFolderNode (path: string) =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some (folderId path)
    
    match trySplitParentFolder path with
    | Some (parent, self) ->
        dataDef.parent <- Some (folderId parent)
        dataDef.["label"] <- Some (upcast self)
    | None ->
        dataDef.["label"] <- Some (upcast path)
    def.data <- dataDef
    def.classes <- Some "folder"
    def

let defineMethodNode (method: ComputedVertex) =
    let def = createEmpty<NodeDefinition>
    let dataDef = createEmpty<NodeDataDefinition>
    dataDef.id <- Some method.id
    dataDef.["label"] <- Some (upcast (match method.label with Some l -> l | None -> method.command.Split(' ').[0].Split([|'\\'; '/'|]) |> Array.last))
    match trySplitParentFolder method.outputs.Head with
    | Some (parent, _) ->
        dataDef.parent <- Some (folderId parent)
    | None -> ()
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

let defineEdgesMethodBased (artefact: ArtefactVertex) isSource = seq {
    for dependant in artefact.dependants ->
        let def = createEmpty<EdgeDefinition>
        let dataDef = createEmpty<EdgeDataDefinition>
        dataDef.id <- Some (artefact.id + " -> " + dependant)
        dataDef.["label"] <- Some (upcast "")
        dataDef.source <- if isSource then artefact.id else artefact.source
        dataDef.target <- dependant
        def.data <- dataDef
        def
}

let artefactIsSource (methodMap: Map<VertexId, ProducerVertex>) (artefact: ArtefactVertex) =
    let parent = methodMap.[artefact.source]
    match parent with
    | Source _ -> true
    | Computed _ -> false

let buildCytoscapeGraph (graph: AlpheusGraph) =
    if graph.artefacts.IsEmpty && graph.methods.IsEmpty
        then
            [||]
        else
            let getMentionedFolders (path: string) =
                let parts = path.Split([|'\\'; '/'|])
                seq { for i in 1..parts.Length - 1 -> System.String.Join ("/", parts |> Array.take i) }
            let folders = graph.artefacts |> Seq.collect (fun art -> getMentionedFolders art.id) |> Seq.distinct
            let folderNodeDefs = folders |> Seq.map defineFolderNode
            let methodMap = graph.methods |> Seq.map (fun m -> match m with Source s -> (s.id, m) | Computed c -> (c.id, m)) |> Map.ofSeq
            let artefactIsSourceMask = graph.artefacts |> Seq.map (artefactIsSource methodMap)
            let artefactNodeDefs = graph.artefacts |> Seq.map2 defineArtefactNode <| artefactIsSourceMask
            let methodNodeDefs = graph.methods |> Seq.choose (fun m-> match m with Source _ -> None | Computed c -> Some c) |> Seq.map defineMethodNode
            let nodeDefs = Seq.append artefactNodeDefs methodNodeDefs |> Seq.append folderNodeDefs //(seq { yield sourceContainer })

            //let edgeDefs = graph.artefacts |> Seq.collect defineEdges

            let edgeDefs = (graph.artefacts, artefactIsSourceMask) ||> Seq.zip  |> Seq.collect (fun (art, isSource) -> defineEdgesMethodBased art isSource)

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
    edgeCss.``curve-style`` <- Some Css.CurveStyle.UnbundledBezier
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

    let folderStyle = createEmpty<Stylesheet>
    folderStyle.selector <- ".folder"
    let folderCss = createEmpty<Css.Node>
    folderCss.``background-color`` <- Some "green"
    folderStyle.style <- U2.Case1 folderCss

    let methodStyle = createEmpty<Stylesheet>
    methodStyle.selector <- ".method"
    let methodCss = createEmpty<Css.Node>
    methodCss.``background-color`` <- Some "blue"
    methodStyle.style <- U2.Case1 methodCss

    ResizeArray [| nodeStyle; containerStyle; artefactStyle; methodStyle; folderStyle; edgeStyle |]

let klayLayout =
    let layoutOpts = createEmpty<CytoscapeKlay.KlayLayoutOptions>
    layoutOpts.nodeDimensionsIncludeLabels <- true
    layoutOpts.name <- "klay"
    //layoutOpts.avoidOverlap <- Some true
    let klayOpts = createEmpty<CytoscapeKlay.KlayOptions>
    klayOpts.direction <- Some CytoscapeKlay.Direction.Right
    klayOpts.layoutHierarchy <- Some true
    klayOpts.nodePlacement <- Some CytoscapeKlay.NodePlacementStrategy.Simple
    klayOpts.nodeLayering <- Some CytoscapeKlay.NodeLayeringStrategy.NetworkSimplex
    //klayOpts.mergeHierarchyCrossingEdges <- Some true
    klayOpts.edgeRouting <- Some CytoscapeKlay.EdgeRouting.Polyline
    klayOpts.spacing <- Some 20.0
    klayOpts.edgeSpacingFactor <- Some 0.3
    klayOpts.fixedAlignment <- Some CytoscapeKlay.Alignment.Balanced
    klayOpts.thoroughness <- Some 1000.0
    klayOpts.aspectRatio <- Some 1.0
    layoutOpts.klay <- Some klayOpts
    layoutOpts

let coseBilkentLayout =
    let layoutOpts = createEmpty<CytoscapeCoseBilkent.CoseBilkentLayoutOptions>
    layoutOpts.nodeDimensionsIncludeLabels <- true
    //layoutOpts.
    layoutOpts.name <- "cose-bilkent"
    layoutOpts.randomize <- Some false
    layoutOpts

let fCoseLayout =
    let layoutOpts = createEmpty<CytoscapeFCose.FCoseLayoutOptions>
    layoutOpts.name <- "fcose"
    layoutOpts.quality <- Some CytoscapeFCose.Quality.Proof
    layoutOpts.nodeDimensionsIncludeLabels <- Some true
    layoutOpts.randomize <- Some false
    layoutOpts.tile <- Some true
    layoutOpts.fit <- Some true
    layoutOpts.nodeRepulsion <- Some 40000.0
    layoutOpts.gravity <- Some 2.0
    layoutOpts.gravityCompound <- Some 5.0
    layoutOpts.edgeElasticity <- Some 2.0
    layoutOpts.numIter <- Some 20000.0
    layoutOpts.sampleSize <- Some 100.0
    layoutOpts.nodeSeparation <- Some 20.0
    layoutOpts

let colaLayout =
    let layoutOpts = createEmpty<CytoscapeCola.ColaLayoutOptions>
    layoutOpts.name <- "cola"
    layoutOpts.animate <- Some true
    layoutOpts.nodeDimensionsIncludeLabels <- Some true
    layoutOpts.randomize <- Some false
    layoutOpts.avoidOverlap <- Some true
    layoutOpts.handleDisconnected <- Some true
    let flow = createEmpty<CytoscapeCola.FlowOptions>
    flow.axis <- CytoscapeCola.Axis.Y
    flow.minSeparation <- Some 30.0
    layoutOpts.flow <- Some flow
    layoutOpts

let expandCollapseOptions =
    let opts = createEmpty<CytoscapeExpandCollapse.Options>
    opts.layoutBy <- Some (upcast colaLayout)
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
            opts.layout <- Some (upcast colaLayout)
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