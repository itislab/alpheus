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

//open ReactCytoscape

/// The different elements of the completed report.
type Report =
    { Location : LocationResponse }

type ServerState = Idle | Loading | ServerError of string

/// The overall data model driving the view.
type Model =
    { Postcode : string
      ValidationError : string option
      ServerState : ServerState
      Report : Report option }

/// The different types of messages in the system.
type Msg =
    | GetReport
    | PostcodeChanged of string
    | GotReport of Report
    | ErrorMsg of exn
    | Clear

/// The init function is called to start the message pump with an initial view.
let init () = 
    { Postcode = null
      Report = None
      ValidationError = None
      ServerState = Idle }, Cmd.ofMsg (PostcodeChanged "")

let decoderForLocationResponse = Thoth.Json.Decode.Auto.generateDecoder<LocationResponse> ()

let inline getJson<'T> (response:Fetch.Fetch_types.Response) = response.text() |> Promise.map Decode.Auto.unsafeFromString<'T>

let getResponse postcode = promise {
    let record = { Postcode = postcode }
    let! location = Fetch.postRecord "/api/distance/" record [] |> Promise.bind getJson<LocationResponse>
    //let! location = Fetch.fetchAs<LocationResponse> (sprintf "/api/distance/%s" postcode) decoderForLocationResponse []
    //let! crimes = Fetch.tryFetchAs (sprintf "api/crime/%s" postcode) decoderForCrimeResponse [] |> Promise.map (Result.defaultValue [||])
    //let! weather = Fetch.fetchAs<WeatherResponse> (sprintf "/api/weather/%s" postcode) decoderForWeatherResponse []
    (* Task 4.5 WEATHER: Fetch the weather from the API endpoint you created.
       Then, save its value into the Report below. You'll need to add a new
       field to the Report type first, though! *)
    return { Location = location } }
 
/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | { ValidationError = None; Postcode = postcode }, GetReport ->
        Bridge.Send (GiveLocation { Postcode = postcode })
        { model with ServerState = Loading }, Cmd.none // Cmd.ofPromise getResponse postcode GotReport ErrorMsg
    | _, GetReport -> model, Cmd.none
    | _, GotReport response ->
        { model with
            ValidationError = None
            Report = Some response
            ServerState = Idle }, Cmd.none
    | _, PostcodeChanged p ->
        { model with
            Postcode = p
            (* Task 2.2 Validation. Use the Validation.validatePostcode function to implement client-side form validation.
               Note that the validation is the same shared code that runs on the server! *)
            ValidationError = if (not (Validation.validatePostcode p)) then Some "BA-AD CODE!" else None }, Cmd.none
    | _, ErrorMsg e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _, Clear -> init ()

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

    let getBingMapUrl latLong =
        sprintf "https://www.bing.com/maps/embed?h=400&w=800&cp=%f~%f&lvl=11&typ=s&FORM=MBEDV8" latLong.Latitude latLong.Longitude

    let bingMapTile (latLong:LatLong) =
        basicTile "Map" [ Tile.Size Tile.Is12 ] [
            iframe [
                Style [ Height 410; Width 810 ]
                Src (getBingMapUrl latLong)
                (* Task 3.1 MAPS: Use the getBingMapUrl function to build a valid maps URL using the supplied LatLong.
                   You can use it to add a Src attribute to this iframe. *)
            ] [ ]
        ]

    let locationTile model =
        childTile "Location" [
            div [ ] [
                Heading.h3 [ ] [ str model.Location.Location.Town ]
                Heading.h4 [ ] [ str model.Location.Location.Region ]
                Heading.h4 [ ] [ sprintf "%.1fKM to London" model.Location.DistanceToLondon |> str ]
            ]
        ]
             

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Item.div [] [
                Heading.h1 [] [ str "Hi there! Location Review!" ] ]
            ]
        
        Container.container [] [
            yield 
                Field.div [] [
                    Label.label [] [ str "Postcode" ]
                    Control.div [ Control.HasIconLeft; Control.HasIconRight ] [
                        Input.text
                            [ Input.Placeholder "Ex: EC2A 4NE"
                              Input.Value model.Postcode
                              Input.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ]
                              Input.Color (if model.ValidationError.IsSome then Color.IsDanger else Color.IsSuccess)
                              Input.Props [ OnChange (fun ev -> dispatch (PostcodeChanged !!ev.target?value)); onKeyDown KeyCode.enter (fun _ -> dispatch GetReport) ] ]
                        Fulma.Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Home ] [] ]
                        (match model with
                         | { ValidationError = Some _ } -> 
                            Icon.icon [ Icon.Size IsSmall; Icon.IsRight ] [ Fa.i [ Fa.Solid.Exclamation ] [] ]
                         | { ValidationError = None } -> 
                            Icon.icon [ Icon.Size IsSmall; Icon.IsRight ] [ Fa.i [ Fa.Solid.Check ] [] ])
                    ]
                    Help.help
                       [ Help.Color (if model.ValidationError.IsNone then IsSuccess else IsDanger) ]
                       [ str (model.ValidationError |> Option.defaultValue "") ]
                ]
            yield
                Field.div [ Field.IsGrouped ] [
                    Level.level [ ] [
                        Level.left [] [
                            Level.item [] [
                                Button.button
                                    [ Button.IsFullWidth
                                      Button.Color IsPrimary
                                      Button.OnClick (fun _ -> dispatch GetReport)
                                      Button.Disabled (model.ValidationError.IsSome)
                                      Button.IsLoading (model.ServerState = ServerState.Loading) ]
                                    [ str "Submit" ] 
                                Button.button
                                    [ Button.IsFullWidth
                                      Button.Color IsPrimary
                                      Button.OnClick (fun _ -> dispatch Clear)
                                      Button.IsLoading (model.ServerState = ServerState.Loading) ]
                                    [ str "Clear" ] ] ] ]

                ]

            match model with
            | { Report = None; ServerState = (Idle | Loading) } -> ()
            | { ServerState = ServerError error } ->
                yield
                    Field.div [] [
                        Tag.list [ Tag.List.HasAddons; Tag.List.IsCentered ] [
                            Tag.tag [ Tag.Color Color.IsDanger; Tag.Size IsMedium ] [
                                str error
                            ]
                        ]
                    ]
            | { Report = Some model } ->
                yield
                    Tile.ancestor [ ] [
                        Tile.parent [ Tile.Size Tile.Is12 ] [
                            bingMapTile model.Location.Location.LatLong
                        ]
                    ]
            
            let pos1 = createEmpty<Position>
            pos1.x <- 0.0
            pos1.y <- 0.0
            let pos2 = createEmpty<Position>
            pos2.x <- 100.0
            pos2.y <- 0.0
            let node1 = createEmpty<NodeDefinition>
            let node1data = createEmpty<NodeDataDefinition>
            node1data.id <- Some "a"
            node1data.parent <- None
            node1data.position <- None
            node1data.["label"] <- Some (upcast "X")
            node1.data <- node1data
            let node2 = createEmpty<NodeDefinition>
            let node2data = createEmpty<NodeDataDefinition>
            node2data.id <- Some "b"
            node2data.parent <- None
            node2data.position <- None
            node2data.["label"] <- Some (upcast "B")
            node2.data <- node2data
            let edge1 = createEmpty<EdgeDefinition>
            let edge1data = createEmpty<EdgeDataDefinition>
            edge1data.id <- Some "ab"
            edge1data.source <- "a"
            edge1data.target <- "b"
            edge1data.["label"] <- Some (upcast "A -> B")
            edge1.data <- edge1data
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
            let layoutOpts = createEmpty<GridLayoutOptions>
            layoutOpts.name <- "grid"
            layoutOpts.rows <- Some 1.0
            layoutOpts.cols <- Some 2.0
            let divStyle = createEmpty<ReactCytoscape.CytoscapeComponentStyle>
            divStyle.height <- "600px"
            divStyle.width <- "600px"
            yield
                ReactCytoscape.cytoscapeComponent [
                        ReactCytoscape.Elements [| node1; node2; edge1 |]
                        ReactCytoscape.Stylesheet [| nodeStyle; edgeStyle |]
                        ReactCytoscape.Style divStyle
                        ReactCytoscape.Layout layoutOpts
                    ]
        ]

        br [ ]

        Footer.footer [] [
            Content.content
                [ Content.Modifiers [ Fulma.Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                [ safeComponents ]
        ]
    ]