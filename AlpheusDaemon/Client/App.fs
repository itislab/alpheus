module App

open Elmish

open Fable.React
open Fable.React.Props

open Fulma

open Shared


type ServerState = Idle | Loading | ServerError of string

/// The init function is called to start the message pump with an initial view.
let init () = 
    { graph = { artefacts = []; methods = [] } }, Cmd.ofMsg Init
    
 
/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | state, Init ->
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

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    div [] [
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Item.div [] [
                Heading.h1 [] [ str "ALPHEUS" ] ]
            ]
        
        Container.container [] [
            yield CytoscapeReact.cytoscape { graph = model.graph }
        ]

        br [ ]

        Footer.footer [] [
            Content.content
                [ Content.Modifiers [ Fulma.Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                [ str "ALPHEUS" ]
        ]
    ]