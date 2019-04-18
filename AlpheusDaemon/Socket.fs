module Socket

open Elmish
open Elmish.Bridge
open Shared

let nodeA = { id = "A"; label = Some "A" }
let nodeB = { id = "B"; label = Some "B" }

let edgeAB = {
    id = "AB"
    label = None
    source = nodeA
    target = nodeB
}

let testState = {
    graph = {
        nodes = [ nodeA; nodeB ]
        edges = [ edgeAB ]
    }
}

let init clientDispatch _ = 
    clientDispatch( StateUpdated testState )
    testState, Cmd.none

let update clientDispatch msg state =
    match msg with
    | DoNothing -> clientDispatch( StateUpdated state )
    state, Cmd.none

let server () =
    //Bridge.mkServer "" init update
    Bridge.mkServer Shared.BridgeInfo.endpoint init update
    |> Bridge.run Giraffe.server