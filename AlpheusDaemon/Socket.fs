module Socket

open Elmish
open Elmish.Bridge
open Shared

let init _ _ = 0, Cmd.none

let update clientDispatch msg _ =
    match msg with
    | GiveLocation req -> clientDispatch( GetLocation { Postcode = req.Postcode; Location = { Location.Town = "Hell"; Region = "Shithole country"; LatLong = { Latitude = 51.5074; Longitude = 0.1278 } }; DistanceToLondon = 5.0 } )
    0, Cmd.none

let server () =
    //Bridge.mkServer "" init update
    Bridge.mkServer Shared.BridgeInfo.endpoint init update
    |> Bridge.run Giraffe.server