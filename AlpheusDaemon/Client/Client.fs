module Client

open Elmish
open Elmish.React
open Elmish.Bridge
open App
open Shared
 
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridgeConfig(
    Bridge.endpoint BridgeInfo.endpoint
    |> Bridge.withMapping
        (fun bridgeMsg ->
            match bridgeMsg with
            | GetLocation loc -> GotReport { Report.Location = loc } ))
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run