module Client

open Elmish
open Elmish.React
open Elmish.Bridge
open App
open Shared
open Fable.Core
 
#if DEBUG
open Elmish.Debug
open Elmish.HMR

#endif

Cytoscape.cytoscapeModuleImport.``use`` CytoscapeKlay.klay
//Cytoscape.cytoscapeModuleImport.``use`` CytoscapeExpandCollapse.expandCollapse
CytoscapeExpandCollapse.expandCollapse.Invoke (Cytoscape.cytoscape, CytoscapeExpandCollapse.jquery)
Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridge BridgeInfo.endpoint
//|> Program.withBridgeConfig(
//    Bridge.endpoint BridgeInfo.endpoint
//    |> Bridge.withMapping
//        (fun bridgeMsg ->
//            match bridgeMsg with
//            | GetLocation loc -> GotReport { Report.Location = loc } ))
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withDebugger
#endif
|> Program.run