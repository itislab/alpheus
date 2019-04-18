open Giraffe
open Giraffe.Serialization
open Microsoft.Extensions.DependencyInjection
open Saturn
open System.IO
open Thoth.Json.Giraffe
open System.Diagnostics
open Elmish.Bridge

let clientPath = "Client" |> Path.GetFullPath
let port = 8080us
let addr = "http://localhost:" + port.ToString() + "/"

let browserRouter = router {
    get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

//let mainRouter = router {
//    forward Shared.BridgeInfo.endpoint (Socket.server ())
//    forward "" browserRouter }

//let config (services:IServiceCollection) =
//    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer())

let app path = application {
    //use_router mainRouter
    use_router (Socket.server path)
    url addr
    memory_cache 
    use_static clientPath
    app_config Giraffe.useWebSockets
    //service_config config
    use_gzip }

[<EntryPoint>]
let main args =
    if args.Length <> 1 then failwith "Path to the repository must be provided as an argument"
    let browsePSI = ProcessStartInfo (addr)
    browsePSI.UseShellExecute <- true
    Process.Start(browsePSI) |> ignore // Race condition here - we need server to start before the browser will try to reach it. Fine for the prototype.
    run (app args.[0])
    0