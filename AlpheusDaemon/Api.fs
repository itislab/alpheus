module Api

open DataAccess
open FSharp.Data.UnitSystems.SI.UnitNames
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open Shared
open FSharp.Control.Tasks

let private london = { Latitude = 51.5074; Longitude = 0.1278 }
let invalidPostcode next (ctx:HttpContext) =
    ctx.SetStatusCode 400
    text "Invalid postcode" next ctx

let getDistanceFromLondon postcode next (ctx:HttpContext) = task {
    if Validation.validatePostcode postcode then
        let location = { Location.Town = "Hell"; Region = "Shithole country"; LatLong = london }
        let distanceToLondon = getDistanceBetweenPositions location.LatLong london
        return! json { Postcode = postcode; Location = location; DistanceToLondon = (distanceToLondon / 1000.<meter>) } next ctx
    else return! invalidPostcode next ctx }

let postDistanceFromLondon next (ctx:HttpContext) = task {
    let! req = ctx.BindModelAsync<PostcodeRequest> ()
    if Validation.validatePostcode req.Postcode then
        let location = { Location.Town = "Hell"; Region = "Shithole country"; LatLong = london }
        let distanceToLondon = getDistanceBetweenPositions location.LatLong london
        return! json { Postcode = req.Postcode; Location = location; DistanceToLondon = (distanceToLondon / 1000.<meter>) } next ctx
    else return! invalidPostcode next ctx }

let apiRouter = router {
    pipe_through (pipeline { set_header "x-pipeline-type" "Api" })
    getf "/distance/%s" getDistanceFromLondon
    post "/distance/" postDistanceFromLondon
    
    (* Task 1.0 CRIME: Add a new /crime/{postcode} endpoint to return crime data
       using the getCrimeReport web part function. Use the above distance
       route as an example of how to add a new route. *)    
        
    (* Task 4.2 WEATHER: Hook up the weather endpoint to the getWeather function. *)
    
    }