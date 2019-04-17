namespace Shared

type LatLong =
    { Latitude : float
      Longitude : float }
type Location =
    { Town : string
      Region : string
      LatLong : LatLong }

type PostcodeRequest = { Postcode : string }

type LocationResponse = { Postcode : string; Location : Location; DistanceToLondon : float }

type ServerMsg =
    | GiveLocation of PostcodeRequest

type ClientMsg =
    | GetLocation of LocationResponse

module BridgeInfo =
    let endpoint = "/socket"

/// Provides validation on data. Shared across both client and server.
module Validation =
    open System.Text.RegularExpressions
    let validatePostcode postcode =
        Regex.IsMatch(postcode, @"([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2})")