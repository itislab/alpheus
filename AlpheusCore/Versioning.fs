module ItisLab.Alpheus.Versioning

open Newtonsoft.Json.Linq
open System.IO

[<Literal>]
/// The format version of the alph files that the most resent
let  AlphFileCurrentVersion = 1
/// The format version of the config files that the most resent
[<Literal>]
let ExperimentConfigFileCurrentVersion = 1

/// extracts the version from JSON text
/// "Version" field MUST be in the root of the JSON
let getVersion jsonText =
    let root = JObject.Parse(jsonText)
    let versionToken = root.GetValue("FileFormatVersion")
    if versionToken = null then
        raise (InvalidDataException "Alph file must contain FileFormatVersion field")
    let version = versionToken.ToObject<int>()
    version