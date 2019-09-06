module ItisLab.Alpheus.Versioning

open Newtonsoft.Json.Linq

let AlphFileCurrentVersion = 1
let ExperimentConfigFileCurrentVersion = 1

/// extracts the version from JSON text
/// "Version" field MUST be in the root of the JSON
let getVersion jsonText =
    let root = JObject.Parse(jsonText)
    let versionToken = root.GetValue("FileFormatVersion")
    let version = versionToken.ToObject<int>()
    version