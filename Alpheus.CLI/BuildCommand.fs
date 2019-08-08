module ItisLab.Alpheus.BuildCommand

open Argu
open ItisLab.Alpheus.CLI
open ItisLab.Alpheus.Trace
open System

let run (buildArgs:ParseResults<BuildArgs>) = 
    result {
        let deps = buildArgs.GetResults <@ D @>
        let outputs = buildArgs.GetResults <@ O @>
        let doNotCleanOutputs = buildArgs.Contains Disable_Outputs_Clean

        do! outputs.Length > 0, "Outputs are not specified"
        let unrecognized = buildArgs.UnrecognizedCliParams
        do! unrecognized.Length > 0, "Command is not specified"

        let command = String.Join(' ', unrecognized) // buildArgs.TryGetResult <@ Command @>
        // Checking that both dependencies and outputs are under the same experiment folder
        let allPathParams = List.append deps outputs
        let roots = List.map Config.tryLocateExperimentRoot allPathParams |> List.distinct
        traceVerbose(sprintf "Found following experiment roots among the supplied artefact paths: %A" roots)

        match roots with
        | [] -> return! Error "An output artefact is not specified"
        | [None] -> return! Error "Not an experiment folder: .alpheus"
        | [Some experimentRoot] -> return! (API.buildAsync experimentRoot deps outputs command doNotCleanOutputs |> Async.RunSynchronously)
        | _ -> return! Error "Not all of the input or output artefacts are under the same experiment root folder"
    }