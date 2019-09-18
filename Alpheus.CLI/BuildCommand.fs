module ItisLab.Alpheus.BuildCommand

open Argu
open ItisLab.Alpheus.CLI
open System

let run workingDir (buildArgs:ParseResults<BuildArgs>) = 
    result {
        let deps = buildArgs.GetResults <@ D @>
        let outputs = buildArgs.GetResults <@ O @>
        
        do! outputs.Length > 0, UserError "Outputs are not specified"
        let unrecognized = buildArgs.UnrecognizedCliParams
        do! unrecognized.Length > 0, UserError "Command is not specified"

        let command = String.Join(' ', unrecognized) // buildArgs.TryGetResult <@ Command @>
        // Checking that both dependencies and outputs are under the same experiment folder
        let allPathParams = List.append deps outputs
        let roots = List.map Config.tryLocateExperimentRoot allPathParams |> List.distinct
        Logger.logVerbose Logger.CLI (sprintf "Found following experiment roots among the supplied artefact paths: %A" roots)

        let settings = 
            { 
                DependencyGraph.DefaultExecutionSettings with
                    DoNotCleanOutputs = buildArgs.Contains Disable_Outputs_Clean
            }

        if settings.DoNotCleanOutputs then
            Logger.logVerbose Logger.CLI "Clearing of outputs is disabled for this command execution"

        let settings2 =
            match buildArgs.TryGetResult Successful_Exit_Codes with
            |   Some(codes) ->
                Logger.logVerbose Logger.CLI (sprintf "Custom exit codes are considered as successful exit: %A" codes)
                {settings with SuccessfulExitCodes = codes }
            |   None ->
                settings

        match roots with
        | [] -> return! Error (UserError "An output artefact is not specified")
        | [None] -> return! Error (UserError "Not an experiment folder: .alpheus")
        | [Some experimentRoot] -> return! (API.buildAsync experimentRoot workingDir deps outputs command settings2 |> Async.RunSynchronously)
        | _ -> return! Error (UserError "Not all of the input or output artefacts are under the same experiment root folder")
    }