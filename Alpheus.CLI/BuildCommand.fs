module ItisLab.Alpheus.BuildCommand

open Argu
open ItisLab.Alpheus.CLI
open ItisLab.Alpheus.Trace
open System

let run (buildArgs:ParseResults<BuildArgs>) = 
    let deps = buildArgs.GetResults <@ D @>
    traceVerbose(sprintf "Dependencies: %A" deps)
    let outputs = buildArgs.GetResults <@ O @>
    traceVerbose(sprintf "Outputs: %A" outputs)
    let doNotCleanOutputs = buildArgs.Contains Disable_Outputs_Clean
    if List.length outputs = 0 then
        raise(ArgumentException("You need to specify at least one output"))
    let unrecognized = buildArgs.UnrecognizedCliParams

    if List.length unrecognized = 0 then
        raise(ArgumentException("You need to specify a command"))
    else
        let command = String.Join(' ',unrecognized) // buildArgs.TryGetResult <@ Command @>
        // Checking that both dependencies and outputs are under the same experiment folder
        let allPathParams = List.append deps outputs
        let roots = List.map Config.tryLocateExpereimentRoot allPathParams |> List.distinct
        traceVerbose(sprintf "Found following experiment roots among the supplied artefact paths: %A" roots)
        match List.length roots with
        | 0 -> invalidArg "Neither input nor output artefact is specified" |> ignore
        | 1 -> () // ok, single root
        | _ -> raise(ArgumentException("Not all of the artefacts (inputs, outputs) are under the same experiment root folder"))            

        // generating leafs full alpheus path
        match List.exactlyOne roots with
        | None -> invalidOp "Not an experiment folder: .alpheus"
        | Some experimentRoot -> API.buildAsync experimentRoot deps outputs command doNotCleanOutputs |> Async.RunSynchronously
        0
