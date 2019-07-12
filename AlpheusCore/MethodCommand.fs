module ItisLab.Alpheus.MethodCommand

open System
open System.Text.RegularExpressions
open AlphFiles

let private regexInput = Regex("\$in(\d+)");
let private regexOutput = Regex("\$out(\d+)");

/// Returns two sets of referenced indices, one for dependencies and one for outputs.
let parseReferences (command:string) =    
    let getIndices (matches:Match seq) =
        matches 
        |> Seq.map(fun m -> Int32.Parse(m.Groups.[1].Value))
        |> Set.ofSeq
    (regexInput.Matches(command) |> getIndices, regexOutput.Matches(command) |> getIndices)

/// Throws an exception if the given command is incorrect.
let validate (inputCount: int, outputCount: int) (command: string) =
    let (inputs, outputs) = parseReferences command
    let wrongInputs = inputs |> Seq.filter(fun ref -> ref < 1 || ref > inputCount) |> Seq.toArray
    let wrongOutputs = outputs |> Seq.filter(fun ref -> ref < 1 || ref > outputCount) |> Seq.toArray
    if wrongInputs.Length > 0 then raise (ArgumentException("Command contains incorrect input indices"))
    if wrongOutputs.Length > 0 then raise (ArgumentException("Command contains incorrect output indices"))
    