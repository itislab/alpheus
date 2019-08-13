module ItisLab.Alpheus.MethodCommand

open System
open System.Text.RegularExpressions
open ItisLab.Alpheus.Logger
open AlphFiles

let private regexInput = Regex("\$in(\d+)");
let private regexOutput = Regex("\$out(\d+)");
let private regexAny = Regex("\$(in|out)(\d+)");

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

/// Replaces placeholders (e.g. $in1 or $out2) with the actual values.
let substitute (input: int -> string, output: int -> string) (command: string) =
    let replacement (m:Match) =
        let i = Int32.Parse m.Groups.[2].Value
        let get = if m.Groups.[1].Value = "in" then input else output
        get i
    let evaluator = MatchEvaluator(replacement)
    regexAny.Replace(command, evaluator);

/// Splits the given command string to a pair of the program name and args.
let split (command:string) =
    let command = command.Trim()
    let q = "\""
    if command.Length > 2 && command.StartsWith(q) then
        let pos = command.IndexOf(q, 1)
        if pos < 0 then logWarning LogCategory.Execution (sprintf "Command line has missing closing quotes: %s" command)
        if pos > 0 && pos < command.Length - 1 then
            (command.Substring(0, pos+1).Trim(), command.Substring(pos+1).Trim())
        else
            (command, "")
    else
       let pos = command.IndexOf(" ")
       if pos > 0 && pos < command.Length - 1 then
            (command.Substring(0, pos+1).Trim(), command.Substring(pos+1).Trim())
       else
            (command, "")

/// Substitutes index of string values into a pattern command, e.g. "/files/*/*".
/// Note that index length is allowed to be less or equal to the rank of the command.
let rec applyIndex (index: string list) (command: string) =
    match index with
    | [] -> command
    | head :: tail ->
        let idx = command.IndexOf "*"
        if idx < 0 then 
            command
        else 
            let newCommand = command.Substring(0, idx) + head + command.Substring(idx+1)
            applyIndex tail newCommand