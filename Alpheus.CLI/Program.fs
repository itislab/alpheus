module ItisLab.Alpheus.Program

open System
open Argu
open ItisLab.Alpheus.CLI
open ItisLab.Alpheus.CliRunner
open ItisLab.Alpheus.Trace
open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let programName = "alpheus"
    let parser = ArgumentParser.Create<AlpheusArgs>(programName = programName)
    
    try 
        let parseResults = parser.ParseCommandLine(argv,false,true,true)
        match parseResults |> run programName with
        | Ok() -> 0
        | Error(m) -> printfn "Error occurred: %s" m; 1
    with e ->
        printfn "%s" (e.ToString())
        2

