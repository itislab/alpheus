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
    
    // registering some CLI event handlers
    // Ctrl+C and Ctrl+Brake (stated in docs)
    // unix SIGINT and SIGTERM ?
    Console.CancelKeyPress.AddHandler(ConsoleCancelEventHandler(fun _ args ->
        Logger.logInfo Logger.LogCategory.CLI (sprintf "Got %A. Closing active subprocesses if any" args.SpecialKey)
        ExecuteCommand.terminateAllSubprocesses()))

    try 
        let parseResults = parser.ParseCommandLine(argv, ignoreMissing=false, ignoreUnrecognized=true, raiseOnUsage=true)

        let effectiveVerbosityLevel =
            match parseResults.TryGetResult Verbosity with
            |   Some(setLevel) ->
                match setLevel with
                |   VerbosityLevel.Quite -> Logger.QuiteLevel
                |   VerbosityLevel.Err -> Logger.ErrorLevel
                |   VerbosityLevel.Warn -> Logger.WarningLevel
                |   VerbosityLevel.Info -> Logger.InfoLevel
                |   VerbosityLevel.Verbose -> Logger.VerboseLevel
                |   unknown ->
                    Logger.logWarning Logger.CLI (sprintf "Unknown verbosity level %A. Default vebosity level 'Info' will be used" unknown)
                    Logger.InfoLevel
            |   None -> Logger.InfoLevel // default verbosity for CLI
        Logger.LogLevel <- effectiveVerbosityLevel

        let cwd = Path.GetFullPath Environment.CurrentDirectory 

        match parseResults |> run programName cwd with
        | Ok() -> 0
        | Error(er) ->
            match er with
            | UserError m -> printfn "Error occurred: %s" m; 1
            | SystemError m -> printfn "Internal error occurred: %s" m; 1
    with
    |   :? ArguException as e ->
        printfn "%s" e.Message // argu exception is parse exception. So don't print stack trace. We print only parse error content.
        2
    |   e ->
        printfn "%s" (e.ToString()) // In all other exception types we print the exception with stack trace.
        3
