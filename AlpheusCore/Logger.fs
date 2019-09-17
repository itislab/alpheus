module ItisLab.Alpheus.Logger

open System.Diagnostics

[<Literal>]
/// Corresponds to no output
let QuiteLevel = 0
[<Literal>]
/// Messages about the fact that prevent the successful execution
let ErrorLevel = 1
[<Literal>]
/// Messages about the facts that can be clue to the potential errors or unexpected behavior
let WarningLevel = 2
[<Literal>]
/// Messages that informs the user about ongoing events of normal successful computation
let InfoLevel = 3
[<Literal>]
/// Messages about the technical details of execution
let VerboseLevel = 4

/// Currently active log level that is applied to all categories
let mutable LogLevel = InfoLevel

type LogCategory =
    /// To be used in tests for logging.
    | Test 
    /// To be used in the Alpheus command line tool.
    | CLI 
    /// To be used in the Alpheus graph execution system.
    | Execution
    /// To be used in the Alpheus storage system.
    | Storage
    /// Higher level API
    | API
    /// Hash management
    | Hash
    /// Dependency graph manipulation and management
    | DependencyGraph
    /// Managing (updating) the .gitignore file
    | GitIgnoreManager

/// A function to be used for logs output. Can be set externally to override default behavior of printing to console
let mutable LogFunction = fun (category: LogCategory) (message:string) ->
    printfn "%A: %s" category message

let logVerbose (category: LogCategory) (message:string) =
    if LogLevel >= VerboseLevel then
        LogFunction category message

let logInfo (category: LogCategory) (message:string) =
    if LogLevel >= InfoLevel then
        LogFunction category message
    
let logWarning (category: LogCategory) (message:string) =
    if LogLevel >= WarningLevel then
        LogFunction category message
    
let logError (category: LogCategory) (message:string) =
    if LogLevel >= ErrorLevel then
        LogFunction category message

let logException (category: LogCategory) (e:exn) =
    if LogLevel >= ErrorLevel then
        LogFunction category (e.ToString())


let doAndLogElapsedTime (log: string -> unit) (message: string) (func: unit -> Async<unit>) = 
    async {
        let sw = Stopwatch.StartNew()
        let! result = func()
        sw.Stop()
        log (sprintf "%s (%A)" message sw.Elapsed)
    }