module ItisLab.Alpheus.Logger

open System.Diagnostics

[<Literal>]
/// Corresponds to no output
let QuietLevel = 0
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
    /// To be used in the outputs of external commands
    | ExecutionOutput
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

let logVerboseLongRunningStart (category: LogCategory) (message:string) =
    let ct = new System.Threading.CancellationTokenSource()
    // if log level is verbose, equivalent to verbose
    if LogLevel >= VerboseLevel then
        logVerbose category message
    else
        if LogLevel >= InfoLevel then
            // schedules the timer to output the message as INFO level, if not canceled via cancellation token
            // useful to indicate start of potentially long operation
            // if it is indeed long running (is not canceled) the user is notified of it, thus aware what long operation is running
            let delayedPrint =
                async {
                    do! Async.SwitchToThreadPool()
                    do! Async.Sleep 500 // half a sec is considered long enough for now
                    lock(ct) (fun () ->
                        if ct.IsCancellationRequested then
                            ct.Dispose()
                        else
                            ct.Cancel()
                            logInfo category message
                    )
                }
            Async.Start delayedPrint
    ct

let logVerboseLongRunningFinish (ct:System.Threading.CancellationTokenSource) (category: LogCategory) (message:string) =
    if LogLevel >= VerboseLevel then
        logVerbose category message
        ct.Dispose()
    else
        if LogLevel >= InfoLevel then
            lock(ct) (fun () ->
                if ct.IsCancellationRequested then
                    // the start was printed, thus we need to print finish
                    logInfo category message
                    ct.Dispose()
                else
                    ct.Cancel()
                )
            

let doAndLogElapsedTime (log: string -> unit) (message: string) (func: unit -> Async<unit>) = 
    async {
        let sw = Stopwatch.StartNew()
        let! result = func()
        sw.Stop()
        log (sprintf "%s (%A)" message sw.Elapsed)
    }