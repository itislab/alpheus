module ItisLab.Alpheus.Logger

let internal showVerbose = true

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
    /// To be used in the Alpheus experiment folder management and manipulation
    | ExperimentFolder
    /// Dependency graph manipulation and management
    | DependencyGraph

/// A function to be used for logs output. Can be set externally to override default behavior of printing to console
let mutable LogFunction = fun (category: LogCategory) (message:string) ->
    printfn "%A: %s" category message

let logVerbose (category: LogCategory) (message:string) =
    if showVerbose then LogFunction category message
    else ()

let logInfo (category: LogCategory) (message:string) =
    LogFunction category message
    
let logWarning (category: LogCategory) (message:string) =
    LogFunction category message
    
let logError (category: LogCategory) (message:string) =
    LogFunction category message

let logException (category: LogCategory) (e:exn) =
    LogFunction category (e.ToString())