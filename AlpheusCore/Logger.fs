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
    /// To be used in the Alpheus experement folder management
    | ExperimentFolder

/// A function to be used for logs output
let mutable LogFunction = fun (category: LogCategory) (message:string) ->
    printfn "%A: %s" category message

let logVerbose (category: LogCategory) (message:string) =
    if showVerbose then LogFunction category message
    else ()

let logInfo (category: LogCategory) (message:string) =
    printfn "%A: %s" category message
    
let logWarning (category: LogCategory) (message:string) =
    printfn "%A: %s" category message
    
let logError (category: LogCategory) (message:string) =
    printfn "%A: %s" category message

let logException (category: LogCategory) (e:exn) =
    printfn "%A: %A" category e