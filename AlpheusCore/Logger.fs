module ItisLab.Alpheus.Logger

let internal showVerbose = true

type LogCategory =
    | Test 
    | Debug
    | CLI 
    | Execution
    | Storage

let logVerbose (category: LogCategory) (message:string) =
    if showVerbose then printfn "%A: %s" category message
    else ()

let logInfo (category: LogCategory) (message:string) =
    printfn "%A: %s" category message
    
let logWarning (category: LogCategory) (message:string) =
    printfn "%A: %s" category message
    
let logError (category: LogCategory) (message:string) =
    printfn "%A: %s" category message

let logException (category: LogCategory) (e:exn) =
    printfn "%A: %A" category e