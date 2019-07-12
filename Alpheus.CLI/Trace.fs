module ItisLab.Alpheus.Trace

let traceVerbose str =
    //ts.TraceEvent(TraceEventType.Verbose,0,str)
    printfn "%s" str

let traceInfo str =
    printfn "%s" str

