module ItisLab.Alpheus.DebuggingHelpers

open System.IO
open System
open System.Net.Http
    

/// Wraps some target stream and prints out various debugging info about operations
type DebuggingStream(name: string, target:Stream) =
    inherit Stream()

    let guid = Guid.NewGuid()

    let mutable readRequestCounter = 0

    let mutable traceEnabled = true

    let trace str =
        if traceEnabled then
            printfn "Stream debugger (%A:%s): %s" guid name str

    member s.IsTraceEnabled 
        with get() =
            traceEnabled
        and set v =
            traceEnabled <- v

    override s.CanRead
        with get() =
            let cr = target.CanRead
            sprintf "getting CanRead -> %A" cr |> trace
            cr

    override s.CanSeek
        with get() =
            let cr = target.CanSeek
            sprintf "getting CanSeek -> %A" cr |> trace
            cr

    override s.CanWrite
        with get() =
            let cr = target.CanWrite
            sprintf "getting CanWrite -> %A" cr |> trace
            cr

    override s.Position
        with get() =
            let cr = target.Position
            sprintf "getting Position -> %A" cr |> trace
            cr
        and set v =
            sprintf "setting Position <- %A" v |> trace
    
    override s.Length
        with get() =
            let cr = target.Length
            sprintf "getting Length -> %A" cr |> trace
            cr

    override s.SetLength v =
        sprintf "setting Length <- %A" v |> trace
        target.SetLength v

    override s.Flush() =
        sprintf "flush" |> trace

    override s.Read(buffer: byte[], offset:int, length:int) =
        sprintf "read request %d: length %A;" readRequestCounter length |> trace
        let read = target.Read(buffer,offset,length)
        sprintf "read request %d: actually read %A (of %A requested)" readRequestCounter read length |> trace
        readRequestCounter <- readRequestCounter + 1
        read
    
    override s.Seek(offset: int64, origin) =
        let res = target.Seek(offset, origin)
        sprintf "Seek offset %A origin %A; result %A" offset origin res |> trace
        res

    override s.Write(buffer:byte[], offset:int, len:int) =
        sprintf "write request offset %A origin %A" offset len |> trace
        target.Write(buffer,offset,len)

    interface IDisposable with
        member s.Dispose() =
            base.Dispose()
            "Dispose" |> trace
            target.Dispose()