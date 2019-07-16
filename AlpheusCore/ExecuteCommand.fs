module ItisLab.Alpheus.ExecuteCommand

open System
open System.IO
open ItisLab.Alpheus.Logger
open ItisLab.Alpheus.AlphFiles
open DependencyGraph

let private formatLine prefix message = 
    sprintf "[%40s]:\t%s" prefix message

/// Prepends the given line with the context information.
let internal annotateLine (methodId:string) (channel:string) (line:string) =
    let name = sprintf "%s [%s]" methodId channel
    formatLine name line

/// Runs the command line method and waits indefinitely until the process exits.
/// Returns the exit code.
let runAndWait (context: ComputationContext) (input: int -> string, output: int -> string) (computation: CommandLineVertex) =
    let program,args = MethodCommand.split computation.Command                   
    let printStream (annotate:string->string) (stream:StreamReader) = 
        async {
            do! Async.SwitchToNewThread()
            while not stream.EndOfStream do
                let! line = Async.AwaitTask(stream.ReadLineAsync())
                line |> annotate |> context.Print
        }

    use p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- program
    p.StartInfo.Arguments <- args
    p.StartInfo.WorkingDirectory <- context.GetAbsolutePath(computation.WorkingDirectory)
    p.StartInfo.RedirectStandardError <- true
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.CreateNoWindow <- true
    logVerbose LogCategory.Execution (formatLine computation.MethodId (sprintf "Running \"%s %s\" in \"%s\"" program args p.StartInfo.WorkingDirectory))
    
    p.Start() |> ignore
    p.StandardOutput |> printStream (annotateLine computation.MethodId "stdout") |> Async.Start
    p.StandardError |> printStream (annotateLine computation.MethodId "stderr") |> Async.Start

    p.WaitForExit()    
    p.ExitCode
