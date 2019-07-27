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

/// Executes program as a separate process, capturing stderr and stdout, processing them with print function callback.
/// Also supplies the output with outputAnnotationId label
/// workingDir is both the path for looking for the program and workingDir for the new process to be executed
let runCmdLocallyAsync print outputAnnotationId program args (workingDir:string) =
    if not(Path.IsPathRooted(workingDir)) then
        invalidArg "workingDir" "working directory must be specified as full path"
    async {
        let printStream (annotate:string->string) (stream:StreamReader) = 
            async {
                do! Async.SwitchToNewThread()
                while not stream.EndOfStream do
                    let! line = Async.AwaitTask(stream.ReadLineAsync())
                    line |> annotate |> print
            }
        
        use p = new System.Diagnostics.Process()
        // There is tricky situation with command (see github issue #1)
        // sometimes the user specifies "command" as an path to executable relative to the "working directory"
        // that is the prioritized behavior
        // if the executable is not found we try to run the "command" searching it in PATH
        let program = 
            let absExecutableCandidate = Path.Combine(workingDir,program)
            if File.Exists(absExecutableCandidate) then
                absExecutableCandidate
            else
                program
        p.StartInfo.FileName <- program
        p.StartInfo.Arguments <- args
        p.StartInfo.WorkingDirectory <- workingDir
        p.StartInfo.RedirectStandardError <- true
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true
        logVerbose LogCategory.Execution (formatLine outputAnnotationId (sprintf "Running \"%s %s\" in \"%s\"" program args p.StartInfo.WorkingDirectory))
        
        p.Start() |> ignore
        let outputTask = p.StandardOutput |> printStream (annotateLine outputAnnotationId "stdout") |> Async.StartAsTask
        let errorTask = p.StandardError |> printStream (annotateLine outputAnnotationId "stderr") |> Async.StartAsTask

        p.WaitForExit()
        outputTask.Wait()
        errorTask.Wait()
        return p.ExitCode
    }

/// Runs the command line method and waits indefinitely until the process exits.
/// Returns the exit code.
let runCommandLineMethodAndWait (context: ComputationContext) (input: int -> string, output: int -> string) (computation: CommandLineVertex) =
    let command = computation.Command.Trim() |> MethodCommand.substitute (input, output)
    let program,args = MethodCommand.split command                 
    let wdAbsPath = context.GetAbsolutePath(computation.WorkingDirectory)
    let localComputation = runCmdLocallyAsync context.Print computation.MethodId program args wdAbsPath 
    Async.RunSynchronously localComputation

    
