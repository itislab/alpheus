module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks
open System

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

/// Class that can be a parent to all tests classes that require single-use one-time unique named directory creation befor test
/// and deletion after the test
/// use .Path property to get the pass to the directory
[<Xunit.Collection("Disk involving test collection")>]
type SingleUseOneTimeDirectory() =
    let tempName = System.Guid.NewGuid().ToString()
    let dir1 = System.IO.Path.Combine("data","singleTimeDirs")
    let path = System.IO.Path.Combine(dir1,tempName)
    
    do
        if not(System.IO.Directory.Exists(dir1)) then
            System.IO.Directory.CreateDirectory(dir1) |> ignore  
        System.IO.Directory.CreateDirectory(path) |> ignore

    /// One-time single-use unique named directory to carry on tests within
    member s.Path
        with get() = path

    interface IDisposable with
        member s.Dispose() =
            System.IO.Directory.Delete(path,true)

/// whether the tests are currently executed on Windows
let isTestRuntimeWindows = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)

/// The OS specific path that is the root on the testing runtime
let testRuntimeRootPath =
    if isTestRuntimeWindows then @"C:\" else "/"

/// runs the command in Unix Shell
/// Useful for setting file permissions, etc.
let execUnixShellcommand (command:string) =
    let escapedArgs = command.Replace("\"", "\\\"");

    let startInfo =  System.Diagnostics.ProcessStartInfo()
    
    // startInfo.RedirectStandardOutput <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    startInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
    startInfo.FileName <- "/bin/sh"
    startInfo.Arguments <- sprintf "-c \"%s\"" escapedArgs
    
    let p = System.Diagnostics.Process.Start(startInfo)
    p.WaitForExit();
    p.ExitCode