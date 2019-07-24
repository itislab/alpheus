module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks
open System
open Xunit
open Xunit.Abstractions

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

/// represents the TextWriter that prints into supplied XUnit ITestOutputHelper
type OutputTextWriter(output:ITestOutputHelper) =
    inherit System.IO.TextWriter()

    override s.WriteLine(m)=
        output.WriteLine(m)

    override s.WriteLine(format,args) = 
        output.WriteLine(format,args)

    override s.Encoding
        with get() = System.Text.Encoding.UTF8

/// Class that can be a parent to all tests classes that require single-use one-time unique named directory creation before test
/// and deletion after the test
/// use .Path property to get the pass to the directory
[<Xunit.Collection("Disk involving test collection")>] // this prevents parallel tests execution
type SingleUseOneTimeDirectory(output:ITestOutputHelper) =
    let output = output

    let tempName = System.Guid.NewGuid().ToString()
    let dir1 = System.IO.Path.Combine("data","singleTimeDirs")
    let path = System.IO.Path.Combine(dir1,tempName)
    
    //let outputTextWriter = new OutputTextWriter(output)

    
    do
        // Console.SetOut(outputTextWriter) // redirect standard output (e.g. printfn) of the alpheus to the XUnit output capturer
        ItisLab.Alpheus.Logger.LogFunction <- fun cat message -> output.WriteLine(sprintf "%A: %s" cat message)

        if not(System.IO.Directory.Exists(dir1)) then
            System.IO.Directory.CreateDirectory(dir1) |> ignore  
        output.WriteLine(sprintf "Creating unique test dir %s" path)
        System.IO.Directory.CreateDirectory(path) |> ignore

    member s.Output
        with get() = output

    /// One-time single-use unique named directory to carry on tests within
    member s.Path
        with get() = path

    interface IDisposable with
        member s.Dispose() =
            //outputTextWriter.Dispose()
            System.IO.Directory.Delete(path,true)
            output.WriteLine(sprintf "Successfuly deleted unique test dir %s" path)

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

/// Asserts that the result is on Ok track
let assertResultOk result =
    match result with
    |   Ok(_) -> () // expected successful operation
    |   Error(e) -> Assert.True(false, sprintf "Expected successful save operation, but got error: %A" e)