﻿module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks
open System
open Xunit
open Xunit.Abstractions

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

/// whether the tests are currently executed on Windows
let isTestRuntimeWindows = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)

/// The OS specific path that is the root on the testing runtime
let testRuntimeRootPath =
    if isTestRuntimeWindows then @"C:\" else "/"

type TargetPlatform =
    | Windows = 1
    | Linux = 2

/// Class that can be a parent to all tests classes that require single-use one-time unique named directory creation befor test
/// and deletion after the test
/// use .Path property to get the pass to the directory
type SingleUseOneTimeDirectory(output:ITestOutputHelper) =
    let output = output

    let tempName = System.Guid.NewGuid().ToString()
    let dir1 = System.IO.Path.Combine("data","singleTimeDirs")
    let relativePath = System.IO.Path.Combine(dir1,tempName) + string System.IO.Path.DirectorySeparatorChar
    let fullPath = System.IO.Path.GetFullPath(relativePath)
    
    //let outputTextWriter = new OutputTextWriter(output)

    let logToXunitOutput cat message =
        try
            output.WriteLine(sprintf "%A: %s" cat message)
        with
        // this is workaround as XUnit sometimes throws InvalidArgument "There is no currently active test"
        // this is connected with async execution of the tests
        // related issues are closed but the issues persists here: https://github.com/xunit/xunit/issues/1540
        |   :? InvalidOperationException -> ()
    
    do
        // setting high verbosity level
        ItisLab.Alpheus.Logger.LogLevel <- ItisLab.Alpheus.Logger.VerboseLevel
        
        // redirect logging module of the alpheus to the XUnit output capturer
        ItisLab.Alpheus.Logger.LogFunction <- logToXunitOutput

        if not(System.IO.Directory.Exists(dir1)) then
            System.IO.Directory.CreateDirectory(dir1) |> ignore  
        output.WriteLine(sprintf "Creating unique test dir %s" relativePath)
        System.IO.Directory.CreateDirectory(relativePath) |> ignore

    member s.Output
        with get() = output

    /// One-time single-use unique named directory to carry on tests within relative to current working directory
    member s.RelativeExperimentRoot = relativePath

    /// Full path of the experiment root - One-time single-use unique named directory to carry on tests within 
    member s.ExperimentRoot = fullPath

    member s.Platform = if isTestRuntimeWindows then TargetPlatform.Windows else TargetPlatform.Linux

    interface IDisposable with
        member s.Dispose() =
            System.IO.Directory.Delete(fullPath,true)
            output.WriteLine(sprintf "Successfully deleted unique test dir %s" relativePath)


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
    |   Error(e) -> Assert.True(false, sprintf "Expected successful operation, but got error: %A" e)

type Microsoft.FSharp.Collections.List<'T> with 
    member this.ToArray() = this |> Array.ofList