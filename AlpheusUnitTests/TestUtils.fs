module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks
open System

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

/// whether the tests are currently executed on Windows
let isTestRuntimeWindows = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)

/// The OS specific path that is the root on the testing runtime
let testRuntimeRootPath =
    if isTestRuntimeWindows then @"C:\" else "/"

type TargetPlatform =
    | Windows = 0
    | Linux = 1

/// Class that can be a parent to all tests classes that require single-use one-time unique named directory creation befor test
/// and deletion after the test
/// use .Path property to get the pass to the directory
type SingleUseOneTimeDirectory() =
    let tempName = System.Guid.NewGuid().ToString()
    let dir1 = System.IO.Path.Combine("data","singleTimeDirs")
    let path = System.IO.Path.Combine(dir1,tempName) + String([|System.IO.Path.DirectorySeparatorChar|])
    
    do
        if not(System.IO.Directory.Exists(dir1)) then
            System.IO.Directory.CreateDirectory(dir1) |> ignore  
        System.IO.Directory.CreateDirectory(path) |> ignore

    /// One-time single-use unique named directory to carry on tests within
    member s.Path
        with get() = path

    member s.Platform = if isTestRuntimeWindows then TargetPlatform.Windows else TargetPlatform.Linux

    interface IDisposable with
        member s.Dispose() =
            System.IO.Directory.Delete(path,true)


