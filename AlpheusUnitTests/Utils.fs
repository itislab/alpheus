module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks
open System

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

[<AbstractClass>]
/// Class that can be a parent to all tests classes that require single-use one-time unique named directory creation befor test
/// and deletion after the test
/// use .Path property to get the pass to the directory
type SingleUseOneTimeDirectory() =
    let tempName = System.Guid.NewGuid().ToString()
    let path = System.IO.Path.Combine("data",tempName)
    
    do
        System.IO.Directory.CreateDirectory(path) |> ignore

    /// One-time single-use unique named directory to carry on tests within
    member s.Path with
        get() = path

    interface IDisposable with
        member s.Dispose() =
            System.IO.Directory.Delete(path,true)
