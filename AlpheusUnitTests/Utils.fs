module ItisLab.Alpheus.Tests.Utils

open System.Threading.Tasks

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

