module ItisLab.Alpheus.Utils

open Angara.Data
open System.Threading.Tasks
open System.Collections.Concurrent
open System

let mapAsync (func: ('a list * 'b) -> Async<'c>) (data: MdMap<'a, 'b>) : Async<MdMap<'a, 'c>>  =
    async {
        let mapOfTasks = data |> MdMap.mapi (fun key value -> func(key, value) |> Async.StartAsTask)
        let! _ = mapOfTasks |> MdMap.toSeq |> Seq.map snd |> Task.WhenAll |> Async.AwaitTask
        return mapOfTasks |> MdMap.map (fun task -> task.Result)
    }

let singleExecutionGuardAsync tasksCache taskArgs taskFactory =
    // either start the task with the current taskArgs
    // or return the task that was started earlier (by previous call with the same tasksCache and taskArgs)
    let mutable task = null
    lock(tasksCache) (fun () -> 
        let t = match Map.tryFind taskArgs !tasksCache with
                |   Some(t) -> t
                |   None -> 
                    let t = taskFactory taskArgs |> Async.StartAsTask
                    tasksCache := Map.add taskArgs t (!tasksCache)
                    t
        task <- t
    )
    Async.AwaitTask task



