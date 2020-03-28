module ItisLab.Alpheus.Utils

open Angara.Data
open System.Threading.Tasks
open System.Collections.Concurrent
open System
open System.Threading

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

let enterResourceGroupMonitorAsync monitors (groupName:string) =
    let mutable simaphore = null
    lock(monitors) (fun () ->
        let ss = match Map.tryFind groupName (!monitors) with
                 |  Some(ss) -> ss
                 |  None ->
                    let ss = new SemaphoreSlim(1)
                    monitors := Map.add groupName ss (!monitors)
                    ss
        simaphore <- ss
    )
    simaphore.WaitAsync() |> Async.AwaitTask

let exitResourceGroupMonitor monitors (groupName:string) =
    let simaphore: SemaphoreSlim = Map.find groupName monitors
    simaphore.Release() |> ignore

/// returns the 'map' in which the 'key' is removed.
/// if there is no element identified with 'key', the same 'map' is returned
let mdmapRemoveIfDefined (key:'k list) (map:MdMap<'k,'v>) =
    map
    |> MdMap.toSeq
    |> Seq.filter (fun p -> let k,_ = p in k <> key)
    |> Seq.fold (fun s e -> let k,v = e in MdMap.add k v s) MdMap.empty

let mdmapMerge (mapper: 'k list -> 'v option -> 'v option -> 'w) mdmap1 mdmap2 =
    let firstKeys = mdmap1 |> MdMap.toSeq |> Seq.map fst |> Set.ofSeq
    let secondKeys = mdmap2 |> MdMap.toSeq |> Seq.map fst |> Set.ofSeq
    let unionKeys = Set.union firstKeys secondKeys
    unionKeys
    |> Seq.map (fun k -> k, (mapper k (MdMap.tryFind k mdmap1) (MdMap.tryFind k mdmap2)))
    |> Seq.fold (fun s e -> let k,v = e in MdMap.add k v s) MdMap.empty