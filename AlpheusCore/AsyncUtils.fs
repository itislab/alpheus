﻿module ItisLab.Alpheus.AsyncUtils

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

let private computeOnceTable = ConcurrentDictionary<Type, ConcurrentDictionary<string, Task<obj>>>()

let computeOnce<'r> (compute: string -> Async<'r>) (arg: string) : Async<'r> =
    let perTypeTable = computeOnceTable.GetOrAdd(typeof<'r>, fun _ -> ConcurrentDictionary<string, Task<obj>>())
    let task = 
        perTypeTable.GetOrAdd(arg, fun key ->
            Task.Run<obj>(fun () ->
                async {
                    let! result = compute key
                    return result :> obj
                } |> Async.StartAsTask))
    async {
        let! objResult = task |> Async.AwaitTask
        return objResult :?> 'r
    }