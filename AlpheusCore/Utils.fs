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

let resolveIndex (index:string list) (map: MdMap<string, 'a>) =
    let rec resolveInTree (index:string list) (map: MdMapTree<string, 'a>) =
        match map, index with
        | _,[] -> Some map
        | MdMapTree.Value value,_ -> Some map // index length > rank of the map
        | MdMapTree.Map values, k :: tail ->
            match values |> Map.tryFind k with
            | Some value -> resolveInTree tail value
            | None -> None
    match resolveInTree index (map |> MdMap.toTree) with
    | Some(MdMapTree.Value v) -> Some v
    | Some(MdMapTree.Map map) when map.IsEmpty -> None
    | Some(MdMapTree.Map _) -> invalidOp "Only one-to-one vectors are supported at the moment"
    | None -> None