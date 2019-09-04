module ItisLab.Alpheus.AngaraGraphCommon

open System
open System.IO
open AlphFiles
open Angara.Data
open Angara.Graph
open Angara.Execution
open Angara.States
open ItisLab.Alpheus.DependencyGraph
open ItisLab.Alpheus.PathUtils

let internal arrayType<'a> rank : Type =
    if rank < 0 then invalidArg "rank" "Rank is negative"
    else if rank = 0 then typeof<ArtefactId>
    else typeof<ArtefactId>.MakeArrayType(rank)

let internal inputRank (v:MethodVertex) =
    match v with
    | Source src -> 0
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.max

let internal outputRank (v:MethodVertex) =
    match v with
    | Source src -> src.Output.Artefact.Rank
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.max

let internal methodRank (v:MethodVertex) = min (outputRank v) (inputRank v)

let getOutputTypes (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> [max 0 (src.Output.Artefact.Rank - rank) |> arrayType]
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType) |> List.ofSeq

let getInputTypes (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> List.empty
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType) |> List.ofSeq


let rec internal toJaggedArrayOrValue (mapValue: (string list * 'a) -> 'c) (index: string list) (map: MdMapTree<string,'a>) : obj =
    let isValue = function
    | MdMapTree.Value _ -> true
    | MdMapTree.Map _ -> false

    let mapToArray (getElement: (string * MdMapTree<string,'a>) -> 'b) (map: Map<string,MdMapTree<string,'a>>) : 'b[] =
        map |> Map.toSeq |> Seq.sortBy fst |> Seq.map getElement |> Seq.toArray

    let append v list = list |> List.append [v]

    match map with
    | MdMapTree.Value v -> upcast(mapValue (index, v))
    | MdMapTree.Map subMap ->
        match subMap |> Map.forall(fun _ -> isValue) with
        | true -> // final level
            upcast(subMap |> mapToArray (fun (k,t) -> 
                let newIndex = index |> append k
                match t with 
                | MdMapTree.Value v -> mapValue (newIndex, v)
                | MdMapTree.Map _ -> failwith "Unreachable case"))
        | false ->
            upcast(subMap |> mapToArray (fun (k,t) -> 
                let newIndex = index |> append k
                match t with 
                | MdMapTree.Map _ -> toJaggedArrayOrValue mapValue newIndex t 
                | MdMapTree.Value _ -> failwith "Data is incomplete and has missing elements"))


/// valid are items that either have actual disk data version match expected version, or actual disk data is missing and expected version is restorable from storage
let areValidItemsVersions checkStoragePresence expectedVersionHashes actualVersionsHashes =
    async {
        if Array.exists Option.isNone expectedVersionHashes then
            // some of the artefact element was not ever produced, this is invalid
            return false
        else
            /// Chooses the pairs that are not valid on disk (filtering out versions match)
            let invalidOnDiskChooser hash1 hash2 =
                match hash1,hash2 with
                |   Some(h1),Some(h2) ->  if h1 = h2 then None else Some(hash1,hash2)
                |   _ -> Some(hash1,hash2)
            let localInvalid = Seq.map2 invalidOnDiskChooser expectedVersionHashes actualVersionsHashes |> Seq.choose id |> Array.ofSeq
            if Array.length localInvalid = 0 then
                // valid as actual disk version match expected version. No need to check the storage
                return true
            else
                // check if the locally "invalid" are "remote valid" (restorable from storage in case of disk data absence)
                let eligibleForRemoteCheckChooser pair =
                    let expected,actual = pair
                    match expected,actual with
                    |   Some(v),None -> Some(v)
                    |   _,_ -> None

                let eligibleForRemoteCheck = Array.choose eligibleForRemoteCheckChooser localInvalid
                if Array.length eligibleForRemoteCheck = Array.length localInvalid then
                    // we proceed with the remote checks only if all of the locally invalid items are eligible for remote check
                    let! remotePresence = checkStoragePresence eligibleForRemoteCheck
                    return Array.forall id remotePresence
                else
                    // otherwise at least one unrestorable item exists. Thus invalid
                    return false
    }

/// This type represents an Angara Flow method.
/// Note that execution modifies the given vertex and it is Angara Flow execution runtime who controls
/// the concurrency.
[<AbstractClass>]
type AngaraGraphNode(producerVertex:MethodVertex) =
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        getInputTypes producerVertex,
        getOutputTypes producerVertex)

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.Output.Artefact.Id
        |   Command(comp) -> (Seq.head comp.Outputs).Artefact.Id // first output is used as vertex ID

    override s.ToString() = 
        match producerVertex with
        | Source src -> sprintf "Source %A" src.Output.Artefact.Id
        | Command cmd -> sprintf "Command %s" cmd.Command

