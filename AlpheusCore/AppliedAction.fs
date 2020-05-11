namespace ItisLab.Alpheus

open Angara.Data
open System.Threading.Tasks

open AlphFiles
open PathUtils
open FSharp.Control

/// what happened to the artefact(or artefact item in case of vector) during the computation
type ArtefactAppliedActionType = 
    /// The system took no actions regarding this item
    |   Nothing
    /// The system updated the artefact disk content trough computation
    |   Recomputed
    /// The system restored missing artefact disk content from one of the avialble storages
    |   Restored
    /// The system deleted the artefact item disk content due to vector index disapper
    |   Deleted


/// Provides threadsafe MdMap of ArtefactAppliedActionType
type ArtefactAppliedAction(id: ArtefactId) = 
    let mutable action = MdMap.empty<string, ArtefactAppliedActionType>
    let actionLock = obj()
    let rank = Artefacts.rank id

    let checkRank (index: string list) =
        if index.Length <> rank then invalidArg "index" (sprintf "Rank of artefact is %d while index length is %d" rank index.Length)

    member s.Id = id

    member s.Get (index: string list) : ArtefactAppliedActionType =
        checkRank index
        
        match action |> MdMap.tryFind index with
        | Some v -> v
        | None -> Nothing
        
    member s.Set (index: string list) v =
        checkRank index
        lock (actionLock) (fun () ->
            action <- MdMap.add index v action
        )
    
            


