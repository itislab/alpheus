namespace ItisLab.Alpheus

open Angara.Data
open System.Threading.Tasks

open AlphFiles
open PathUtils
open FSharp.Control


/// Provides an artefact version synchronized with the local disk.
type ActualArtefactVersion(id: ArtefactId, experimentRoot: string) = 
    let mutable version = MdMap.empty<string, Lazy<Task<HashString option>>>
    let versionLock = obj()
    let rank = Artefacts.rank id

    let checkRank (index: string list) =
        if index.Length <> rank then invalidArg "index" (sprintf "Rank of artefact is %d while index length is %d" rank index.Length)

    let readItemVersionAsync (index: string list) =
        async {
            let fullPath = id |> idToFullPath experimentRoot |> applyIndex index
            Logger.logVerbose Logger.LogCategory.DependencyGraph (sprintf "Calculating hash of %s..." fullPath)
            return! Hash.hashPathAndSave fullPath
        }

    let lazyReadVersion (index: string list) =
        Lazy<Task<HashString option>>(fun () -> 
            async {
                let! hash = readItemVersionAsync index
                return hash
            } |> Async.StartAsTask)

    member s.Id = id

    /// Invalidates the given artefact instance version.
    member s.Invalidate(index: string list) =
        checkRank index
        lock versionLock (fun() -> version <- version |> MdMap.add index (lazyReadVersion index))

    /// Gets an actual version for the given exact index.
    /// None means that there is nothing on disk
    member s.Get (index: string list) : Async<HashString option> =
        checkRank index
        let factory = 
            match version |> MdMap.tryFind index with
            | Some v -> v
            | None -> 
                lock versionLock (fun () ->
                    match version |> MdMap.tryFind index with
                    | Some v -> v
                    | None ->
                        s.Invalidate index
                        version |> MdMap.find index
                )
        factory.Value |> Async.AwaitTask            

    override s.ToString() = 
        let myVersion = version
        myVersion 
        |> MdMap.toSeq
        |> Seq.map(fun(index, v) ->
            let stringVersion =
                if v.IsValueCreated then    
                    match v.Value.Status with
                    | System.Threading.Tasks.TaskStatus.RanToCompletion -> sprintf "%A" (v.Value.Result |> Option.map(fun s -> s.Substring(0,6)))
                    | System.Threading.Tasks.TaskStatus.Faulted -> sprintf "error: %A" v.Value.Exception
                    | _ -> "disk version not checked"
                else "disk version not checked"
            if index.Length = 0 then stringVersion else sprintf "%A:%s" index stringVersion)
        |> String.concat ", "
            


