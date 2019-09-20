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
open System.Threading

let internal arrayType<'a> rank : Type =
    if rank < 0 then invalidArg "rank" "Rank is negative"
    else if rank = 0 then typeof<'a>
    else typeof<'a>.MakeArrayType(rank)

let internal inputRank (v:MethodVertex) =
    match v with
    | Source src -> 0
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.fold max 0

let internal outputRank (v:MethodVertex) =
    match v with
    | Source src -> src.Output.Artefact.Rank
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> a.Artefact.Rank) |> Seq.max

let internal methodRank (v:MethodVertex) = min (outputRank v) (inputRank v)

let getOutputTypes<'a> (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> [max 0 (src.Output.Artefact.Rank - rank) |> arrayType<'a>]
    | Command cmd -> cmd.Outputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType<'a>) |> List.ofSeq

let getInputTypes<'a> (v:MethodVertex) =
    let rank = methodRank v
    match v with
    | Source src -> List.empty
    | Command cmd -> cmd.Inputs |> Seq.map(fun a -> max 0 (a.Artefact.Rank - rank) |> arrayType<'a>) |> List.ofSeq

let toJaggedArrayOrValue (mapValue: (string list * 'a) -> 'c) (map: MdMap<string,'a>) : obj =
    let rec toJaggedArrayOrValueRec (mapValue: (string list * 'a) -> 'c) (index: string list) (map: MdMapTree<string,'a>) : obj =
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
                    | MdMapTree.Map _ -> toJaggedArrayOrValueRec mapValue newIndex t 
                    | MdMapTree.Value _ -> failwith "Data is incomplete and has missing elements"))
    toJaggedArrayOrValueRec mapValue [] (map |> MdMap.toTree)

/// Truncates `index` so its length is `rank`, if rank is less or equal to the length of the index.
/// Throws if the rank is greater than the length of the index.
let rec internal truncateIndex (rank: int) (index: string list)  = 
    if rank < 0 then failwith "Rank is negative"
    else if rank = 0 then []
    else // rank > 0
        match index with
        | [] -> failwith "Rank is greater than the length of the index"
        | head :: tail -> head :: (truncateIndex (rank-1) tail)

/// Returns a child MdMap that corresponds to the given index, where
/// - index can be incomplete (i.e. returns non-scalar MdMap, e.g. for scatter)
/// - index can be complete (i.e. returns scalar)
/// - index can be over-specified (i.e. it is truncated, e.g. for reduce)
/// - index not found (i.e. returns None)
let internal resolveIndex (index:string list) (map: MdMap<string, 'a>) =
    let rec resolveInTree (index:string list) (map: MdMapTree<string, 'a>) =
        match map, index with
        | _,[] -> Some map // index length may be less than rank of the map (scattering)
        | MdMapTree.Value value,_ -> Some map // index length > rank of the map (reducing)
        | MdMapTree.Map values, k :: tail ->
            match values |> Map.tryFind k with
            | Some value -> resolveInTree tail value
            | None -> None

    match resolveInTree index (map |> MdMap.toTree) with
    | Some(MdMapTree.Value v) -> Some (MdMap.scalar v)
    | Some(MdMapTree.Map map) when map.IsEmpty -> None
    | Some(MdMapTree.Map _) -> Some (MdMap.get index map)
    | None -> None

let getExpectedAndActualVersions index (link: LinkToArtefact) =
    async {
        match link.ExpectedVersion |> resolveIndex index with
        | None -> // no expectations for this instance
            return MdMap.empty |> MdMap.add index (None, None)
        | Some expected ->
            let! actualSeq = 
                expected
                |> MdMap.toSeq
                |> Seq.map(fun (j, e) ->                     
                    async {
                        let! a = 
                            match e with 
                            | Some _ -> link.Artefact.ActualVersion.Get j
                            | None -> async.Return None
                        return (j,e,a)
                    })
                |> Async.Parallel
            let expectedAndActual = actualSeq |> Seq.fold (fun actual (j,e,a) -> actual |> MdMap.add j (e,a)) MdMap.empty
            return expectedAndActual
    }

let getExpectedAndActualVersionsForLinks index links =
    async {
        let! expectedAndActual = 
            links
            |> List.map (getExpectedAndActualVersions index)
            |> Async.Parallel
        let expPerInput, actualPerInput = 
            expectedAndActual 
            |> Array.map(fun map -> map |> MdMap.toSeq |> Seq.map snd |> Seq.toArray |> Array.unzip) 
            |> Array.unzip
        let expected = expPerInput |> Array.collect id
        let actual = actualPerInput |> Array.collect id
        return expected, actual
    }

/// Returns a sequence of indices and hashes of missing local artefact instances to be restored.
let versionsToRestore index (link: LinkToArtefact) =
    async {
        let! expectedAndActual = link |> getExpectedAndActualVersions index
        let artefactPathPattern = link.Artefact.Id |> PathUtils.idToFullPath link.Artefact.ExperimentRoot
        let toRestore =
            expectedAndActual 
            |> MdMap.toSeq
            |> Seq.choose(fun (j, (exp,act)) -> 
                match act with
                | Some _ -> None
                | None -> exp |> Option.map(fun exp -> (artefactPathPattern |> PathUtils.applyIndex j, exp)))
            |> Seq.toList
        return toRestore 
    }

/// Returns status of the method instance which determines whether the command method needs actual CLI tool execution.
/// This is a common code that is used both during the artefact status calculation and artefact production.
let getCommandVertexStatus checkStoragePresence (command:CommandLineVertex) index =
    let methodItemId = command.MethodId |> applyIndex index
    let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%s: %s" methodItemId str)

    async {
        let! expectedInputs, actualInputs = command.Inputs |> getExpectedAndActualVersionsForLinks index
        let! inputsStatus = findExpectedArtefacts checkStoragePresence expectedInputs actualInputs
        match inputsStatus with
        // we can avoid checking outputs to speed up the work            
        |   SomeAreNotFound ->
            logVerbose "Needs recomputation as some of the inputs not found" 
            return Outdated InputsOutdated
        |   SomeAreLocalUnexpected ->
            logVerbose "Needs recomputation as disk version of the input does not match expected version"
            return Outdated InputsOutdated                 
        |   AllExist _ ->
            // checking outputs
            let! expectedOutputs, actualOutputs = command.Outputs |> getExpectedAndActualVersionsForLinks index
            let! outputsStatus = findExpectedArtefacts checkStoragePresence expectedOutputs actualOutputs
            match outputsStatus with
            |   SomeAreNotFound ->
                logVerbose "Needs recomputation as some of the outputs not found" 
                return Outdated OutputsOutdated
            |   SomeAreLocalUnexpected ->
                logVerbose "Needs recomputation as disk version of the output does not match expected version"
                return Outdated OutputsOutdated
            |   AllExist outputs ->
                return UpToDate outputs
    }


/// This type represents an Angara Flow method.
/// Note that execution modifies the given vertex and it is Angara Flow execution runtime who controls
/// the concurrency.
/// 'a is an artefact type, i.e. type of objects passed between methods.
[<AbstractClass>]
type AngaraGraphNode<'a>(producerVertex:MethodVertex) =
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        getInputTypes<'a> producerVertex,
        getOutputTypes<'a> producerVertex)

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.MethodId
        |   Command(comp) -> comp.MethodId

    override s.ToString() = 
        match producerVertex with
        | Source src -> sprintf "Source id=%A" src.Output.Artefact.Id
        | Command cmd -> sprintf "Command id=%s, command=%s" cmd.MethodId cmd.Command
