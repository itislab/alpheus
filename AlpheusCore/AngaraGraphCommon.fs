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

/// Returns a submap (by index applied) of link expected expectations supplied with the actual version
let getActualForExpected index (link: LinkToArtefact) =
    async {
        match link.ExpectedVersion |> resolveIndex index with // this guards against overspecified index
        | None -> // no expectations for this instance
            return MdMap.empty |> MdMap.add index NoVersionExpected
        | Some expected ->
            let! index_expected_actual_seq = 
                expected
                |> MdMap.toSeq
                |> Seq.map(fun (j, e) ->                     
                    async {
                        let! a = 
                            match e with 
                            | Some e2 -> // expecting particular version? time to check what version is on disk
                                async {
                                    let! a2 = link.Artefact.ActualVersion.Get j
                                    return ExpectedAndActual(e2,a2)
                                }
                            | None -> async.Return NoVersionExpected
                        return (j,a)
                    })
                |> Async.Parallel
            let result = index_expected_actual_seq |> Seq.fold (fun actual (j,a) -> actual |> MdMap.add j a) MdMap.empty
            return result
    }

let getActualForExpectedVersionsForLinks index links =
    async {
        let! actualForExpected = 
            links
            |> List.map (getActualForExpected index)
            |> Async.Parallel
        let expectations = 
            actualForExpected 
            |> Seq.collect(fun map -> map |> MdMap.toSeq |> Seq.map snd) 
        return expectations
    }

/// Returns a sequence of indices and hashes of missing local artefact instances to be restored.
let getPathsToRestore index (link: LinkToArtefact) =
    // if we call this it means that the inputs are valid (local or remote)
    let isReduce = link.Artefact.Rank > List.length index // the index is not enough to fill all "*" in the artefact path
    async {
        let! actualForExpected = link |> getActualForExpected index
        let artefactPathPattern = link.Artefact.Id |> PathUtils.idToFullPath link.Artefact.ExperimentRoot |> PathUtils.applyIndex index
        let! restoreCandidate =
            actualForExpected 
            |> MdMap.toSeq
            |> Seq.map(fun (j, linkExpectation) -> // j are free indices not fixed by "index" in case of gather
                let pathToMissing = artefactPathPattern |> PathUtils.applyIndex j
                match linkExpectation with
                | ExpectedAndActual(expected,None) ->
                    // we want to consider for restore only those which absent now on disk AND we expect some version of them!
                    async.Return([|Some(pathToMissing, expected)|])
                | ExpectedAndActual(_,Some(act)) ->
                    // input is on disk. We don't care whether the versions match
                    // in any case we do not need to restore
                    async.Return [| None |]
                | NoVersionExpected ->
                    // this means that disk presence was not checked
                    // Now we need to check that as if something is on disk we do not need to restore                    
                    async {
                        let indiciesToCheck =
                            if isReduce then
                                // path to missing input still contains "*" and we may need to restore it
                                // well, we have to gather all of the disk content that matches the pattern
                                let reducedIndicies = PathUtils.enumeratePath pathToMissing |> MdMap.toSeq |> Seq.map fst
                                reducedIndicies |> Seq.map (fun reducedIdx -> List.append index reducedIdx)
                            else
                                seq { yield List.append index j |> List.truncate link.Artefact.Rank }
                        let checkIdx itemIdx =
                            async {
                                match! link.Artefact.ActualVersion.Get itemIdx with
                                |   Some(_) ->
                                    // there is something on disk. No need to restore
                                    return None
                                |   None ->
                                    // input version not expected, no data on disk.
                                    // tricky part here!!!
                                    // 
                                    // we may still want to extract expected version from the producer of the artefact
                                    // this is needed in case the user saved the artefact, deleted it, and now uses this artefact as input in newly created command (which does not expect particular input version yet)
                                    // it's tricky as producer may have different rank (in case of scatter/reduce)
                                    let artefact = link.Artefact
                                    let producer = artefact.ProducedBy                       
                                    let! expectedItem =
                                        async {
                                            match producer with
                                            |   Source s ->
                                                let expectedBySource = resolveIndex itemIdx s.Output.ExpectedVersion
                                                match expectedBySource with
                                                |   Some expectedVerBySource ->
                                                    if expectedVerBySource.IsScalar then
                                                        return expectedVerBySource.AsScalar()
                                                    else
                                                        failwith "not supported: we used most extended index (actualVersionLookupIdx) but it was not enough"
                                                        return None
                                                |   None ->
                                                    failwith "can't happen. Sourcing the artefact without alph file means that it is on disk. But we checked that it is not"
                                                    return None
                                            |   Command c ->
                                                let output = c.Outputs |> Seq.find (fun x -> x.Artefact.Id = artefact.Id)
                                                let expectedByCommand = resolveIndex itemIdx output.ExpectedVersion
                                                match expectedByCommand with
                                                |   Some expectedVerByCommand ->
                                                    if expectedVerByCommand.IsScalar then
                                                        return expectedVerByCommand.AsScalar()
                                                    else
                                                        failwith "not supported: we used most extended index (actualVersionLookupIdx) but it was not enough"
                                                        return None
                                                |   None ->
                                                    // the producer command does not expect any output version. It was not successful complete.
                                                    return None
                                            }
                                    return expectedItem |> Option.map (fun sh -> pathToMissing,sh)
                                }
                        return! indiciesToCheck |> Seq.map checkIdx |> Async.Parallel
                        }
                    )
            |> Async.Parallel
        let restore = restoreCandidate |> Seq.concat |> Seq.choose id |> Seq.toList        
        return restore
    }

/// Returns status of the method instance which determines whether the command method needs actual CLI tool execution.
/// This is a common code that is used both during the artefact status calculation and artefact production.
let getCommandVertexStatus checkStoragePresence (command:CommandLineVertex) index =
    let methodItemId = command.MethodId |> applyIndex index
    let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%s: %s" methodItemId str)

    async {
        let! inputExpectations = command.Inputs |> getActualForExpectedVersionsForLinks index
        let! inputsStatus = findExpectedArtefacts checkStoragePresence inputExpectations
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
            let! outputExpectations = command.Outputs |> getActualForExpectedVersionsForLinks index
            let! outputsStatus = findExpectedArtefacts checkStoragePresence outputExpectations
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
