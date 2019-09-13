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



let extractActualVersionsFromLinks index links =
    links
    |> Seq.map (fun (a:LinkToArtefact) -> a.Artefact.ActualVersion.Resolve index)
    |> Async.Parallel

let extractExpectedVersionsFromLinks index links =
    links 
    |> Seq.map (fun (a:LinkToArtefact) -> Utils.resolveIndex index a.ExpectedVersion |> Option.flatten)

/// whether the command method needs actual CLI tool execution
/// common code that is used both during the artefact status calculation and artefact production
let getCommandVertexStatus checkStoragePresence (command:CommandLineVertex) index =
    let methodItemId = command.MethodId |> applyIndex index
    let logVerbose str = Logger.logVerbose Logger.Execution (sprintf "%s: %s" methodItemId str)
    async {
        let expectedInputItemVersions = extractExpectedVersionsFromLinks index command.Inputs |> Array.ofSeq
        let! actualInputItemVersions = extractActualVersionsFromLinks index command.Inputs

        let! inputsStatus = findExpectedArtefacts checkStoragePresence expectedInputItemVersions actualInputItemVersions
        
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
            let expectedOutputItemVersions = extractExpectedVersionsFromLinks index command.Outputs |> Array.ofSeq
            let! actualOutputItemVersions = extractActualVersionsFromLinks index command.Outputs
            let! outputsStatus = findExpectedArtefacts checkStoragePresence expectedOutputItemVersions actualOutputItemVersions
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
[<AbstractClass>]
type AngaraGraphNode(producerVertex:MethodVertex) =
    inherit ExecutableMethod(
        System.Guid.NewGuid(),
        getInputTypes producerVertex,
        getOutputTypes producerVertex)

    member s.VertexID =
        match producerVertex with
        |   Source(s) -> s.MethodId
        |   Command(comp) -> comp.MethodId

    override s.ToString() = 
        match producerVertex with
        | Source src -> sprintf "Source %A" src.Output.Artefact.Id
        | Command cmd -> sprintf "Command %s" cmd.Command
