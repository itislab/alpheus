/// The module computes the statuses of the artefacts
module ItisLab.Alpheus.StatusGraph

open Angara.Graph
open Angara.Execution
open Angara.States
open AlphFiles
open System
open ItisLab.Alpheus

/// The file/directory on the local machine that correspond to the artefact
type DataState =
    /// Exists and stored in one of the storage
    | Saved
    /// Exists but is not stored in any storage
    | Unsaved
    /// Does not exist on disk
    | Absent
    /// In process of being created on disk
    | InProgress

type OutdatedReason =
    |

/// Computational part of the state
type ComputationState =
    /// Outdated and can't be computed as inputs are absent or not up to date
    | InputsNotReady
    /// Outdated and everything is ready for running the computation
    | ReadyForComputation
    /// Can be restored from one of the storages
    | Restorable
    /// In process of computing the file/directory
    | Computing
    /// In process of restoring the file/directory from the storage
    | Restoring
    /// Computed with non zero exit code and matches the expected version
    | Failed
    /// Computed with zero exit code and matches the expected version
    | UpToDate

type ArtefactStatus = {
    FullID: ArtefactFullID
    DataState: DataState
    ComputationState: ComputationState
    /// Whether the artefact ever been saved
    IsTracked: bool
    ProducedVersionStorages: string list
    CurrentDiskVersionStorages: string list
}

[<AbstractClass>]
type StatusGraphNode(depCount,outCount) = 
    inherit ExecutableMethod(System.Guid.NewGuid(), [ for i in 0..(depCount-1) -> typeof<ArtefactStatus>] , [for i in 0..(outCount-1) -> typeof<ArtefactStatus>])

    member s.OutputCount =
        outCount

let createDataState (artefactVert:DependencyGraph.ArtefactVertex) =
    let isOnDisk = artefactVert.ActualHash.IsSome
    let state = 
        if isOnDisk then  
            // something is on disk
            if List.isEmpty artefactVert.StoragesContainingActualHash then
                Unsaved
            else
                Saved
        else
            Absent
    isOnDisk,state


type SourceGraphNode(orphanArtefact:DependencyGraph.VersionedArtefact) =
    inherit StatusGraphNode(0,1)

    override s.Execute(_, _) = //ignoring inputs and checkpoint.
        // Just utilizing parallel method computation feature of AngaraFlow to check the statuses of all method vertex

        let isOnDisk,dataState = createDataState(orphanArtefact.Artefact)
            
        let compState = 
            // source artefacts are either restorabe or up to date
            if isOnDisk then
                if orphanArtefact.Artefact.ActualHash.Value <> orphanArtefact.Version then
                    if not (List.isEmpty orphanArtefact.StoragesContainingVersion) then
                        Restorable
                    else
                        UpToDate
                else
                    UpToDate
            else
                if not (List.isEmpty orphanArtefact.StoragesContainingVersion) then
                    Restorable
                else
                    invalidOp (sprintf "Source artefact does not exist on disk and is not restorable: %s" (fullIDtoString orphanArtefact.Artefact.FullID))

                

        // source method is always up to date or restorable, thus succeeds                        
        let result = {
            FullID= orphanArtefact.Artefact.FullID;
            IsTracked = orphanArtefact.Artefact.IsTracked
            DataState = dataState
            ComputationState = compState
            ProducedVersionStorages = orphanArtefact.StoragesContainingVersion
            CurrentDiskVersionStorages = orphanArtefact.Artefact.StoragesContainingActualHash
        }

        seq{ yield [result :> Artefact], null }

type NotSourceGraphNode(methodVertex:DependencyGraph.ComputedVertex) =
    inherit StatusGraphNode(methodVertex.Inputs.Count, methodVertex.Outputs.Count)

    member s.FirstOutputID =
        methodVertex.FirstOutputFullID

    override s.Execute(inputs, _) = //ignoring checkpoint.
        let inputs = inputs |> List.map (fun x -> x:?> ArtefactStatus)
        // Just utilizing parallel method computation feature of AngaraFlow to check the statuses of all method vertex

        // There can be 3 reasons why the node can be outdated (execution halts)
        // a) inputs are outdated
        // b) one of the inputs hash does not match
        // c) one of the output hashes does nor match        

        let outputsArray = methodVertex.Outputs |> Set.toArray

        let outputToStatus idx isUpToDate = 
            let output = outputsArray.[idx]
            let isOnDisk,dataState = createDataState output.Artefact
            let compState = 
                let isNotReady (artStatus:ArtefactStatus) = 
                    match artStatus.ComputationState with
                    |   ComputationState.UpToDate -> false
                    |   ComputationState.Failed -> true
                    |   ComputationState.Restorable -> true
                    |   ComputationState.Computing -> true
                    |   ComputationState.InputsNotReady -> true
                    |   ComputationState.ReadyForComputation -> true
                    |   ComputationState.Restoring -> true
                if List.exists isNotReady inputs then
                    InputsNotReady
                else
                    if isOnDisk then
                        if output.Artefact.ActualHash.Value = output.Version then
                            // failed or upToDate

            {
                FullID = output.Artefact.FullID
                IsTracked = output.Artefact.IsTracked
                DataState = dataState
                ComputationState = compState
                ProducedVersionStorages = output.StoragesContainingVersion
                CurrentDiskVersionStorages = output.Artefact.StoragesContainingActualHash
            }

        let outdatedResult = seq { yield (List.init methodVertex.Outputs.Count (fun i -> outputToStatus i false:> Artefact) , null) }

        let isVersionMismatch expected actual =
            match actual with
            |   None -> false // if actual file is missing. That's OK. There is no version mismatch
            |   Some(actual) -> expected <> actual

        // checking a)
        if List.exists (fun i -> not i.IsUpToDate) inputs then
            outdatedResult
        else                    
            if
                // Checking b)
                (Seq.exists (fun (input:DependencyGraph.VersionedArtefact) -> isVersionMismatch input.Version input.Artefact.ActualHash) methodVertex.Inputs) ||
                // Checking c)
                (Seq.exists (fun (output:DependencyGraph.VersionedArtefact) -> isVersionMismatch output.Version output.Artefact.ActualHash) methodVertex.Outputs) then
                outdatedResult            
            else
                let results = List.init methodVertex.Outputs.Count (fun i -> outputToStatus i true:> Artefact)
                seq { yield (results, null) }                

let buildStatusGraph (g:DependencyGraph.Graph) =    
    let factory method : StatusGraphNode =
        match method with
        |   DependencyGraph.Source(source) -> upcast SourceGraphNode(source.Artefact)
        |   DependencyGraph.Computed(computed) -> upcast NotSourceGraphNode(computed)
        |   DependencyGraph.NotSetYet -> invalidArg "method" "All of the dependency graph nodes must be filled in before construction of status graph"
    FlowGraphFactory.buildFlowGraph g factory
        
let printStatuses (g:FlowGraph<StatusGraphNode>) =
    let state = 
        {
            TimeIndex = 0UL
            Graph = g
            Vertices = Map.empty
        }
    try
        use engine = new Engine<StatusGraphNode>(state,Scheduler.ThreadPool())        
        engine.Start()
        // engine.Changes.Subscribe(fun x -> x.State.Vertices)
        let final = Control.pickFinal engine.Changes
        let finalState = final.GetResult()
        
        let getVertexOutdatedOutputs (vertex:StatusGraphNode) =
            let N = vertex.OutputCount
            Seq.init N (fun idx -> Control.outputScalar(vertex,idx) finalState ) |> Seq.choose (fun (s:ArtefactStatus) -> if s.IsUpToDate then None else Some(s.FullID))
        
        let getVertexOutputStatusStrings (vertex:StatusGraphNode) =
            let N = vertex.OutputCount
            let statusToStr s =
                let diskStatus = if s.IsOnDisk then "on disk" else "absent"
                let uptToDateStatus = if s.IsUpToDate then "up to date" else "needs (re)computation"
                let expectedVerStorages = 
                    if List.length s.ProducedVersionStorages > 0 then
                        sprintf "restorable from %s" (String.Join(",",s.ProducedVersionStorages))
                    else
                        String.Empty
                let actualVerStorages = 
                    if List.length s.CurrentDiskVersionStorages > 0 then
                        sprintf "Saved in %s" (String.Join(",",s.ProducedVersionStorages))
                    else
                        "Unsaved"
                let storagesStatus = 
                    if s.IsTracked then
                        if s.IsOnDisk then
                            actualVerStorages
                        else
                            expectedVerStorages
                    else String.Empty                    
                s.FullID, (sprintf "%s\t%s\t%s" diskStatus uptToDateStatus storagesStatus)
            Seq.init N (fun idx -> Control.outputScalar(vertex,idx) finalState ) |> Seq.map statusToStr

        let outdatedArtefacts = finalState.Graph.Structure.Vertices |> Set.toSeq |> Seq.collect getVertexOutdatedOutputs
        let allArtefactStatuses = finalState.Graph.Structure.Vertices |> Set.toSeq |> Seq.collect getVertexOutputStatusStrings |> Seq.sortBy fst |> Seq.map (fun x -> let id,status = x in sprintf "%10s:\t%s" (fullIDtoString id) status)
        let statuses = String.Join("\n\t", allArtefactStatuses)
        printfn "Statuses:\n\t%s" statuses
        0
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
        printfn "Failed to compute the artefacts: \n\t%s" failed
        1