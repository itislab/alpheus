/// The module computes the statuses of the artefacts
module ItisLab.Alpheus.StatusGraph

open Angara.Graph
open Angara.Execution
open Angara.States
open AlphFiles
open System
open ItisLab.Alpheus
open ItisLab.Alpheus.PathUtils

type ArtefactStatus = {
    Id: ArtefactId
    IsOnDisk: bool
    IsUpToDate: bool
    IsTracked: bool
    ProducedVersionStorages: string list
    CurrentDiskVersionStorages: string list
}

[<AbstractClass>]
type StatusGraphNode(depCount,outCount) = 
    inherit ExecutableMethod(System.Guid.NewGuid(), [ for i in 0..(depCount-1) -> typeof<ArtefactStatus>] , [for i in 0..(outCount-1) -> typeof<ArtefactStatus>])

    member s.OutputCount =
        outCount

type SourceGraphNode(orphanArtefact:DependencyGraph.VersionedArtefact) =
    inherit StatusGraphNode(0,1)

    override s.Execute(_, _) = //ignoring inputs and checkpoint.
        // Just utilizing parallel method computation feature of AngaraFlow to check the statuses of all method vertex

        let isOnDisk = orphanArtefact.Artefact.ActualHash.IsSome

        // source method is always up to date, thus succeeds                        
        let result = {
            Id = orphanArtefact.Artefact.Id;
            IsUpToDate = (not isOnDisk) || (orphanArtefact.Artefact.ActualHash.Value = orphanArtefact.Version.Value);
            IsOnDisk = isOnDisk;
            IsTracked = orphanArtefact.Artefact.IsTracked
            ProducedVersionStorages = orphanArtefact.StoragesContainingVersion
            CurrentDiskVersionStorages = orphanArtefact.Artefact.StoragesContainingActualHash
        }

        seq{ yield [result :> Artefact], null }

type NotSourceGraphNode(methodVertex:DependencyGraph.CommandLineVertex) =
    inherit StatusGraphNode(methodVertex.Inputs.Count, methodVertex.Outputs.Count)

    member s.FirstOutputID =
        methodVertex.MethodId

    override s.Execute(inputs, _) = //ignoring checkpoint.
        let inputs = inputs |> List.map (fun x -> x:?> ArtefactStatus)
        // Just utilizing parallel method computation feature of AngaraFlow to check the statuses of all method vertex

        // There can be 3 reasons why the node can be outdated (execution halts)
        // a) inputs are outdated
        // b) one of the inputs hash does not match
        // c) one of the output hashes does nor match        

        let outputs = methodVertex.Outputs
        let outputToStatus idx isUpToDate = 
            let output = outputs.[idx]
            {
                Id = output.Artefact.Id
                IsUpToDate = isUpToDate
                IsOnDisk =
                    match output.Artefact.ActualHash with
                    |   None -> false
                    |   Some(_) -> true
                IsTracked = output.Artefact.IsTracked
                ProducedVersionStorages = output.StoragesContainingVersion
                CurrentDiskVersionStorages = output.Artefact.StoragesContainingActualHash
            }

        let outdatedResult = seq { yield (List.init methodVertex.Outputs.Count (fun i -> outputToStatus i false:> Artefact) , null) }

        let isVersionMismatch expected actual =
            match expected,actual with
            |   _,None -> false // if actual file is missing. That's OK. There is no version mismatch
            |   Some(expected),Some(actual) -> expected <> actual
            |   _ -> raise(NotImplementedException("need to define behavior"))

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
        |   DependencyGraph.Command(computed) -> upcast NotSourceGraphNode(computed)
    g |> DependencyGraphToAngaraWrapper |> AngaraTranslator.translate factory
        
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
            Seq.init N (fun idx -> Control.outputScalar(vertex,idx) finalState ) |> Seq.choose (fun (s:ArtefactStatus) -> if s.IsUpToDate then None else Some(s.Id))
        
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
                s.Id, (sprintf "%s\t%s\t%s" diskStatus uptToDateStatus storagesStatus)
            Seq.init N (fun idx -> Control.outputScalar(vertex,idx) finalState ) |> Seq.map statusToStr

        let outdatedArtefacts = finalState.Graph.Structure.Vertices |> Set.toSeq |> Seq.collect getVertexOutdatedOutputs
        let allArtefactStatuses = finalState.Graph.Structure.Vertices |> Set.toSeq |> Seq.collect getVertexOutputStatusStrings |> Seq.sortBy fst |> Seq.map (fun x -> let id,status = x in sprintf "%10A:\t%s" id status)
        let statuses = String.Join("\n\t", allArtefactStatuses)
        printfn "Statuses:\n\t%s" statuses
        0
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
        printfn "Failed to compute the artefacts: \n\t%s" failed
        1