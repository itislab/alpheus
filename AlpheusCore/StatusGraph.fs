/// The module computes the statuses of the artefacts
module ItisLab.Alpheus.StatusGraph

open Angara.Graph
open Angara.Execution
open Angara.States
open AlphFiles
open System
open ItisLab.Alpheus
open System.IO
open ItisLab.Alpheus.PathUtils
open ItisLab.Alpheus.AngaraGraphCommon
open DependencyGraph
open Angara.Data

type ArtefactItem =
    { ArtefactId: ArtefactId
      Index: string list
      Status: MethodInstanceStatus
      }


type SourceMethod(source: SourceVertex, experimentRoot, checkStoragePresense) = 
    inherit AngaraGraphNode<ArtefactItem>(DependencyGraph.Source source)

    override s.Execute(_, _) = // ignoring checkpoints
        async{
            let expectedArtefact = source.Output
            let artefact = expectedArtefact.Artefact

            let getItemStatus (link:LinkToArtefact) index =            
                link.AnalyzeStatus checkStoragePresense index

            // Output of the method is an scalar or a vector of full paths to the data of the artefact.
            // disk indices (if any on disk) specify the actual vector indices
            let diskIndicesMap =
                artefact.Id 
                |> PathUtils.enumerateItems experimentRoot
            let diskIndices =
                diskIndicesMap
                |> MdMap.toSeq |> Seq.map fst |> Array.ofSeq

            let! result =
                async {
                    let linkStatusToCommandVertextStatus status =
                        match status with
                        |   LocalUnexpected ->
                            // Source methods always updates their output.
                            // Always valid if there is any disk version of the artefact
                            UpToDate [ArtefactLocation.Local]
                        |   NotFound ->
                            // that's bad! as there is not local nor remote version
                            // we can't get the artefact data by any means
                            // this is exception
                            raise (ArtefactNotFoundInStoragesException "The artefact data is not found neither on disk nor in any of the available storages")
                        |   Local -> UpToDate [ArtefactLocation.Local]
                        |   Remote -> UpToDate [ArtefactLocation.Remote]
                
                    if diskIndicesMap.IsScalar then
                        let! linkStatus = getItemStatus expectedArtefact List.empty
                        let toRet = 
                            {
                                ArtefactId = artefact.Id;
                                Index = [];
                                Status = linkStatusToCommandVertextStatus linkStatus
                            }
                        return [toRet :> Artefact] 
                    else
                        let indices =
                            if MdMap.toShallowSeq diskIndicesMap |> Seq.isEmpty then
                                // check remote up to date
                                let expectedIndices = expectedArtefact.ExpectedVersion

                                if not expectedIndices.IsScalar && (MdMap.toShallowSeq expectedIndices |> Seq.isEmpty) then
                                    raise (ZeroLengthVectorException (sprintf "No disk data matches source vector pattern for artefact %O" artefact ))
                                else
                                    MdMap.toSeq expectedIndices |> Seq.map fst |> Seq.toArray
                                
                            else
                                // TODO: check issue #89
                                diskIndices
                            
                        let! itemStatuses = indices |> Array.map (getItemStatus expectedArtefact) |> Async.Parallel
                        let toRet =
                            Array.map2 (fun index status -> {ArtefactId = artefact.Id; Index = index; Status = linkStatusToCommandVertextStatus status}) diskIndices itemStatuses
                            |> Seq.map (fun x -> x :> Artefact)
                            |> List.ofSeq
                        return toRet
                }
            
            return seq{ yield (result, null) }
        } |> Async.RunSynchronously
        

type CommandMethod(command: CommandLineVertex,
                    experimentRoot,
                    checkStoragePresence: HashString seq -> Async<bool array>) =
    inherit AngaraGraphNode<ArtefactItem>(DependencyGraph.Command command)  

    override s.Execute(inputs, _) = //ignoring checkpoint.
        async{
            // Rules of execution
            // The artefact is valid either if actual disk version matches expected version or if the disk version is absent and expected version is restorable from storage
            // We can bypass the computation entirely if inputs and outputs are valid
            
            let inputItems = inputs |> List.map (fun inp -> inp :?> ArtefactItem)
            let index =
                inputItems 
                |> Seq.map(fun item -> item.Index)
                |> Seq.fold(fun (max: string list) index -> if index.Length > max.Length then index else max) []
            
            let isUpToDate status =
                match status with
                |   UpToDate _ -> true
                |   Outdated _ -> false

            let areInputsValid = inputItems |> Seq.map (fun x -> x.Status) |> Seq.forall isUpToDate

            let! currentVertexStatus =
                async {
                    if not areInputsValid then
                        // shortcut: just propagating outdated status down the graph
                        return Outdated InputsOutdated
                    else
                        // actually checking current vertex
                        return! getCommandVertexStatus checkStoragePresence command index
                }
       
            let outputIds =
                command.Outputs // the order is important here
                |> List.map(fun out -> out.Artefact.Id)

            let prepareAngaraArtefact artId : Artefact =
                upcast {Index = index; Status = currentVertexStatus; ArtefactId= artId }
            let result =  Seq.singleton(List.map prepareAngaraArtefact outputIds, null)
            return result
        } |> Async.RunSynchronously

let buildStatusGraph (g:DependencyGraph.Graph) experimetRoot checkStoragePresence =    
    let factory method : AngaraGraphNode<ArtefactItem> =
        match method with
        |   DependencyGraph.Source(source) -> upcast SourceMethod(source, experimetRoot, checkStoragePresence)
        |   DependencyGraph.Command(computed) -> upcast CommandMethod(computed, experimetRoot, checkStoragePresence)
    g |> DependencyGraphToAngaraWrapper |> AngaraTranslator.translate factory

type ArtefactStatus =
|   UpToDate of ArtefactLocation
|   NeedsRecomputation of OutdatedReason
        
let getStatuses (g:FlowGraph<AngaraGraphNode<ArtefactItem>>) =
    let state = 
        {
            TimeIndex = 0UL
            Graph = g
            Vertices = Map.empty
        }
    try
        use engine = new Engine<AngaraGraphNode<ArtefactItem>>(state,Scheduler.ThreadPool())        
        engine.Start()
        // engine.Changes.Subscribe(fun x -> x.State.Vertices)
        let final = Control.pickFinal engine.Changes
        let finalState = final.GetResult()
               
        let vertices = finalState.Vertices

        let vertexStateToStatus state =
            let toArtefactItemStatus (x:MethodOutput) =
                let outputsCount = (x :> IVertexData).Shape.Length
                let methodInstanceStatusToOutputStatus (status:MethodInstanceStatus) outputIdx =
                    match status with
                    |   MethodInstanceStatus.UpToDate outputs ->
                        ArtefactStatus.UpToDate(List.item outputIdx outputs)
                    |   Outdated reason ->
                        NeedsRecomputation reason
                let outputNumToRes idx : (ArtefactId * string list* ArtefactStatus) =
                    let artItem: ArtefactItem = downcast x.TryGet(idx).Value
                    artItem.ArtefactId, artItem.Index, (methodInstanceStatusToOutputStatus artItem.Status idx)
                Seq.init outputsCount outputNumToRes
            let itemsStatus = state |> MdMap.toSeq |> Seq.collect (fun x -> let _,v = x in v.Data.Value |> toArtefactItemStatus)
            itemsStatus

        let verticesPairs = vertices  |> Map.toSeq |> Seq.collect (fun x -> let _,output = x in (vertexStateToStatus output))

        // assembling MdMap back        
        let folder (state:Map<ArtefactId,MdMap<string,ArtefactStatus>>) (elem: ArtefactId * string list* ArtefactStatus) =
            let artId, index, status = elem
            let artMap =
                match Map.tryFind artId state with
                |   Some m -> m
                |   None -> MdMap.empty
            let updatedArtMap = MdMap.add index status artMap
            Map.add artId updatedArtMap state
        let result = Seq.fold folder Map.empty verticesPairs
        
        Ok(result)
    with 
    | :? Control.FlowFailedException as flowExc -> 
        let failed = String.Join("\n\t", flowExc.InnerExceptions |> Seq.map(fun e -> e.ToString()))
        Error(SystemError(sprintf "Failed to compute the artefacts: \n\t%s" failed))