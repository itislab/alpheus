module ItisLab.Alpheus.DependencyGraphUtils

open Angara.Data     
open ItisLab.Alpheus
open ItisLab.Alpheus.PathUtils
open ItisLab.Alpheus.DependencyGraph


///// Fulls up the StoragesContainingActualHash of the artefacts
//let fillinArtefactContainingStoragesAsync (artefacts:ArtefactVertex seq) (getContainingStorageNames: (HashString option array -> Async<(string list) array>)) =
//    async {
//        // we fill in only artefacts that are on disk
//        let artefactItemHashes = 
//            artefacts
//            |> Seq.filter (fun (art:ArtefactVertex) -> art.ActualVersion.IsSome) 
//            |> Seq.map(fun artefact -> artefact.ActualVersion.Value |> MdMap.toSeq |> Seq.filter (snd >> Option.isSome) |> Seq.map(fun (key, hash) -> hash))
//            |> Seq.concat
//            |> Array.ofSeq

//        let! containigStorages = getContainingStorageNames artefactItemHashes
//        Array.iter2 (fun (art:ArtefactVertex) storages -> art.StoragesContainingActualHash <- storages ) artefactItemHashes containigStorages
//    }

///// fills up Inputs and Outputs of methods with the information about the storages that contain the mentioned versions
//let fillinMethodEdgeContainingStoragesAsync (methods:MethodVertex seq) (getContainingStorageNames: ((HashString option) array -> Async<(string list) array>)) =
//    async {
//        let methodsArray = Array.ofSeq methods
//        // gathering versions
        
//        let extractExpectedVersions method =
//            match method with
//            |   Source(sourceVertex) -> seq { yield sourceVertex.Output}
//            |   Command(computedVertex) ->
//                seq {
//                    yield! computedVertex.Inputs;
//                    yield! computedVertex.Outputs
//                }
//        let allVersions = methodsArray |> Seq.collect extractExpectedVersions |> Seq.map (fun art -> art.ExpectedVersion) |> Array.ofSeq
//        let! containingStorages = getContainingStorageNames allVersions
//        let storagesMap = Seq.zip allVersions containingStorages |> Map.ofSeq
        
//        let updateVersionedArtefact (art:VersionedArtefact) =
//            { art with StoragesContainingVersion = Map.find art.Version storagesMap }
//        let iterator vertex =
//            match vertex with
//            |   Source(sourceVertex) -> sourceVertex.Output <- updateVersionedArtefact sourceVertex.Artefact
//            |   Command(computedVertex) -> computedVertex.UpdateArtefacts updateVersionedArtefact
//        methodsArray |> Array.iter iterator      
//    }