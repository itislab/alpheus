module ItisLab.Alpheus.FlowGraphFactory

open Angara.Graph
open System

/// builds an Angara Flow graph from Dependency graph, transforming dependency node in behavior specific flow nodes using supplied factories
let buildFlowGraph<'Node when 'Node :> IVertex and 'Node:comparison> (g:DependencyGraph.Graph) (nodeFactory: DependencyGraph.ProducerVertex -> 'Node)=        

    let registerArtefactRelatedMethods (artefact:DependencyGraph.ArtefactVertex) graph allocatedNodes=
        // Every artefact has a producer (source or non-source method)
        // getting (allocating if needed) it
        let producerMethod, graph, allocatedNodes =
            match Map.tryFind artefact.FullID allocatedNodes with
            |   None ->
                let node = nodeFactory artefact.ProducedBy
                let allocatedNodes = Map.add artefact.FullID node allocatedNodes
                node,(FlowGraph.add node graph), allocatedNodes
            |   Some(preallocated) ->
                preallocated,graph,allocatedNodes
        let outputPortIndex = 
            match artefact.ProducedBy with                        
            |   DependencyGraph.ProducerVertex.NotSetYet -> raise(InvalidOperationException("NotYetSet vertex detected"))
            |   DependencyGraph.ProducerVertex.Source _ ->   0 // source artefacts are always have 0 out port index
            |   DependencyGraph.ProducerVertex.Computed(computed) ->
                // There is intermediate producer method, checking whether we have already added it or not yet                                    
                // Determining the producer port number that outputs the current artefact
                Seq.findIndex (fun (va:DependencyGraph.VersionedArtefact) -> va.Artefact = artefact) computed.Outputs                
        
        // the artefact may be used in some other methods as input.
        // connecting these consumer methods with the producer of current artefact
        let folder acc method =
            let graph, allocatedNodes = acc            
            
            // determining current method input port
            match method with
            |   DependencyGraph.ProducerVertex.NotSetYet -> raise(InvalidOperationException("NotYetSet vertex detected"))
            |   DependencyGraph.ProducerVertex.Source _ ->  raise(InvalidOperationException("Source method can't be among artefact outputs"))                
            |   DependencyGraph.ProducerVertex.Computed(consumer) ->
                // There is intermediate producer method, checking whether we have already added it or not yet                                    
                // Determining the producer port number that outputs the current artefact                
                let consumerInputPort = Seq.findIndex (fun (va:DependencyGraph.VersionedArtefact) -> va.Artefact = artefact) consumer.Inputs
                // getting or allocating consumer node

                let node,graph,allocatedNodes =
                    match Map.tryFind consumer.FirstOutputFullID allocatedNodes with
                    |   None ->
                        let node = nodeFactory method
                        node, (FlowGraph.add node graph), (Map.add consumer.FirstOutputFullID node allocatedNodes)
                    |   Some(node) -> node, graph,allocatedNodes
                
                let graph = FlowGraph.connect (producerMethod,outputPortIndex) (node,consumerInputPort) graph
                (graph, allocatedNodes)
        Seq.fold folder (graph,allocatedNodes) (artefact.UsedIn |> Seq.map (fun computed -> DependencyGraph.ProducerVertex.Computed(computed)))
    
    let registering_folder acc artefact =    
        let graph, allocatedNodes = acc
        registerArtefactRelatedMethods artefact graph allocatedNodes
    let graph, _ = Seq.fold registering_folder (FlowGraph.Empty, Map.empty) g.Artefacts
    graph