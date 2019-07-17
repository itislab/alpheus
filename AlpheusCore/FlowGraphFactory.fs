module ItisLab.Alpheus.FlowGraphFactory

open Angara.Graph

/// builds an Angara Flow graph from the dependency graph, transforming dependency nodes in behavior-specific flow nodes using the supplied factory
let buildGraph<'Node when 'Node :> IVertex and 'Node:comparison> (g:DependencyGraph.Graph) (nodeFactory: DependencyGraph.MethodVertex -> 'Node)=     

    /// Creates new or returns an existing flow graph node corresponding to the given dependency graph vertex.
    let addOrGetNode (method: DependencyGraph.MethodVertex) flowGraph methodNodeMap =
        match methodNodeMap |> Map.tryFind method.MethodId with
        | Some node -> node, flowGraph, methodNodeMap
        | None ->
            let node = nodeFactory method
            let newMap = methodNodeMap |> Map.add method.MethodId node
            let newGraph = flowGraph |> FlowGraph.add node
            node, newGraph, newMap

    let registerArtefactRelatedMethods (artefact:DependencyGraph.ArtefactVertex) graph methodNodeMap =
        // Every artefact has a producer (source or non-source method)
        // getting (allocating if needed) it
        let producerNode, graph, methodNodeMap = addOrGetNode artefact.ProducedBy graph methodNodeMap
        let outputPortIndex = 
            match artefact.ProducedBy with                        
            |   DependencyGraph.MethodVertex.Source _ -> 0 // source artefacts are always have 0 out port index
            |   DependencyGraph.MethodVertex.Command(computed) ->
                // There is intermediate producer method, checking whether we have already added it or not yet                                    
                // Determining the producer port number that outputs the current artefact
                Seq.findIndex (fun (va:DependencyGraph.VersionedArtefact) -> va.Artefact = artefact) computed.Outputs                
        
        // the artefact may be used in some other methods as input.
        // connecting these consumer methods with the producer of current artefact
        let connectWithConsumer (graph, methodNodeMap) consumer =
            // determining current method input port
            match consumer with
            |   DependencyGraph.MethodVertex.Source _ -> invalidOp "The artefact is used as an input of the source method"              
            |   DependencyGraph.MethodVertex.Command(command) ->
                // There is an intermediate producer method, we're checking whether we have already added it or not                                    
                // Determining the producer port number that outputs the current artefact                
                let consumerInputPort = command.Inputs |> Seq.findIndex (fun (va:DependencyGraph.VersionedArtefact) -> va.Artefact = artefact) 
                let consumerNode, graph, methodNodeMap = addOrGetNode consumer graph methodNodeMap
                let graph = graph |> FlowGraph.connect (producerNode,outputPortIndex) (consumerNode,consumerInputPort) 
                (graph, methodNodeMap)

        artefact.UsedIn 
            |> Seq.map DependencyGraph.MethodVertex.Command
            |> Seq.fold connectWithConsumer (graph, methodNodeMap)
    
    let registerArtefact (graph, methodNodeMap) artefact =    
        registerArtefactRelatedMethods artefact graph methodNodeMap

    let graph, _ = g.Artefacts |> Seq.fold registerArtefact (FlowGraph.Empty, Map.empty)
    graph