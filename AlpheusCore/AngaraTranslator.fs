module ItisLab.Alpheus.AngaraTranslator

open Angara.Graph
open System

type IDependencyGraph<'Artefact, 'Method when 'Artefact: equality and 'Method: comparison> =
    abstract member Artefacts: seq<'Artefact> 
    abstract member InputsOf: 'Method -> seq<'Artefact>
    abstract member OutputsOf: 'Method -> seq<'Artefact>
    abstract member ProducedBy: 'Artefact -> 'Method
    abstract member UsedIn: 'Artefact -> seq<'Method>
    /// Rank of an artefact:
    ///  - Number of asterisks in the path
    ///  - 
    /// E.g. birds/*.csv --> data/*/metadata.csv
    ///      reduce: data/ --> statistics.csv
    abstract member RankOf: 'Artefact -> int


/// Creates new or returns an existing flow graph node corresponding to the given dependency graph vertex.
let private addOrGetNode flowGraph methodNodeMap nodeFactory method =
    match methodNodeMap |> Map.tryFind method with
    | Some node -> node, flowGraph, methodNodeMap
    | None ->
        let node = nodeFactory method
        let newMap = methodNodeMap |> Map.add method node
        let newGraph = flowGraph |> FlowGraph.add node
        node, newGraph, newMap

let translate<'Node,'Artefact,'Method when 'Node:>IVertex and 'Node:comparison and 'Artefact:equality and 'Method:comparison> nodeFactory (dependencyGraph:IDependencyGraph<'Artefact,'Method>) : FlowGraph<'Node> =
    let registerArtefactRelatedMethods artefact graph methodNodeMap =
        // Every artefact has a producer (source or non-source method)
        // getting (allocating if needed) it
        let producerNode, graph, methodNodeMap = 
            artefact 
            |> dependencyGraph.ProducedBy 
            |> addOrGetNode graph methodNodeMap nodeFactory
        let outputPortIndex = artefact |> dependencyGraph.ProducedBy |> dependencyGraph.OutputsOf |> Seq.findIndex (fun output -> output = artefact) 
           
        // the artefact may be used in some other methods as input.
        // connecting these consumer methods with the producer of current artefact
        let connectWithConsumer (graph, methodNodeMap) consumer =
            // determining current method input port
            // There is an intermediate producer method, we're checking whether we have already added it or not                                    
            // Determining the producer port number that outputs the current artefact                
            let consumerInputPort = consumer |> dependencyGraph.InputsOf |> Seq.findIndex (fun input -> input = artefact) 

            // We have pair (producer) ---> (consumer)
            // We have actual ranks of artefacts from the dependency graph.
            // We know the producer node rank. Hence we might need to add scatter nodes to equal ranks. 

            let consumerNode, graph, methodNodeMap = consumer |> addOrGetNode graph methodNodeMap nodeFactory
            let graph = graph |> FlowGraph.connect (producerNode,outputPortIndex) (consumerNode,consumerInputPort) 
            (graph, methodNodeMap)

        artefact
            |> dependencyGraph.UsedIn 
            |> Seq.fold connectWithConsumer (graph, methodNodeMap)
       
    let registerArtefact (graph, methodNodeMap) artefact =    
        registerArtefactRelatedMethods artefact graph methodNodeMap

    let graph, _ = dependencyGraph.Artefacts |> Seq.fold registerArtefact (FlowGraph.Empty, Map.empty)
    graph

