namespace ItisLab.Alpheus

open ItisLab.Alpheus.DependencyGraph

type DependencyGraphToAngaraWrapper(graph: Graph) =
    interface ItisLab.Alpheus.AngaraTranslator.IDependencyGraph<ArtefactVertex, MethodVertex> with
        member x.Artefacts = graph.Artefacts |> Seq.ofArray
        
        member x.InputsOf method = 
            match method with
            | Source _ -> invalidOp "Source method has no inputs"
            | Command cmd -> cmd.Inputs |> Seq.map (fun a -> a.Artefact)

        member x.OutputsOf method =
            match method with
            | Source src -> Seq.singleton src.Artefact.Artefact
            | Command cmd -> cmd.Outputs |> Seq.map (fun a -> a.Artefact)

        member x.ProducedBy artefact = artefact.ProducedBy

        member x.UsedIn artefact = artefact.UsedIn |> Set.toSeq |> Seq.map Command