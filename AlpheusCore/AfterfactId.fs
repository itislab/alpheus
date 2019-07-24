module ItisLab.Alpheus.Artefacts

open System
open System.IO

let rank (artefactId:ArtefactId) =
        // todo: encapsulate all pattern-related logic and validation here
        // add strict rules on the pattern
        artefactId.ToString() |> Seq.filter (fun c -> c = '*') |> Seq.length
