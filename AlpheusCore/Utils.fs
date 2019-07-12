module ItisLab.Alpheus.Utils

open ItisLab.Alpheus.AlphFiles
open System.IO
open System


let getArtefactId (experimentRoot: string) (artefactPath: string) =
    let relativePath = Path.GetRelativePath(experimentRoot, artefactPath)
    if (Path.IsPathRooted artefactPath) && relativePath = artefactPath then raise (ArgumentException("The given artefact path is not under the experiment root"))
    ArtefactFullID.ID relativePath
