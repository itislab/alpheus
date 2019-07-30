module ItisLab.Alpheus.PathUtils

open System
open System.IO

/// conversion to OS specific directory delimiter
let internal normalizePath (path:string) = path.Replace('/', Path.DirectorySeparatorChar).Trim()
let internal unixPath (path:string) = path.Replace('\\', '/')

let isDirectory (path:string) = path.EndsWith(Path.DirectorySeparatorChar) || path.EndsWith(Path.AltDirectorySeparatorChar)

let isAlphFile (path:string) = path.ToLowerInvariant().EndsWith(".alph")

/// Transforms targetPath so it is relative to the basePath.
/// Note that basePath is allowed to be both folder and file. If it is a file, its folder will be used (see isDirectory function).
let relativePath (basePath:string) (targetPath:string) : string =
    let baseFolder = Path.GetDirectoryName(normalizePath basePath)
    let normalizedTargetPath = normalizePath targetPath
    let relativePath = Path.GetRelativePath(baseFolder, normalizedTargetPath)
    if (Path.IsPathRooted targetPath) && relativePath = normalizedTargetPath then invalidArg "targetPath" "The artefact path is not under the given basePath"
    relativePath

/// Returns a path to an artefact relative to the experiment root, given the artefact id.
/// For vector arterfacts, that path will contain '*'.
let idToExperimentPath (artefactId:ArtefactId) : ExperimentRelativePath = match artefactId with Path path -> normalizePath path

/// Returns the full path to the artefact, given the artefact id.
/// For vector arterfacts, that path will contain '*'.
let idToFullPath (experimentRoot: string) (artefactId:ArtefactId) = 
    let experimentRoot = normalizePath experimentRoot
    if not (Path.IsPathRooted experimentRoot) then invalidArg "experimentRoot" "The path is relative"
    Path.GetFullPath(Path.Combine(experimentRoot, idToExperimentPath artefactId))

/// Returns ArtefactId built from the given path, which can be an either option:
/// - A path to the artefact, either full or relative to the current directory.
/// - A path to artefact's alph file
let rec pathToId (experimentRoot: string) (path: string) : ArtefactId =
    let experimentRoot = normalizePath experimentRoot
    let path = normalizePath path
    if not(isDirectory experimentRoot) then invalidArg "experimentRoot" "The path is not a directory"
    match isAlphFile path with
    | true ->
        // we need to know whether the corresponding artefact is directory (e.g. its ID ends with '\') or single file
        // to figure this out we read the alph file content. First output by convention corresponds to the associated artefact (NOTE: each artefact has correspondent alph file)
        match AlphFiles.tryLoad path with
        | None -> failwith (sprintf "Couldn't load the file: %s" path)
        | Some alphFile ->
            let relative =
                match alphFile.Origin with
                | AlphFiles.CommandOrigin cmd when cmd.Outputs.Length <= cmd.OutputIndex ->
                    invalidOp "The alph file does not specify correct number of output artefacts"
                | AlphFiles.CommandOrigin cmd ->
                    cmd.Outputs.[cmd.OutputIndex].RelativePath
                | AlphFiles.SourceOrigin snap ->
                    snap.RelativePath 
            relative |> alphRelativePathToId path experimentRoot
            
    | false ->
        let relativePath = relativePath experimentRoot path
        ArtefactId.Path (unixPath relativePath)

/// Transforms a path relative to the alph file into ArtefactId.
and alphRelativePathToId (alphFile: string) (experimentRoot: string) (relativePath: AlphRelativePath) : ArtefactId =
    let experimentRoot = normalizePath experimentRoot
    let relativePath = normalizePath relativePath
    let alphFile = normalizePath alphFile
    if Path.IsPathRooted relativePath then invalidArg "relativePath" "The path is absolute"
    let alphFolder = Path.GetDirectoryName(alphFile) 
    let fullPath = Path.GetFullPath(Path.Combine(alphFolder, relativePath))
    pathToId experimentRoot fullPath

/// Given the artefact path of any type, returns the path to the corresponding alph file of the same type as the original path.
let pathToAlphFile (artefactPath:string) : string =
    let artefactPath = normalizePath artefactPath
    let prefix = 
        if artefactPath.EndsWith(Path.DirectorySeparatorChar) then
            artefactPath.Substring(0,artefactPath.Length-1)
        else
            artefactPath
    let idx = prefix.IndexOf('*')
    let prefix =
        if idx < 0 then 
            prefix
        else
            prefix.Substring(0, idx) + prefix.Substring(idx).Replace(Path.DirectorySeparatorChar, '-').Replace("*", "vector")
    
    sprintf "%s.alph" prefix 

/// Given the artefact id, returns the path to the corresponding alph file (relative to the experiment root).
let idToAlphFilePath (artefactId:ArtefactId) : ExperimentRelativePath =
    artefactId |> idToExperimentPath |> pathToAlphFile 

/// Given the artefact id, returns the full path to the corresponding alph file.
let idToAlphFileFullPath (experimentRoot: string) (artefactId:ArtefactId) : string =
    let experimentRoot = normalizePath experimentRoot
    if not(Path.IsPathRooted experimentRoot) then invalidArg "experimentRoot" "Experiment root is not absolute"
    if not(isDirectory experimentRoot) then invalidArg "experimentRoot" "Experiment root is not a directory (must end with the slash)"
    Path.Combine(experimentRoot, idToAlphFilePath artefactId)