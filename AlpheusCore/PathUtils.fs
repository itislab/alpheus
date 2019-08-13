module ItisLab.Alpheus.PathUtils

open System
open System.IO
open Angara.Data

/// conversion to OS specific directory delimiter
let internal normalizePath (path:string) = path.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar).Trim()
let unixPath (path:string) = path.Replace('\\', '/')

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

let private unvectorizePath (artefactPath:string) : string =
    let artefactPath = normalizePath artefactPath
    let prefix = 
        if artefactPath.EndsWith(Path.DirectorySeparatorChar) then
            artefactPath.Substring(0,artefactPath.Length-1)
        else
            artefactPath
    let idx = prefix.IndexOf('*')
    if idx < 0 then 
        prefix
    else
        prefix.Substring(0, idx) + prefix.Substring(idx).Replace(Path.DirectorySeparatorChar, '-').Replace("*", "vector")


/// Given the artefact path of any type, returns the path to the corresponding alph file of the same type as the original path.
let pathToAlphFile (artefactPath:string) : string =
    let prefix = unvectorizePath artefactPath    
    sprintf "%s.alph" prefix 

/// Given the artefact path of any type, returns the path to the corresponding hash file of the same type as the original path.
let pathToHashFile (artefactPath:string) : string =
    let prefix = unvectorizePath artefactPath    
    sprintf "%s.hash" prefix 

/// Given the artefact id, returns the path to the corresponding alph file (relative to the experiment root).
let idToAlphFilePath (artefactId:ArtefactId) : ExperimentRelativePath =
    artefactId |> idToExperimentPath |> pathToAlphFile 

/// Given the artefact id, returns the full path to the corresponding alph file.
let idToAlphFileFullPath (experimentRoot: string) (artefactId:ArtefactId) : string =
    let experimentRoot = normalizePath experimentRoot
    if not(Path.IsPathRooted experimentRoot) then invalidArg "experimentRoot" "Experiment root is not absolute"
    if not(isDirectory experimentRoot) then invalidArg "experimentRoot" "Experiment root is not a directory (must end with the slash)"
    Path.Combine(experimentRoot, idToAlphFilePath artefactId)

/// Builds an instance of MdMap which contains all files or directories (depending whether the given ends with the sepator or not),
/// satisfying the given artefact path pattern.
/// In case of a vector, the keys of the MdMap instance contain a concrete replacement string for every asterisk.
/// todo: document rules that file is expected to have * instead of file name without extension and that will be the key.
let enumeratePath (artefactPath:string) : MdMap<string, string> =
    let isDirectory = isDirectory artefactPath
    let isHidden (name:string) = name.StartsWith(".") || name.ToLower().EndsWith(".hash")
    let isPattern (name:string) = name.Contains("*")
    
    let rec enumerate (path: string) (parts: string list) : MdMap<string, string> =
        match parts with
        | [] -> 
            MdMap.scalar path
        | head :: _ when isHidden head ->
            MdMap.empty
        | [head] when isDirectory -> 
            DirectoryInfo(path).GetDirectories(head) 
            |> Seq.filter(fun fi -> not(isHidden fi.Name)) 
            |> Seq.fold (fun map fi -> map |> MdMap.add [fi.Name] fi.FullName) MdMap.empty
        | [head] -> 
            DirectoryInfo(path).GetFiles(head) 
            |> Seq.filter(fun fi -> not(isHidden fi.Name)) 
            |> Seq.fold (fun map fi -> map |> MdMap.add [Path.GetFileNameWithoutExtension fi.Name] fi.FullName) MdMap.empty
        | head :: tail ->
            sprintf "%s :: %A" head tail |> Logger.logInfo Logger.Test
            sprintf "Looking for %s in %s" head path |> Logger.logInfo Logger.Test
            let found = Directory.EnumerateDirectories(path, head) |> Seq.toArray
            sprintf "Found %A" found |> Logger.logInfo Logger.Test
            DirectoryInfo(path).GetDirectories(head) 
            |> Seq.filter(fun fi -> not(isHidden fi.Name)) 
            |> Seq.map(fun fi -> (fi.Name, enumerate fi.FullName tail)) 
            |> Seq.fold(fun map (fi, mapForFi) -> map |> MdMap.set [fi] mapForFi) MdMap.empty

           
    // Combines the top items of the path unless a pattern item is found.
    let rec rootPath path (parts : string list) : (string * string list) =
        match parts with
        | [] -> (path, [])
        | head :: tail when not(isPattern(head)) -> rootPath (if String.IsNullOrEmpty(path) then head else Path.Combine(path, head)) tail
        | _ -> (path, parts)

    let parts = artefactPath.Split([|Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let (root, partsWithPattern) = rootPath String.Empty parts
    enumerate root partsWithPattern          

/// Returns for all items (files or directories) corresponding to the given artefact id.
let enumerateItems (experimentRoot: string) (artefactId: ArtefactId) =
    let artefactPath = artefactId |> idToFullPath experimentRoot 
    enumeratePath artefactPath

/// If exists, deletes a file or a directory (recursively) given the path.
let deletePath (path:string) =
    let path = normalizePath path
    if path.EndsWith(Path.DirectorySeparatorChar) then
        Directory.Delete(path,true)
    else
        File.Delete path


/// Substitutes index of string values into a pattern command, e.g. "/files/*/*".
/// Note that index length is allowed to be less or equal to the rank of the command.
let rec applyIndex (index: string list) (path: string) =
    match index with
    | [] -> path
    | head :: tail ->
        let idx = path.IndexOf "*"
        if idx < 0 then 
            path
        else 
            let newCommand = path.Substring(0, idx) + head + path.Substring(idx+1)
            applyIndex tail newCommand