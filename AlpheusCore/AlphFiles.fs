module ItisLab.Alpheus.AlphFiles

open Newtonsoft.Json
open System.IO
open Hash
open System
open ItisLab.Alpheus

/// Path relative to the project root
/// Trailing slash indicates that the artifact is folder
type ArtefactFullID =
    ID of string

let isFullIDDirectory (fullID:ArtefactFullID) =
    match fullID with
    |   ArtefactFullID.ID s -> s.EndsWith(Path.DirectorySeparatorChar)

/// Path relative to some .alph file
/// Trailing slash indicates that the artifact is folder
type RelativeArtefactID = 
    ID of string

let isRelativeIDDirectory (relID:RelativeArtefactID) =
    match relID with
    |   RelativeArtefactID.ID s -> s.EndsWith(Path.DirectorySeparatorChar)

//GetDirectoryName('C:\MyDir\MySubDir\myfile.ext') returns 'C:\MyDir\MySubDir'
//GetDirectoryName('C:\MyDir\MySubDir') returns 'C:\MyDir'
//GetDirectoryName('C:\MyDir\') returns 'C:\MyDir'          <----- '\' at the end IS striped
//GetDirectoryName('C:\MyDir') returns 'C:\'
//GetDirectoryName('C:\') returns ''

// GetFullPath('mydir') returns 'C:\Users\dmitr\source\repos\PathsUtils\PathsUtils\bin\Debug\netcoreapp2.1\mydir'
// GetFullPath('myfile.ext') returns 'C:\Users\dmitr\source\repos\PathsUtils\PathsUtils\bin\Debug\netcoreapp2.1\myfile.ext'
// GetFullPath('\mydir') returns 'C:\mydir'
// GetFullPath('mydir\') returns 'C:\Users\dmitr\source\repos\PathsUtils\PathsUtils\bin\Debug\netcoreapp2.1\mydir\'            <----- '\' at the end is NOT striped

//  string[] paths = { @"d:\archives", "2001", "media", @"images\" };
//  Path.Combine(paths) returns d:\archives\2001\media\images\                            <----- '\' at the end is NOT striped

let fullIDtoString (fullID:ArtefactFullID) =
    match fullID with
        |   ArtefactFullID.ID s -> s

let relIDtoString (fullID:RelativeArtefactID) =
    match fullID with
        |   RelativeArtefactID.ID s -> s

let fullIDtoFullPath rootPath (fullID:ArtefactFullID) =
    let s =
        match fullID with
        |   ArtefactFullID.ID s -> s
    Path.GetFullPath(Path.Combine(rootPath, s))

let fullIDtoRelative rootPath (alphFileFullPath:string) (fullID:ArtefactFullID)  =
    let fullID = 
        match fullID with
        |   ArtefactFullID.ID s -> s
    let artefactAbsPath = Path.GetFullPath(Path.Combine(rootPath,fullID))    
    let fullAlphDir = Path.GetDirectoryName(alphFileFullPath)
           
    let relID:RelativeArtefactID = RelativeArtefactID.ID(Path.GetRelativePath(fullAlphDir,artefactAbsPath))
    relID

let relIDtoFullID rootPath (alphFileFullPath:string) (relID:RelativeArtefactID)  =
    let relID = 
        match relID with
        |   RelativeArtefactID.ID s -> s
    let alphDir = Path.GetDirectoryName(alphFileFullPath)
    let absPath = Path.GetFullPath(Path.Combine(alphDir,relID))
    let fullID:ArtefactFullID = ArtefactFullID.ID(Path.GetRelativePath(rootPath, absPath))
    fullID

type VersionedArtefact = {
    ID: RelativeArtefactID
    Hash: HashString
}

type ComputeSection =  {
    Inputs: VersionedArtefact array
    Outputs: VersionedArtefact array
    /// Relative to the alph file location
    WorkingDirectory: string
    Command: string    
    Signature: HashString
    OutputsCleanDisabled: bool
}

type ArtefactType =
    |   FileArtefact
    |   DirectoryArtefact

type SnapshotSection = {
    Type: ArtefactType
    Version : HashString
}

type DataOrigin =
    |   Computed of ComputeSection
    |   Snapshot of SnapshotSection    

type AlphFile = {
    Origin: DataOrigin
    /// Has ever been saved to some storage
    IsTracked: bool
}

let saveAsync (alphfile:AlphFile) (filepath:string) =
    async {
        let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented)
        use sw = new StreamWriter(filepath)
        do! Async.AwaitTask(sw.WriteAsync(serialized))
    }

let tryLoadAsync (filepath:string) =
    async {
        if File.Exists(filepath) then
            use sr = new StreamReader(filepath)
            let! read = Async.AwaitTask(sr.ReadToEndAsync())
            let alphFile = JsonConvert.DeserializeObject<AlphFile>(read)
            return Some(alphFile)
        else
            return None
    }
   
   
/// Full path transformed to full path. Relative to relative.
let artefactPathToAlphFilePath (artefactPath:string) =
    let prefix = 
        if artefactPath.EndsWith(Path.DirectorySeparatorChar) then
            artefactPath.Substring(0,artefactPath.Length-1)
        else
            artefactPath
    sprintf "%s.alph" prefix   

/// Full path transformed to full path. Relative to relative.
let alphFilePathToArtefactPathAsync (alphFilePath:string) =
    async {
        if not (alphFilePath.EndsWith(".alph")) then raise(ArgumentException(sprintf "%s path does not end with \".alph\"" alphFilePath))
        // we need to know whether the corresponding artefact is directory (e.g. its ID ends with '\') or single file
        // to figure this out we read the alph file content. First output by convention corresponds to the associated artefact (NOTE: each artefact has correspondent alph file)
        let! loaded  = tryLoadAsync alphFilePath
        match loaded with
        |   None -> return failwith "alphFilePathToArtefactPath can be executed only for existent alph files"
        |   Some(alphFile) ->
            let isDir =
                match alphFile.Origin with
                |   Computed(comp) ->
                    if comp.Outputs.Length>0 then
                        isRelativeIDDirectory comp.Outputs.[0].ID
                    else
                        failwith "alphFile does not specify any output artefacts. How it was created at all!?"
                |   Snapshot(snap) ->
                    match snap.Type with
                    |   DirectoryArtefact -> true
                    |   FileArtefact -> false
            let suffix = if isDir then Path.DirectorySeparatorChar.ToString() else ""
            return (alphFilePath.Substring(0,alphFilePath.Length-5) + suffix)
    }


/// Computes signature of supplied computeSection (Signature and isTracked member inside this computeSection is ignored during hash calculation)
let getSignature (computeSection:ComputeSection) =
    use sha = System.Security.Cryptography.SHA1.Create()
    let addHash (str:string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(str)
        sha.TransformBlock(bytes,0,bytes.Length,bytes,0) |> ignore
        ()
    addHash computeSection.Command 
    addHash computeSection.WorkingDirectory
    let hashArtefact art =
        addHash (relIDtoString art.ID)
        addHash art.Hash
    Seq.iter hashArtefact (Seq.append computeSection.Inputs  computeSection.Outputs)
    sha.TransformFinalBlock(Array.zeroCreate<byte> 0,0,0) |> ignore
    hashToString sha.Hash

let checkSignature (computeSection:ComputeSection) =
    let readSignature = computeSection.Signature
    let expectedSignature = getSignature computeSection
    if readSignature = expectedSignature then
        computeSection
    else
        // wiping out result hashes
        {
            computeSection with
                Outputs = Array.map (fun x -> {x with Hash=""}) computeSection.Outputs
        }