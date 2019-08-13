module ItisLab.Alpheus.AlphFiles

open System
open System.IO
open Angara.Data
open Newtonsoft.Json
open CustomSerializers

// Example 1:
//  artefact id: files/*.txt
//  alph file: file/vector.txt.alph
//  SourceOrigin.RelativePath: *.txt
// Example 2:
//  artefact id: files/*/*.txt
//  alph file: file/vector-vector.txt.alph
//  SourceOrigin.RelativePath: */*.txt

/// Identifies a version of an artefact (either scalar or vector) using a hash string (can be neither null nor empty).
type ArtefactVersion = MdMap<string, HashString option>

type VersionedArtefact = {
    RelativePath: AlphRelativePath
    /// In case of a vector, intermediate keys contain a concrete replacement string for an asterisk; at a leaf the key contains a full path to the file/directory.
    Hash: ArtefactVersion
}

type CommandOutput =  {
    Inputs: VersionedArtefact array
    Outputs: VersionedArtefact array
    OutputIndex: int
    WorkingDirectory: AlphRelativePath
    Command: string    
    /// Hashed content of this instance. Allows to determine if it was edited.
    Signature: HashString
    OutputsCleanDisabled: bool
}

type DataOrigin =
    | CommandOrigin of CommandOutput
    | SourceOrigin of VersionedArtefact  
    

type AlphFile = {
    Origin: DataOrigin
    /// Has ever been saved to some storage
    IsTracked: bool
}

let saveAsync (alphfile:AlphFile) (filepath:string) =
    async {
        let converter = ArtefactVersionConverter()
        let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented, converter)
        use sw = new StreamWriter(filepath)
        do! Async.AwaitTask(sw.WriteAsync(serialized))
    }

let save (alphfile:AlphFile) (filepath:string) =
    let converter = ArtefactVersionConverter()
    let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented, converter)
    use sw = new StreamWriter(filepath)
    sw.Write(serialized)


let tryLoad (filepath:string) =
    if File.Exists(filepath) then
        use sr = new StreamReader(filepath)
        let read = sr.ReadToEnd()
        let converter = ArtefactVersionConverter()
        let alphFile = JsonConvert.DeserializeObject<AlphFile>(read, converter)
        Some(alphFile)
    else
        None

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

