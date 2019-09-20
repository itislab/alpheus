module ItisLab.Alpheus.AlphFiles

open System
open System.IO
open Angara.Data
open Newtonsoft.Json
open CustomSerializers

// Example 1:
//  artefact id: files/*.txt
//  alph file: files/vector.txt.alph
//  SourceOrigin.RelativePath: *.txt
// Example 2:
//  artefact id: files/*/*.txt
//  alph file: files/vector-vector.txt.alph
//  SourceOrigin.RelativePath: */*.txt

/// Identifies a version of an artefact (either scalar or vector) using a hash string (can be neither null nor empty).
/// map: path -> version
type ArtefactVersion = MdMap<string, HashString option>

type VersionedArtefact = {
    RelativePath: AlphRelativePath
    /// In case of a vector, intermediate keys contain a concrete replacement string for an asterisk; at a leaf the key contains a full path to the file/directory.
    Hash: ArtefactVersion
}

type CommandOutput =  {
    Inputs: VersionedArtefact list
    Outputs: VersionedArtefact list
    SuccessfulExitCodes: int list
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
    FileFormatVersion: int
    Origin: DataOrigin
    /// Has ever been saved to some storage
    IsTracked: bool
}

/// Save the content of alphfile into to file filepath, recreating the file if it exists
let save (alphfile:AlphFile) (filepath:string) =
    let converter = ArtefactVersionConverter()
    let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented, converter)
    use sw = new StreamWriter(filepath)
    sw.Write(serialized)

/// Save the content of alphfile into to file filepath, recreating the file if it exists
let saveAsync (alphfile:AlphFile) (filepath:string) =
    async {
        let converter = ArtefactVersionConverter()
        let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented, converter)
        use sw = new StreamWriter(filepath)
        do! Async.AwaitTask(sw.WriteAsync(serialized))
    }

let tryLoadAsync (filepath:string) =
    async {
        if File.Exists(filepath) then
            use sr = new StreamReader(filepath)
            let! read = Async.AwaitTask(sr.ReadToEndAsync())
            let readVersion = Versioning.getVersion read

            match readVersion with
            |   Versioning.AlphFileCurrentVersion ->            
                let converter = ArtefactVersionConverter()
                let alphFile = JsonConvert.DeserializeObject<AlphFile>(read, converter)
                return Some(alphFile)
            |   v ->
                // TODO: this is the place to engage version upgrade conversions
                return failwithf "Unsupported alph file version %d: %s" v filepath
        else
            return None
    }

let tryLoad (filepath:string) =
    tryLoadAsync filepath |> Async.RunSynchronously
    
    

