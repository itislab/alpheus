module ItisLab.Alpheus.AlphFiles

open System
open System.IO
open Newtonsoft.Json

// Example 1:
//  artefact id: files/*.txt
//  alph file: file/vector.txt.alph
//  SourceOrigin.RelativePath: *.txt
// Example 2:
//  artefact id: files/*/*.txt
//  alph file: file/vector-vector.txt.alph
//  SourceOrigin.RelativePath: */*.txt

type VersionedArtefact = {
    RelativePath: AlphRelativePath
    Hash: HashString
}

type CommandOutput =  {
    Inputs: VersionedArtefact array
    Outputs: VersionedArtefact array
    OutputIndex: int
    WorkingDirectory: AlphRelativePath
    Command: string    
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
        let serialized = JsonConvert.SerializeObject(alphfile,Formatting.Indented)
        use sw = new StreamWriter(filepath)
        do! Async.AwaitTask(sw.WriteAsync(serialized))
    }

let tryLoad (filepath:string) =
    if File.Exists(filepath) then
        use sr = new StreamReader(filepath)
        let read = sr.ReadToEnd()
        let alphFile = JsonConvert.DeserializeObject<AlphFile>(read)
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

