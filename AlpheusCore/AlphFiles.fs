module ItisLab.Alpheus.AlphFiles

open Newtonsoft.Json
open System.IO
open Hash

type ArtefactFullID = string

type VersionedArtefact = {
    ID: ArtefactFullID
    Hash: HashString
}

type ComputeSection =  {
    Inputs: VersionedArtefact array
    Outputs: VersionedArtefact array
    WorkingDirectory: string
    Command: string    
    Signature: HashString
}


type DataOrigin =
    |   Computed of ComputeSection
    |   Snapshot of HashString    

type AlphFile = {
    Origin: DataOrigin
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
        addHash art.ID
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