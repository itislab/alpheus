module ItisLab.Alpheus.Config

open System.IO
open System
open Newtonsoft.Json

type AzureDefinition = {
    AccountName: string
    AccountKey: string
    ContainerName: string
}

type Storage =
    /// if the path is relative, it is relative to research root
    |   Directory of path:string
    |   Azure of AzureDefinition

/// This object persists to disk
type ConfigFile = {
    FileFormatVersion: int
    Storage : Map<string,Storage>
    // the name of the storage that is used if explicit storage name is not specified during the storage related commands
    DefaultStorage: string
}

type Config = {
    /// A full path to the root of the experiment
    RootPath : string
    ConfigFile : ConfigFile
}


let defaultConfigFile = {
    FileFormatVersion = Versioning.ExperimentConfigFileCurrentVersion
    Storage = 
        [
            ("local",Directory(".alpheus/storage"))
        ] |> Map.ofList
    DefaultStorage = "local"
}


let serviceDir = ".alpheus"
let configFileName = "config.json"

let tryLoadConfigFileAsync filepath = 
    async {
        if File.Exists(filepath) then
            use sr = new StreamReader(filepath)
            let! read = Async.AwaitTask(sr.ReadToEndAsync())
            let version = Versioning.getVersion read
            match version with
            |   Versioning.ExperimentConfigFileCurrentVersion ->
                let configFile = JsonConvert.DeserializeObject<ConfigFile>(read)
                return Some(configFile)
            |   v ->
                // TODO: this is the place to engage version upgrade conversions
                return failwithf "Unsupported .config file version %d: %s" v filepath
        else
            return None
    }

let saveConfigFileAsync configFile (filepath:string) =
    async {
        let serialized = JsonConvert.SerializeObject(configFile,Formatting.Indented)
        use sw = new StreamWriter(filepath)
        do! Async.AwaitTask(sw.WriteAsync(serialized))
    }

let saveConfigAsync config =
    async {
        let rootPath = config.RootPath
        let configFilePath = Path.Combine(rootPath, serviceDir,configFileName)
        do! saveConfigFileAsync config.ConfigFile configFilePath
        
    }

let isExperimentDirectory rootPath = 
    let serviceFullPath = Path.Combine(rootPath,serviceDir)
    Directory.Exists(serviceFullPath)

let openExperimentDirectoryAsync rootPath =
    async {
        if not (isExperimentDirectory rootPath) then
            raise(InvalidOperationException("The directory is not initialized"))
        let condigFullFilePath = Path.Combine(rootPath,serviceDir,configFileName)
        let! configLoadResults = tryLoadConfigFileAsync condigFullFilePath
        match configLoadResults with
        |   None ->
            raise(InvalidDataException("The config file is absent"))
            return {
                RootPath = "dummy"
                ConfigFile = defaultConfigFile
                }
        |   Some(configFile) ->
            let results =
                {
                    RootPath = rootPath
                    ConfigFile = configFile
                }
            return results
    }
    
let createExperimentDirectoryAsync rootPath =
    async {
        let serviceFullPath = Path.Combine(rootPath,serviceDir)
        if Directory.Exists(serviceFullPath) then
            raise(InvalidOperationException("The directory is already initialized"))
        else
            Directory.CreateDirectory(serviceFullPath) |> ignore
            let defaultStorageFullPath = Path.Combine(serviceFullPath,"storage")
            Directory.CreateDirectory(defaultStorageFullPath) |> ignore
            let config = {
                RootPath = rootPath
                ConfigFile = defaultConfigFile
            }
            do! saveConfigAsync config
        return! openExperimentDirectoryAsync rootPath
    }

/// Tries to find the root of the Alpheus experiment in the supplied path and up in the file hierarchy
/// Returns the full path to the root
let tryLocateExperimentRoot path =
    //path can be either file, folder
    //it can exist or not exist
    let fullPath = Path.GetFullPath(path)
    let splited = fullPath.Split([| Path.DirectorySeparatorChar|])
    
    let l1 = List.ofArray splited
    let isUnixRooted =
        match l1 with
        | ""::_ -> true // absolute rooted unix path (e.g. /home/user1/) after split contains empty first element
        | _ -> false
    let l2 = List.rev l1 // drive (or unix root /) is the deepest element now (at the tail)
    // printfn "candidate list is %A" l2

    let stringOfReversedPathElements l =
        let res = l |> List.rev |> List.toArray |> Path.Combine        
        // Path.Combine ignores empty elements, thus removes leading / in case of unix.
        // Guarding against it
        if isUnixRooted then "/"+res else res
            
    let appendDirectorySeparator path = 
        path + string Path.DirectorySeparatorChar // indicates that the path is a directory

    let rec locateList candidate_l =
        match candidate_l with
        |   [] -> None
        |   _::tail ->
            
            let toCheck = (serviceDir :: candidate_l) |> stringOfReversedPathElements
            // printfn "Checking existence of %s" toCheck

            if Directory.Exists(toCheck) then
                Some(candidate_l |> stringOfReversedPathElements |> appendDirectorySeparator)
            else
                locateList tail
    locateList l2
