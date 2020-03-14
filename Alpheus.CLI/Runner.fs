module ItisLab.Alpheus.CliRunner

open Argu
open ItisLab.Alpheus.CLI
open System.IO
open System.Text

let run (programName:string) (workingDir:string) (parseResults:ParseResults<AlpheusArgs>) : Result<unit, AlpheusError> =
    let usage = parseResults.Parser.PrintUsage(programName = programName)
    if parseResults.IsUsageRequested then
        printfn "%s" usage
        Ok()
    else if parseResults.Contains Init then
        result {            
            let! cwd = (workingDir, (fun p -> not (Config.isExperimentDirectory p)), (UserError "The current directory is already an Alpheus experiment directory"))
            return API.createExperimentDirectory cwd |> Async.RunSynchronously |> ignore
        }
    elif parseResults.Contains Config then
        result {
            let configArgs = parseResults.GetResult <@ Config @>
            let! experimentRoot = (Config.tryLocateExperimentRoot workingDir, UserError "The file you've specified is not under an Alpheus experiment folder")
            let! storageArgs = (configArgs.TryGetResult <@ CLI.Storage @>, UserError  "Please specify what to configure")
            return! async {                                
                if storageArgs.Contains AddLocal then
                    let name,dirPath = storageArgs.GetResult <@ AddLocal @>                                        
                    do! API.configAddDirectoryStorageAsync experimentRoot name dirPath
                    return Ok()
                elif storageArgs.Contains AddAzure then
                    let name,accName,accKey,container = storageArgs.GetResult <@ AddAzure @>                                        
                    do! API.configAddAzureStorageAsync experimentRoot name accName accKey container
                    return Ok()
                elif storageArgs.Contains SetDefault then
                    let newDefaultStorage = storageArgs.GetResult <@ SetDefault @>
                    return! API.configStorageSetDefault experimentRoot newDefaultStorage
                elif storageArgs.Contains List then
                    let sb = StringBuilder()
                    let! storages,defaultStorageName = API.configListStoragesAsync experimentRoot
                    let printer name storage =
                        let storageStr = 
                            match storage with
                            |   Config.Directory(path) -> sprintf "Local directory \"%s\"" path
                            |   Config.Azure(def) -> sprintf "Azure BLOB accountName:%s container:%s" def.AccountName def.ContainerName
                        let nameExt =
                            if name = defaultStorageName then
                                sprintf "%s [default]" name
                            else
                                name
                        sb.AppendLine(sprintf "%20s\t\t%s" nameExt storageStr) |> ignore
                    
                    Map.iter printer storages
                    printfn "You have following storages configured:\n%s" (sb.ToString())
                    return Ok()
                elif storageArgs.Contains Remove then
                    let nameToRemove = storageArgs.GetResult <@ Remove @>
                    do! API.configRemoveStorageAsync experimentRoot nameToRemove
                    return Ok()
                else
                    return Error (UserError "Please specify what to do with storage configuration")
            }|> Async.RunSynchronously
        }
    elif parseResults.Contains Compute then
        result {
            let computeArgs = parseResults.GetResult <@ Compute @>
            let filePath = computeArgs.GetResult <@ ComputeArgs.File @>
            let! artefact = API.artefactFor workingDir filePath
            return! API.compute artefact
        }
    elif parseResults.Contains Status then
        result {
            let statusArgs = parseResults.GetResult <@ Status @>
            let artefactPath = statusArgs.GetResult <@ StatusArgs.File  @>
            let! artefact = API.artefactFor workingDir artefactPath
            let statusesRes = API.status artefact
            match statusesRes with
            |   Ok(statuses)->
                let printStatus artId (status:Angara.Data.MdMap<string,StatusGraph.ArtefactStatus>) =
                    let statusToString status =
                        match status with
                        |   StatusGraph.UpToDate location ->
                            match location with
                            |   DependencyGraph.Local -> "Up to date"
                            |   DependencyGraph.Remote -> "Up to date (in storage)"
                        |   StatusGraph.NeedsRecomputation _ ->
                            "Need recomputation"
                    if status.IsScalar then
                        printfn "%-50s: %s" (artId.ToString()) (status.AsScalar() |> statusToString)
                    else
                        printfn "%-50s:" (artId.ToString())
                        let printItem pair =
                            let (index:string list),v = pair
                            printfn "\t%-50s: %s" (String.concat " " index) (statusToString v)
                        status |> Angara.Data.MdMap.toSeq |> Seq.iter printItem
                statuses |> Map.iter printStatus 
                return ()
            |   Error e -> return! Error e
        }
    elif parseResults.Contains Restore then
        result {
            let restoreArgs = parseResults.GetResult <@ Restore @>
            let artefactPath = restoreArgs.GetResult <@ RestoreArgs.Path @>
            let! artefact = API.artefactFor workingDir artefactPath
            return! API.restoreAsync artefact |> Async.RunSynchronously
        }
    elif parseResults.Contains Save then
        result {
            let saveArgs = parseResults.GetResult <@ Save @>
            let saveAll = saveArgs.Contains AllUnsaved 
            let specifiedStorageName = saveArgs.TryGetResult <@ SaveArgs.Storage @>
            let inputpath = saveArgs.GetResult <@ SaveArgs.Path @>
            let! artefact = API.artefactFor workingDir inputpath
            return! API.saveAsync artefact specifiedStorageName saveAll |> Async.RunSynchronously
        }
    elif parseResults.Contains Build then                
        BuildCommand.run workingDir (parseResults.GetResult <@ Build @>)
    elif parseResults.Contains Sign then
        result {
            let signArgs = parseResults.GetResult <@ Sign @>
            let path = signArgs.GetResult <@ SignArgs.Path @>
            let! artefact = API.artefactFor workingDir path
            return! API.signAlphFileAsync artefact |> Async.RunSynchronously
        }
    elif parseResults.Contains Hash then
        result {
            let hashArgs = parseResults.GetResult <@ Hash @>
            let path = hashArgs.GetResult <@ HashArgs.Path @>
            let! artefact = API.artefactFor workingDir path
            let versions = API.hashArtefactAsync artefact |> Async.RunSynchronously
            printfn "Actual disk version of the artefact %O" (snd artefact)
            versions |> List.iter (fun arg -> let idx,ver = arg in printfn "%15A:\t%A" idx ver.Value)
            return! Ok()
        }
    else
        Error (UserError usage)

