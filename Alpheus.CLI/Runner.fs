module ItisLab.Alpheus.CliRunner

open Argu
open ItisLab.Alpheus.CLI
open System.IO
open System.Text

let run (programName:string) (parseResults:ParseResults<AlpheusArgs>) : Result<unit, string> =
    let usage = parseResults.Parser.PrintUsage(programName = programName)
    if parseResults.IsUsageRequested then
        printfn "%s" usage
        Ok()
    else if parseResults.Contains Init then
        result {
            let! cwd = (Directory.GetCurrentDirectory(), (fun p -> not (Config.isExperimentDirectory p)), "The current directory is already an Alpheus experiment directory")
            return API.createExperimentDirectoryAsync cwd |> Async.RunSynchronously |> ignore
        }
    elif parseResults.Contains Config then
        result {
            let configArgs = parseResults.GetResult <@ Config @>
            let cwd = Directory.GetCurrentDirectory()
            let! experimentRoot = (Config.tryLocateExperimentRoot cwd, "The file you've specified is not under an Alpheus experiment folder")
            let! storageArgs = (configArgs.TryGetResult <@ CLI.Storage @>, "Please specify what to configure")
            return! async {                                
                if storageArgs.Contains AddLocal then
                    let name,dirPath = storageArgs.GetResult <@ AddLocal @>                                        
                    do! API.configAddDirectoryStorageAsync experimentRoot name dirPath
                    return Ok()
                elif storageArgs.Contains AddAzure then
                    let name,accName,accKey,container = storageArgs.GetResult <@ AddAzure @>                                        
                    do! API.configAddAzureStorageAsync experimentRoot name accName accKey container
                    return Ok()
                elif storageArgs.Contains List then
                    let sb = StringBuilder()
                    let printer name storage =
                        let storageStr = 
                            match storage with
                            |   Config.Directory(path) -> sprintf "Local directory \"%s\"" path
                            |   Config.Azure(def) -> sprintf "Azure BLOB accountName:%s container:%s" def.AccountName def.ContainerName
                        sb.AppendLine(sprintf "%20s\t\t%s" name storageStr) |> ignore
                    let! storages = API.configListStoragesAsync experimentRoot
                    Map.iter printer storages
                    printfn "You have following storages configured:\n%s" (sb.ToString())
                    return Ok()
                elif storageArgs.Contains Remove then
                    let nameToRemove = storageArgs.GetResult <@ Remove @>
                    do! API.configRemoveStorageAsync experimentRoot nameToRemove
                    return Ok()
                else
                    return Error "Please specify what to do with storage configuration"
            }|> Async.RunSynchronously
        }
    elif parseResults.Contains Compute then
        result {
            let computeArgs = parseResults.GetResult <@ Compute @>
            let filePath = computeArgs.GetResult <@ ComputeArgs.File @>
            let! artefact = API.artefactFor filePath
            return! API.compute artefact
        }
    elif parseResults.Contains Status then
        result {
            let statusArgs = parseResults.GetResult <@ Status @>
            let artefactPath = statusArgs.GetResult <@ StatusArgs.File  @>
            let! artefact = API.artefactFor artefactPath
            return! Error "NOT IMPLEMENTED"
            //return! API.status artefact
        }
    elif parseResults.Contains Restore then
        result {
            let restoreArgs = parseResults.GetResult <@ Restore @>
            let artefactPath = restoreArgs.GetResult <@ RestoreArgs.Path @>
            let! artefact = API.artefactFor artefactPath
            return! Error "NOT IMPLEMENTED"
            //return! API.restoreAsync artefact |> Async.RunSynchronously
        }
    elif parseResults.Contains Save then
        result {
            let saveArgs = parseResults.GetResult <@ Save @>
            let saveAll = saveArgs.Contains AllUnsaved 
            let storageName = saveArgs.GetResult <@ SaveArgs.Storage @>
            let inputpath = saveArgs.GetResult <@ SaveArgs.Path @>
            let! artefact = API.artefactFor inputpath
            return! API.saveAsync artefact storageName saveAll |> Async.RunSynchronously
        }
    elif parseResults.Contains Build then                
        BuildCommand.run (parseResults.GetResult <@ Build @>)
    else
        Error usage

