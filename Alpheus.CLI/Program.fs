module ItisLab.Alpheus.Program

open System
open Argu
open ItisLab.Alpheus.CLI
open ItisLab.Alpheus.Trace
open System.IO
open System.Text

[<EntryPoint>]
let main argv =

    let programName = "alpheus"
    let parser = ArgumentParser.Create<AlpheusArgs>(programName = programName)
    
    try 
        let cliResult = 
            let usage = parser.PrintUsage(programName = programName)
            let parseResults = parser.ParseCommandLine(argv,ignoreMissing=false,ignoreUnrecognized=true,raiseOnUsage=false)
            if parseResults.IsUsageRequested then
                printfn "%s" usage
                Ok()
            else if parseResults.Contains Init then
                let cwd = Directory.GetCurrentDirectory()
                if Config.isExperimentDirectory cwd then
                    Error("The current directory is already Alpheus experiment directory")
                else
                    let initComputaiton = API.createExperimentDirectoryAsync cwd
                    Async.RunSynchronously initComputaiton |> ignore
                    Ok()
            elif parseResults.Contains Config then
                let configArgs = parseResults.GetResult <@ Config @>
                let cwd = Directory.GetCurrentDirectory()
                match Config.tryLocateExperimentRoot cwd with
                    |   None ->
                        Error("The file you've specified is not under Alpheus experiment folder")
                    |   Some(experimentRoot) ->
                        let configComputation = 
                            async {                                
                                    if configArgs.Contains CLI.Storage then
                                        let storageArgs = configArgs.GetResult <@ CLI.Storage @>
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
                                            return Error("Please specify what to do with storage configuration")
                                    else
                                        return Error("Please specify what to configure")
                            }
                        configComputation |> Async.RunSynchronously
            elif parseResults.Contains Compute then
                let computeArgs = parseResults.GetResult <@ Compute @>
                let filePath = computeArgs.GetResult <@ ComputeArgs.File @>
                API.compute filePath
            elif parseResults.Contains Status then
                let statusArgs = parseResults.GetResult <@ Status @>
                let artefactPath = statusArgs.GetResult <@ StatusArgs.File  @>
                API.status artefactPath
            elif parseResults.Contains Restore then
                let restoreArgs = parseResults.GetResult <@ Restore @>
                let artefactPath = restoreArgs.GetResult <@ RestoreArgs.Path @>
                let restoreComputation = API.restoreAsync artefactPath
                restoreComputation |> Async.RunSynchronously
            elif parseResults.Contains Save then
                let saveArgs = parseResults.GetResult <@ Save @>
                let saveAll = saveArgs.Contains AllUnsaved 
                let storageName = saveArgs.GetResult <@ SaveArgs.Storage @>
            
                let inputpath = saveArgs.GetResult <@ SaveArgs.Path @>
                API.saveAsync inputpath storageName saveAll |> Async.RunSynchronously
            elif parseResults.Contains Build then                
                BuildCommand.run (parseResults.GetResult <@ Build @>)
            else
                Error usage
        match cliResult with
        |   Ok() -> 0
        |   Error(m) -> printfn "Error occurred: %s" m; 1
    with
    |   :? ArguException as e ->
        printfn "%s" e.Message // argu exception is parse exception. So don't print stack trace. We print only parse error content.
        2
    |   e ->
        printfn "%s" (e.ToString()) // In all other exception types we print the exception with stack trace.
        3
