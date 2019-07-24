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
        let usage = parser.PrintUsage(programName = programName)
        let parseResults = parser.ParseCommandLine(argv,false,true,true)
        if parseResults.Contains Init then
            let cwd = Directory.GetCurrentDirectory()
            if Config.isExperimentDirectory cwd then
                printfn "The current directory is already Alpheus experiment directory"
                1
            else
                let initComputaiton = API.createExperimentDirectoryAsync cwd
                Async.RunSynchronously initComputaiton |> ignore
                0
        elif parseResults.Contains Config then
            let configArgs = parseResults.GetResult <@ Config @>
            let cwd = Directory.GetCurrentDirectory()
            match Config.tryLocateExperimentRoot cwd with
                |   None ->
                    printfn "The file you've specified is not under Alpheus experiment folder"
                    1
                |   Some(experimentRoot) ->
                    let configComputation = 
                        async {                                
                                if configArgs.Contains CLI.Storage then
                                    let storageArgs = configArgs.GetResult <@ CLI.Storage @>
                                    if storageArgs.Contains AddLocal then
                                        let name,dirPath = storageArgs.GetResult <@ AddLocal @>                                        
                                        do! API.configAddDirectoryStorageAsync experimentRoot name dirPath
                                        return 0
                                    elif storageArgs.Contains AddAzure then
                                        let name,accName,accKey,container = storageArgs.GetResult <@ AddAzure @>                                        
                                        do! API.configAddAzureStorageAsync experimentRoot name accName accKey container
                                        return 0
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
                                        return 0
                                    elif storageArgs.Contains Remove then
                                        let nameToRemove = storageArgs.GetResult <@ Remove @>
                                        do! API.configRemoveStorageAsync experimentRoot nameToRemove
                                        return 0
                                    else
                                        printfn "Please specify what to do with storage configuration"
                                        return 1
                                else
                                    printfn "Please specify what to configure"
                                    return 1
                        }
                    configComputation |> Async.RunSynchronously
        elif parseResults.Contains Compute then
            let computeArgs = parseResults.GetResult <@ Compute @>
            let filePath = computeArgs.GetResult <@ ComputeArgs.File @>
            API.compute (API.artefactFor filePath)
        elif parseResults.Contains Status then
            let statusArgs = parseResults.GetResult <@ Status @>
            let artefactPath = statusArgs.GetResult <@ StatusArgs.File  @>
            API.status (API.artefactFor artefactPath)
        elif parseResults.Contains Restore then
            let restoreArgs = parseResults.GetResult <@ Restore @>
            let artefactPath = restoreArgs.GetResult <@ RestoreArgs.Path @>
            let restoreComputation = API.restoreAsync (API.artefactFor artefactPath)
            restoreComputation |> Async.RunSynchronously

        elif parseResults.Contains Save then
            let saveArgs = parseResults.GetResult <@ Save @>
            let saveAll = saveArgs.Contains AllUnsaved 
            let storageName = saveArgs.GetResult <@ SaveArgs.Storage @>
            
            let inputpath = saveArgs.GetResult <@ SaveArgs.Path @>
            API.saveAsync (API.artefactFor inputpath) storageName saveAll |> Async.RunSynchronously
        elif parseResults.Contains Build then                
            BuildCommand.run (parseResults.GetResult <@ Build @>)
        else
            printfn "%s" usage
            2
    with e ->
        printfn "%s" (e.ToString())
        0
