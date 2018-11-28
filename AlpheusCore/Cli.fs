module ItisLab.Alpheus.CLI

open Argu.ArguAttributes
open Argu

type InitArgs =
    |   [<GatherUnrecognized; Hidden>]Dummy of string    
with
    interface IArgParserTemplate with
        member s.Usage = "Initialize current directory as alpheus experiment root"

[<CliPrefix(CliPrefix.None)>]
type StorageArgs =
    |   [<First>]   AddLocal of name:string * Directory:string
    |   [<First>]   AddAzure of name:string * AccountName:string * AccountKey:string * ContainerName:string
    |   [<First>]   Remove of name:string
    |   [<First>]   List
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   AddLocal _ -> "Register local directoy artefact storage (or replaces existing one with the same name)"
            |   AddAzure _ -> "Register Azure Blob artefact storage (or replaces existing one with the same name)"
            |   Remove _ -> "Remove specified artefact storage"
            |   List -> "Prints configured storages"

[<CliPrefix(CliPrefix.None)>]
type ConfigArgs =
    |   [<CliPrefix(CliPrefix.DoubleDash)>]Local
    |   Storage of ParseResults<StorageArgs>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Local -> "Edit local configuration"
            |   Storage _ -> "Artefact storage configuration"

[<CliPrefix(CliPrefix.Dash)>]
type BuildArgs = 
    |   D of dependency:string
    |   O of output:string
    | [<Last;CliPrefix(CliPrefix.None)>]Command
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   D _ -> "Dependency path (file or folder)"
            |   O _ -> "Output path (file or folder)"
            |   Command _ -> "Command that generates the outputs"

[<CliPrefix(CliPrefix.None)>]
type ComputeArgs =
    |   [<Mandatory; MainCommand>]File of ``File or Dir``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   File _ -> "File to produce/(re)calculate"

[<CliPrefix(CliPrefix.None)>]
type StatusArgs = 
    |   [<Mandatory; MainCommand>]File of ``File or Dir``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   File _ -> "File to build a dependency graph for"
            
type SaveArgs =
    |   AllUnsaved
    |   [<Mandatory; MainCommand>]Path of ``File or Dir``:string
    |   [<Mandatory>] Storage of ``storage name``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Path _ -> "File/Directory to save in storage(s)"
            |   AllUnsaved -> "Save all of the artefacts with status unsaved"
            |   Storage _ -> "Where to send a artefact copy. If not specified, the artefacts are sent to all registered storages"

type RestoreArgs =
    |   [<Mandatory; MainCommand>]Path of ``File or Dir``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Path _ -> "File/Directory to restore from"            


[<CliPrefix(CliPrefix.None)>]
type AlpheusArgs = 
    | [<First>] Init of ParseResults<InitArgs>
    | [<First>] Config of ParseResults<ConfigArgs>
    | [<First>] Build of ParseResults<BuildArgs>    
    | [<First>] Compute of ParseResults<ComputeArgs>
    | [<First>] Status of ParseResults<StatusArgs>
    | [<First>] Save of ParseResults<SaveArgs>
    | [<First>] Restore of ParseResults<RestoreArgs>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Init _ -> "Make the current directory an Alpheus experiment directory"
            |   Config _ -> "Modify configuration of research directory"
            |   Build _ -> "Creates an experiment graph node"
            |   Compute _ -> "Tries to compute the graph to make the outdated node up to date"
            |   Status _ -> "Prints the graph status for particular .alph file"
            |   Save _ -> "Save a copy of file/directory to the storage(s)"
            |   Restore _ -> "Restore a copy of the file/directory from storage"
