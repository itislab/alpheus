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
    |   [<First>]   SetDefault of name:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   AddLocal _ -> "Register local directoy artefact storage (or replaces existing one with the same name)"
            |   AddAzure _ -> "Register Azure Blob artefact storage (or replaces existing one with the same name)"
            |   SetDefault _ -> "Set particular storage to be used for saving artefacts by default"
            |   Remove _ -> "Remove specified artefact storage"
            |   List -> "Prints configured storages"

[<CliPrefix(CliPrefix.None)>]
type ConfigArgs =
    |   Storage of ParseResults<StorageArgs>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Storage _ -> "Artefact storage configuration"

[<CliPrefix(CliPrefix.Dash)>]
type BuildArgs = 
    |   D of dependency:string
    |   O of output:string
    |   [<CliPrefix(CliPrefix.DoubleDash)>]Disable_Outputs_Clean
    |   [<CliPrefix(CliPrefix.DoubleDash)>][<AltCommandLine("-ec")>]Successful_Exit_Codes of codes:int list
    | [<Last;CliPrefix(CliPrefix.None)>]Command
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   D _ -> "Dependency path (file or folder)"
            |   O _ -> "Output path (file or folder)"
            |   Disable_Outputs_Clean -> "Make the command responsible for clearing the outputs in case of re-computation. Useful for resumable computations. Default: The outputs are cleaned by alpheus"
            |   Successful_Exit_Codes _ -> "Exit codes (space separated) of the command that are considered as successful computation. Default: 0"
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
    |   Storage of ``storage name``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Path _ -> "File/Directory to save in storage(s)"
            |   AllUnsaved -> "Save all of the artefacts with status unsaved"
            |   Storage _ -> "Where to send a artefact copy. If not specified, the artefacts are sent to the default storage"

type RestoreArgs =
    |   [<Mandatory; MainCommand>]Path of ``File or Dir``:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            |   Path _ -> "File/Directory to restore"            

type VerbosityLevel =
    | Quiet = 0
    | Err  = 1
    | Warn = 2
    | Info  = 3
    | Verbose = 4

[<CliPrefix(CliPrefix.None)>]
type AlpheusArgs = 
    | Init of ParseResults<InitArgs>
    | Config of ParseResults<ConfigArgs>
    | Build of ParseResults<BuildArgs>    
    | Compute of ParseResults<ComputeArgs>
    | Status of ParseResults<StatusArgs>
    | Save of ParseResults<SaveArgs>
    | Restore of ParseResults<RestoreArgs>
    | [<AltCommandLine("-v")>][<CliPrefix(CliPrefix.DoubleDash)>] Verbosity of VerbosityLevel
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
            |   Verbosity _ -> "Level of verbosity for produced text messages"
