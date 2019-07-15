module ItisLab.Alpheus.Tests.ExecuteCommandTests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus
open ItisLab.Alpheus.ExecuteCommand

[<Theory>]
[<InlineData("start", "start", "")>]
[<InlineData("  start\t ", "start", "")>]
[<InlineData("start  a b c d ", "start", "a b c d")>]
[<InlineData("\"start  a b c d ", "\"start  a b c d", "")>]
[<InlineData("\"start \" \"a\"", "\"start \"", "\"a\"")>]
[<InlineData("\" ", "\"", "")>]
[<InlineData(" \"c:\my scripts\script runner.exe\"  \"a b\" c 'd.exe' >> \"/c/data/output.txt/\" ", "\"c:\my scripts\script runner.exe\"", "\"a b\" c 'd.exe' >> \"/c/data/output.txt/\"")>]
let ``Splitting command line into a program and its arguments`` (commandLine:string) (expectedProgram:string) (expectedArgs:string) =
    let (program, args) = ExecuteCommand.splitCommandLine commandLine
    Assert.Equal(expectedProgram, program)
    Assert.Equal(expectedArgs, args)