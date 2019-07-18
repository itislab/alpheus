module ItisLab.Alpheus.Tests.ExecuteCommandTests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus
open ItisLab.Alpheus.ExecuteCommand

[<Fact>]
let ``Running utility from PATH``() =
    async {
        let! exitCode = ExecuteCommand.runCmdLocallyAsync (fun x -> printfn "%s" x) "path utility test" "dotnet" "--list-runtimes" Environment.CurrentDirectory
        Assert.Equal(0,exitCode)
        } |> toAsyncFact

type FileCopingTests()=
    inherit SingleUseOneTimeDirectory()

    [<Fact>]
    member s.``Running local executable``() =
        async {
            let localExecutable =
                if isTestRuntimeWindows then
                    "copy.cmd"
                else
                    // to be runnable on Unix we need to set executable flag on the executable
                    Assert.Equal(0,execUnixShellcommand "chmod a+x data/copy_prog")
                    "copy_prog"
            let wd = Path.GetFullPath(Path.Combine(s.Path,"../../")) // this is "data" dir.
            // Test checks that the executable is searched here,
            // and that executable's working directory is set properly
            // Note that this wd is different from test runtime working directory
            let newLocation = Path.GetFullPath(Path.Combine(s.Path,"texturalData.txt"))
            let args = sprintf "texturalData.txt %s" newLocation

            let tracefn s =
                System.Diagnostics.Trace.WriteLine(s)

            sprintf "command: %s" localExecutable |> tracefn
            sprintf "args: %s" args |> tracefn
            sprintf "wd: %s" wd |> tracefn

            let! exitCode = ExecuteCommand.runCmdLocallyAsync (fun x -> System.Diagnostics.Trace.WriteLine(sprintf "%s" x)) "local executable test" localExecutable args wd
            Assert.Equal(0,exitCode)
        }
