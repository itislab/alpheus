module ItisLab.Alpheus.Tests.Config

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus.Config

type ConfigSerialization(output)=
    inherit SingleUseOneTimeDirectory(output)

    [<Fact>]
    member s.``Round serialization``() =
        async {
            // saving general config file
            // most fully filled (to increase code coverage)
            
            // the loading

            let relativeLocalStorage:Storage = Directory "./directoryStorage1"
            let absLocalStorage:Storage = Directory (Path.Combine(testRuntimeRootPath,"dir1"))
            let azureStorage = Azure {AccountName="abc"; AccountKey="cba"; ContainerName="container1"}
            let configFile: ConfigFile = 
                {
                    FileFormatVersion = ItisLab.Alpheus.Versioning.ExperimentConfigFileCurrentVersion
                    Storage =
                        [
                            "relLocal",relativeLocalStorage
                            "absLocal",absLocalStorage
                            "azure1",azureStorage
                            ] |> Map.ofList
                }

            let path = Path.Combine(s.Path,"config.json")

            // saving
            do! saveConfigFileAsync configFile path

            // and now loading
            let! loadResult = tryLoadConfigFileAsync path
            match loadResult with
            |   None -> Assert.True(false,"Failed to load previously saved config file")
            |   Some loadedConfig ->
                Assert.Equal(loadedConfig, configFile)

        }   |> toAsyncFact

    [<Fact>]
    member s.``isExperimentDirectory handles initialized directory`` () =
        async {
            let! dummyConfig = createExperimentDirectoryAsync(s.Path)

            let isRoot = isExperimentDirectory s.Path
            Assert.True(isRoot)
        } |> toAsyncFact

    [<Fact>]
    member s.``openExperimentDirectoryAsync loads just initialized directory`` () =
        async {
            let! dummyConfig = createExperimentDirectoryAsync(s.Path)

            let! loadedConfig = openExperimentDirectoryAsync(s.Path)

            Assert.Equal(dummyConfig,loadedConfig)

        } |> toAsyncFact
        
    [<Fact>]
    member s.``isExperimentDirectory handles uninitialized directory`` () =
        async {
            let isRoot = isExperimentDirectory s.Path
            Assert.False(isRoot)
        } |> toAsyncFact

    [<Fact>]
    member s.``tryLocateExpereimentRoot finds nothing on unintialized dir``() = 
        match tryLocateExperimentRoot s.Path with
        |   None -> Assert.True(true)
        |   Some(found) -> Assert.True(false, sprintf "tryLocateExpereimentRoot found the root where there is no root: %s" found)

    [<Fact>]
    member s.``tryLocateExpereimentRoot finds the root upward on the tree``() = 
        async {
            let dir1 = Path.Combine(s.Path,"dir1")
            let dir2 = Path.Combine(s.Path,"dir1","dir2")
            let dir3 = Path.Combine(s.Path,"dir1","dir2","dir3")
            Directory.CreateDirectory(dir1) |> ignore
            Directory.CreateDirectory(dir2) |> ignore
            Directory.CreateDirectory(dir3) |> ignore

            let! _ = createExperimentDirectoryAsync(dir1)

            match tryLocateExperimentRoot dir3 with
            |   None -> Assert.True(false,"Failed to find the experiment root")
            |   Some(found) -> Assert.Equal(Path.GetFullPath(dir1) + string Path.DirectorySeparatorChar,found)
        }   |> toAsyncFact