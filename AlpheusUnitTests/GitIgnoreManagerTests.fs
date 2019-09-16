module ItisLab.Alpheus.Tests.GitIgnoreManager

open System
open Xunit
open System.IO
open ItisLab.Alpheus.Tests.Utils
open ItisLab.Alpheus
open System.Collections
open System.Collections.Generic

type TestCase =
    {
        InitialEntries: (string array) option // initial lines of .gitignore file. None means that the .gitignorefile is absent
        toBeAddedEntries: string list // entries to be added
    }

type TestCaseDataSource() =
    let fileWithoutAlphSection =
        [|
            "a.txt"
            "b.txt"
            ""
            "c/"
        |] |> Some
    let fileEmpty = Some [||]
    let fileAbsent = None
    let fileEmptyAlphSectionInTheBeggining = 
        [|
            GitIgnoreManager.section_start_marker
            GitIgnoreManager.section_end_marker
            "a.txt"
            "directory1/"
        |] |> Some
    let fileEmptyAlphSectionInTheMiddle = 
        [|
            "a.txt"
            GitIgnoreManager.section_start_marker
            GitIgnoreManager.section_end_marker
            ""
            "directory1/"
        |] |> Some
    let fileEmptyAlphSectionInTheEnd = 
        [|
            "a.txt"
            ""
            "directory1/"
            GitIgnoreManager.section_start_marker
            GitIgnoreManager.section_end_marker
        |] |> Some
    let fileAlphSectionInTheBeggining = 
        [|
            GitIgnoreManager.section_start_marker
            "b.txt"
            "c/"
            GitIgnoreManager.section_end_marker
            "a.txt"
            "directory1/"
        |] |> Some
    let fileAlphSectionInTheMiddle = 
        [|
            "a.txt"
            GitIgnoreManager.section_start_marker
            "b.txt"
            "c/"
            GitIgnoreManager.section_end_marker
            ""
            "directory1/"
        |] |> Some
    let fileAlphSectionInTheEnd = 
        [|
            "a.txt"
            ""
            "directory1/"
            GitIgnoreManager.section_start_marker
            "b.txt"
            "c/"
            GitIgnoreManager.section_end_marker
        |] |> Some
    let fileEmptyAlphSectionOnly = 
        [|
            GitIgnoreManager.section_start_marker
            GitIgnoreManager.section_end_marker
        |] |> Some
    let fileAlphSectionOnly = 
        [|
            GitIgnoreManager.section_start_marker
            "b.txt"
            "c/"
            GitIgnoreManager.section_end_marker
        |] |> Some

    let sequence : IEnumerable<obj array> = 
        seq {
            yield [| {InitialEntries = fileWithoutAlphSection; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileEmpty; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileAbsent; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileEmptyAlphSectionInTheBeggining; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileEmptyAlphSectionInTheMiddle; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileEmptyAlphSectionInTheEnd; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileAlphSectionInTheBeggining; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileAlphSectionInTheMiddle; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileAlphSectionInTheEnd; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileEmptyAlphSectionOnly; toBeAddedEntries=["d.txt";"e/"]} |]
            yield [| {InitialEntries = fileAlphSectionOnly; toBeAddedEntries=["d.txt";"e/"]} |]
        }
    interface IEnumerable<obj array> with
        member s.GetEnumerator() =
            sequence.GetEnumerator()

    interface IEnumerable with
        member s.GetEnumerator() =
            sequence.GetEnumerator() :> IEnumerator            


type RecordsMenagement(output) =
    inherit SingleUseOneTimeDirectory(output)

    /// returns the content of .gitignore files after adding new entries to the original content.
    /// Used to in many tests
    member private s.callAddEntriesAsync testCase =
        async {
            let gitIgnoreFile = Path.Combine(s.RelativeExperimentRoot,"gitignore.txt")

            let toBeAddedLines = testCase.toBeAddedEntries

            match testCase.InitialEntries with
            |   Some initialLines ->
                // file initially contains some entries
                use sw = new StreamWriter(gitIgnoreFile)
                for entry in initialLines do
                    do! sw.WriteLineAsync(entry) |> Async.AwaitTask
                sw.Dispose()
                output.WriteLine("Initial gitignore file successfully deployed")
            |   None -> output.WriteLine("Emulating absence of .gitignore file")
                    
            do! GitIgnoreManager.addEntriesAsync gitIgnoreFile toBeAddedLines // actual method to be tested
            output.WriteLine("addEntriesAsync successfully complete")

            return! File.ReadAllLinesAsync gitIgnoreFile |> Async.AwaitTask
        }

    [<Theory>]
    [<ClassData(typedefof<TestCaseDataSource>)>]
    member s.``New entries are added``(testCase) =
        async {
            let! lines = s.callAddEntriesAsync testCase
            let expected = Set.ofList testCase.toBeAddedEntries
            let actual = Set.ofArray lines
            Assert.True(Set.isSubset expected actual,"added entries must be present in .gitignore after addEntriesAsync call")
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<TestCaseDataSource>)>]
    member s.``Initial entries are retained``(testCase) =
        async {
            let! lines = s.callAddEntriesAsync testCase
            let expected =
                match testCase.InitialEntries with
                |   Some entries -> Set.ofArray entries
                |   None -> Set.empty
            let actual = Set.ofArray lines
            Assert.True(Set.isSubset expected actual,"inital entries must be present in .gitignore after addEntriesAsync call")
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<TestCaseDataSource>)>]
    member s.``Nor other entries are added``(testCase) =
        async {
            let! lines = s.callAddEntriesAsync testCase
            let initial =
                match testCase.InitialEntries with
                |   Some entries -> Set.ofArray entries
                |   None -> Set.empty
            let expected =
                (Set.ofList testCase.toBeAddedEntries)
                + initial
                + Set.singleton GitIgnoreManager.section_start_marker
                + Set.singleton GitIgnoreManager.section_end_marker
                - Set.singleton String.Empty // empty lines are not considered
            let actual = (Set.ofArray lines) - (Set.singleton String.Empty)
            Assert.Equal(expected.Count,actual.Count)
        } |> toAsyncFact

    [<Theory>]
    [<ClassData(typedefof<TestCaseDataSource>)>]
    member s.``Not more than one line is added``(testCase) =
        async {
            let! lines = s.callAddEntriesAsync testCase
            let actual = Array.length lines
            let initialCount =
                match testCase.InitialEntries with
                |   Some entries -> Array.length entries
                |   None -> 0
            let expected = initialCount + testCase.toBeAddedEntries.Length
            Assert.True(actual - expected <= 3) // start marker, end marker, and possible 1 empty line
        } |> toAsyncFact
