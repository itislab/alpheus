module ItisLab.Alpheus.GitIgnoreManager

open System.IO
open System.Text

// This modules manages the alpheus related content of .gitignore
// .gitignore which is held in the experiment root holds special section // ---alpheus-managed----
// which is maintained by alpheus

let private section_start = "// ---alpheus-managed----"
let private section_end = "// ---alpheus-managed-end----"

type ParsingPhase =
    |   LookingForStart
    |   LookingForEnd
    |   EndFound

/// entry_to_add - path relative to alpheus experiment root
let addEntryAsync gitignore_path entry_to_add =
    async {
    let head = StringBuilder()
    let mutable alpheus_entries = Set.empty
    let mutable parsing_phase = ParsingPhase.LookingForStart
    let tail = StringBuilder()
    if not (File.Exists(gitignore_path)) then
        File.Create(gitignore_path) |> ignore
    else
        use sr = new StreamReader(gitignore_path)

        let (|SectionStart|SectionEnd|NotMarker|) str =
            if str = section_start then
                SectionStart
            elif str = section_end then
                SectionEnd
            else
                NotMarker

        while not(sr.EndOfStream) do
            let! line = sr.ReadLineAsync() |> Async.AwaitTask            
            match line, parsing_phase with
            |   SectionStart, LookingForStart -> parsing_phase <- LookingForEnd
            |   SectionStart, LookingForEnd -> raise(InvalidDataException(".gitignore file contains double alpheus section start markers"))
            |   SectionStart, EndFound -> raise(InvalidDataException(".gitignore file contains two or more alpheus sections. there must be only one alpheus section"))
            |   SectionEnd, LookingForStart -> raise(InvalidDataException(".gitignore file contains double alpheus section end marker prior to session section start marker"))
            |   SectionEnd, LookingForEnd -> parsing_phase <- EndFound
            |   SectionEnd, EndFound -> raise(InvalidDataException(".gitignore file contains double alpheus section end markers"))
            |   NotMarker, LookingForStart -> head.AppendLine(line) |> ignore
            |   NotMarker, LookingForEnd -> alpheus_entries <- Set.add line alpheus_entries
            |   NotMarker, EndFound -> tail.AppendLine(line) |> ignore

        alpheus_entries <- Set.add entry_to_add alpheus_entries

        let alpheusSectionBuilder = StringBuilder()
        for str in alpheus_entries do
            alpheusSectionBuilder.AppendLine(str) |> ignore

        use sw = new StreamWriter(gitignore_path)
        do! sw.WriteLineAsync(head.ToString()) |> Async.AwaitTask
        do! sw.WriteLineAsync(section_start) |> Async.AwaitTask
        do! sw.WriteLineAsync(alpheusSectionBuilder.ToString()) |> Async.AwaitTask
        do! sw.WriteLineAsync(section_end) |> Async.AwaitTask
        do! sw.WriteLineAsync(tail.ToString()) |> Async.AwaitTask
    }