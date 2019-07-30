module ItisLab.Alpheus.GitIgnoreManager

open System.IO
open System.Text

// This modules manages the alpheus related content of .gitignore
// .gitignore which is held in the experiment root holds special section // ---alpheus-managed----
// which is maintained by alpheus

// theses are not private for tests access
let section_start_marker = "// ---alpheus-managed-start---"
let section_end_marker = "// ---alpheus-managed-end---"

type private ParsingPhase =
    /// alpheus section marker is not traversed yet
    |   LookingForStart
    /// alpheus section start marker has been traversed but alpheus section end marker has not been travered yet
    |   LookingForEnd
    /// alpheius section end marker has been traversed
    |   EndFound

/// entries_to_add - artefactID string
let addEntriesAsync gitignore_path entries_to_add =
    async {
        let head = StringBuilder()
        let mutable alpheus_entries = Set.empty
        let mutable parsing_phase = ParsingPhase.LookingForStart
        let tail = StringBuilder()
        if File.Exists(gitignore_path) then
            use sr = new StreamReader(gitignore_path)

            let (|SectionStart|SectionEnd|NotMarker|) str =
                if str = section_start_marker then
                    SectionStart
                elif str = section_end_marker then
                    SectionEnd
                else
                    NotMarker

            try
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
            finally
                sr.Dispose()
        
        for entry_to_add in entries_to_add do
            alpheus_entries <- Set.add entry_to_add alpheus_entries
        let alpheusSectionBuilder = StringBuilder()
        for str in alpheus_entries do
            alpheusSectionBuilder.AppendLine(str) |> ignore


        let writeGitIgnore (path:string) (head:string) (alphSection:string) (tail:string) =
            async {
                use sw = new StreamWriter(path)
                do! sw.WriteAsync(head) |> Async.AwaitTask
                do! sw.WriteLineAsync(section_start_marker) |> Async.AwaitTask
                do! sw.WriteAsync(alphSection) |> Async.AwaitTask
                do! sw.WriteLineAsync(section_end_marker) |> Async.AwaitTask
                do! sw.WriteAsync(tail) |> Async.AwaitTask
                sw.Dispose()
                return ()
            }

        match parsing_phase with
        |   LookingForEnd ->
            // this is erroneous situation, as the alpheus section is not closed
            raise(InvalidDataException("The alpheus section in the .gitignore file is not closed"))
        |   EndFound | LookingForStart ->
            // the alpheus section presents in the .gitignore
            // or alpheus section does not present in the .gitignore at all. So creating it
            // the behavior is the same
            let writeGitIgnoreComputation = writeGitIgnore gitignore_path (head.ToString()) (alpheusSectionBuilder.ToString()) (tail.ToString())
            if File.Exists gitignore_path then
                let initialAttributes = File.GetAttributes(gitignore_path)
                try
                    // wiping out Hidden, Archive and Readonly attributes if any
                    let writableAttributesInt =
                        (int initialAttributes) &&&
                        (~~~ (int FileAttributes.Hidden)) &&&
                        (~~~ (int FileAttributes.Archive)) &&&
                        (~~~ (int FileAttributes.ReadOnly))
                    let writableAttributes: FileAttributes = downcast FileAttributes.ToObject(typedefof<FileAttributes>,writableAttributesInt)
                    Logger.logVerbose Logger.GitIgnoreManager "Making .gitignore file writable"
                    File.SetAttributes(gitignore_path, writableAttributes)
                    Logger.logVerbose Logger.GitIgnoreManager "Writing .gitignore file"
                    do! writeGitIgnoreComputation

                finally
                    // restoring back attributes
                    Logger.logVerbose Logger.GitIgnoreManager "Restoring initial .gitignore attributes"
                    File.SetAttributes(gitignore_path,initialAttributes)
            else
                do! writeGitIgnoreComputation
    }