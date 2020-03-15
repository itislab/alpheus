module ItisLab.Alpheus.DependencyGraph

open System.IO
open System.Collections.Generic
open ItisLab.Alpheus.AlphFiles
open System
open System.Diagnostics
open ItisLab.Alpheus
open ItisLab.Alpheus.PathUtils
open Angara.Data
open FSharp.Control
open Logger

// Disable warning on requiring to override GetHashCode in case of Equals overriding
// As I want vertices to be alphnumerically sorted, but compared by reference
#nowarn "0346"


type ArtefactLocation  =
    /// Artefact can be restored from storage
    |   Remote
    /// Artefact is currently on disk
    |   Local

type OutdatedReason =
    | InputsOutdated
    | OutputsOutdated

/// computation status of method instance (e.g. scalar or vector element)
type MethodInstanceStatus =
    /// Expected outputs version exist
    | UpToDate of outputs: ArtefactLocation list
    | Outdated of OutdatedReason

type LinkToArtefactStatus = 
    /// there is no artefact locally and no expected version on remotes
    | NotFound
    /// there is an expected version of artefact on the local disk, and no information about remotes
    | Local
    /// there is a version of artefact on the local disk, but it has unexpected version, and no information about remotes
    | LocalUnexpected 
    /// there is no any version of artefact on the local disk, but there is a remote artefact of the expected version
    | Remote

/// This type is used by `findExpectedArtefacts`
type ExpectedArtefactSearchResult =
    |   SomeAreLocalUnexpected
    |   SomeAreNotFound
    /// All group has expected versions
    |   AllExist of location:ArtefactLocation list


type LinkElementExpectationStatus =
|   NoVersionExpected
|   ExpectedAndActual of HashString * (HashString option)

/// Checks location of the expected version of the given collection of artefacts and either returns locations of all given artefacts, if they are found; or, otherwise, returns issues.
let findExpectedArtefacts checkStoragePresence (expectations:LinkElementExpectationStatus seq) =
    async {
        let expectationsArr = Array.ofSeq expectations
        let N = Array.length expectationsArr
        let isNoExpectation = function
            |   NoVersionExpected -> true
            |   _ -> false
        if Seq.exists isNoExpectation expectationsArr then
            // some of the artefact element was not ever produced, this is invalid            
            return SomeAreLocalUnexpected
        else
            /// Chooses the pairs that are not valid on disk (filtering out versions match)
            let invalidOnDiskChooser expectation =
                match expectation with
                | ExpectedAndActual(expected,Some(actual)) when expected = actual -> None
                | _ -> Some expectation
            let localInvalid = Seq.map invalidOnDiskChooser expectationsArr |> Seq.choose id |> Array.ofSeq
            if Array.length localInvalid = 0 then
                // valid as actual disk version match expected version. No need to check the storage
                return AllExist (List.init N (fun _ -> ArtefactLocation.Local))
            else
                // check if the locally "invalid" are "remote valid" (restorable from storage in case of disk data absence)
                let eligibleForRemoteCheckChooser idx expectation =
                    match expectation with
                    |   ExpectedAndActual(expectedVersion, None) -> Some(idx,expectedVersion)
                    |   _ -> None

                let eligibleForRemoteCheck = Seq.mapi eligibleForRemoteCheckChooser localInvalid |> Seq.choose id |> Array.ofSeq
                if Array.length eligibleForRemoteCheck = Array.length localInvalid then
                    // we proceed with the remote checks only if all of the locally invalid items are eligible for remote check
                    let eligibleForRemoteCheckVersions = Array.map snd eligibleForRemoteCheck
                    let! remotePresence = checkStoragePresence (Seq.ofArray eligibleForRemoteCheckVersions)
                    if Array.forall id remotePresence then
                        let resultArray = Array.create N ArtefactLocation.Local
                        // as all of the remote check eligible elements are present remotely
                        eligibleForRemoteCheck
                        |> Seq.map fst
                        |> Seq.iter (fun idx -> (resultArray.[idx] <- ArtefactLocation.Remote))
                        return AllExist (List.ofArray resultArray)
                    else
                        return SomeAreNotFound
                else
                    // otherwise at least one unrestorable item exists on disk. Thus invalid
                    return SomeAreLocalUnexpected
    }

/// Optional settings of the execution
type CommandExecutionSettings = {
    /// If true disables the deletion of output artefact on disk before activation on CLI command
    DoNotCleanOutputs: bool
    /// Exit codes that are considered to be successful computation
    SuccessfulExitCodes: int list
    /// Which of the resources the current command consumes
    ResourceGroups: Set<string>
}
with
    static member Default = 
        { DoNotCleanOutputs = false
          SuccessfulExitCodes = [ 0 ]
          ResourceGroups = Set.empty
          }

type ArtefactVertex(id:ArtefactId, experimentRoot:string) =    
    // experiment root is needed to calc actual data versions (via path to the actual data)
    let mutable producer : MethodVertex option = None
    let mutable usedIn : Set<CommandLineVertex> = Set.empty
    let mutable isTracked = false
    let actualVersion = ActualArtefactVersion(id, experimentRoot)
    let saveLock = obj()
    let rank = Lazy<int>(fun() -> Artefacts.rank id)

    member s.Id = id

    /// Gets a method which produces the artefact.
    member s.ProducedBy
        with get() =
            match producer with
            | Some(v) -> v
            | None -> invalidOp "The artefact vertex has no producer vertex set. Set the ProduceBy before retrieving its value"
        and set v = producer <- Some(v)

    /// Gets a set of methods using this artefact as an input.
    member s.UsedIn = usedIn
    
    member s.IsTracked
        with get() = isTracked
        and set v = isTracked <- v
    
    /// Gets the version calculated from the data on the disk.
    /// Lazy execution. Calculated based on disk data only on the first call. 
    /// Later returns the hashed results, unless ForceActualVersionCalc() is called.
    member s.ActualVersion = actualVersion

    member s.Rank = rank.Value

    member s.ExperimentRoot = experimentRoot

    member s.AddUsedIn method = usedIn <- usedIn |> Set.add method 

    
    /// Builds an AlphFile instance describing the artefact and saves it to disk
    member s.SaveAlphFile() =
        let alphFileFullPath = s.Id |> PathUtils.idToAlphFileFullPath experimentRoot
        if not (Path.IsPathRooted alphFileFullPath) then
            raise(ArgumentException(sprintf "alphFileFullPath must contain rooted full path: %s" alphFileFullPath))
        lock saveLock (fun() ->
            logVerbose DependencyGraph (sprintf "Saving alph file for artefact %A" s.Id)
            // Constructing the "content" and dumping to the disk must be under the same lock
            // as there is no guarantee that the if we lock only on disk dump it will be served in a FIFO manner
            // on Linux it is not. Thus if we want the latest content dump to be written the last on disk we need to lock the content as well
            let content = 
                // Fills in Signature field in compute section object with correct value       
                match s.ProducedBy with
                | MethodVertex.Source(sourceVertex) ->
                    let experimentRoot = sourceVertex.ExperimentRoot
                    let expectedVersion = sourceVertex.Output.ExpectedVersion
                    let artefactPath = sourceVertex.Output.Artefact.Id |> idToFullPath experimentRoot
                    let snapshotSection : AlphFiles.VersionedArtefact = 
                        { Hash = expectedVersion
                          RelativePath = relativePath alphFileFullPath artefactPath }
                    {
                        FileFormatVersion = Versioning.AlphFileCurrentVersion
                        Origin = SourceOrigin snapshotSection
                        IsTracked = sourceVertex.Output.Artefact.IsTracked
                    }            
                | MethodVertex.Command(commandVertex) ->
                    // dependency graph contains all paths relative to project root
                    // alpheus files contains all paths relative to alph file   
                    let experimentRoot = experimentRoot                
                    let computeSection =
                        let alphFileRelativeWorkingDir = 
                            let workingDirFull = Path.GetFullPath(Path.Combine(experimentRoot,commandVertex.WorkingDirectory))
                            let candidate = relativePath alphFileFullPath workingDirFull
                            if candidate = "" then ("." + string Path.DirectorySeparatorChar) else candidate
                        let toSection (a:LinkToArtefact) = 
                            let relative = relativePath alphFileFullPath (a.Artefact.Id |> idToFullPath experimentRoot)
                            { RelativePath = relative; Hash = a.ExpectedVersion}
    
                        { Inputs = commandVertex.Inputs |> Seq.map toSection |> List.ofSeq
                          Outputs = commandVertex.Outputs |> Seq.map toSection |> List.ofSeq
                          OutputIndex = commandVertex.Outputs |> Seq.findIndex (fun output -> output.Artefact.Id = s.Id)
                          Command = commandVertex.Command
                          ResourceGroups = List.ofSeq commandVertex.ResourceGroups
                          WorkingDirectory = alphFileRelativeWorkingDir
                          Signature = String.Empty
                          OutputsCleanDisabled = commandVertex.DoNotCleanOutputs
                          SuccessfulExitCodes = commandVertex.SuccessfulExitCodes
                          }
                    {
                        FileFormatVersion = Versioning.AlphFileCurrentVersion
                        Origin = DataOrigin.CommandOrigin { computeSection with Signature = Hash.getSignature computeSection}
                        IsTracked = s.IsTracked
                    }
                   
            PathUtils.ensureDirectories alphFileFullPath
            AlphFiles.save content alphFileFullPath
            logVerbose DependencyGraph (sprintf "Saved alph file for artefact %A" s.Id))
        
    interface System.IComparable with
        member s.CompareTo(other) =
            match other with
            | :? ArtefactVertex as otherArtefactVertex ->
                s.Id.ToString().CompareTo(otherArtefactVertex.Id.ToString())
            |  _ -> invalidArg "other" "System.IComaprable.CompareTo must be called on the object of the same types"

    override s.Equals(obj1:obj) =
        // this override prevents usage of System.IComparable for equality checks
        Object.ReferenceEquals(s,obj1)
       
    override s.ToString() =        
        sprintf "Artefact(%O|%O)" s.Id actualVersion




/// Represents a link to a specific version of an artefact.
and LinkToArtefact(artefact: ArtefactVertex, expectedVersion: ArtefactVersion) = 
    let mutable expected = expectedVersion
    let lockObj = obj()
    
    /// Creates a link to the artefact which expects the given actual version.
    new(artefact) = LinkToArtefact(artefact, MdMap.scalar None)

    member s.Artefact : ArtefactVertex = artefact

      /// What version of the corresponding artefact is expected to work with. Empty version means that the artefact version was never fixed.
      /// If the actual version differs from the expected, it should be handled specifically.
    member s.ExpectedVersion : ArtefactVersion = expected

    member s.AnalyzeStatus checkStoragePresence (index: string list) =    
        if index.Length <> artefact.Rank then invalidArg "index" "Index doesn't correspond to the rank of the artefact"
        async {
            let expectedVersion = MdMap.find index expected
            let! artefactItemActualVersion = artefact.ActualVersion.Get index
            let expectation =
                match expectedVersion with
                |   None -> NoVersionExpected
                |   Some(expVer) -> ExpectedAndActual(expVer,artefactItemActualVersion)
            let! status = findExpectedArtefacts checkStoragePresence (Seq.singleton expectation)
            match status with
            |   SomeAreLocalUnexpected -> return LocalUnexpected
            |   SomeAreNotFound -> return NotFound
            |   AllExist items ->
                match List.head items with
                |   ArtefactLocation.Local -> return LinkToArtefactStatus.Local
                |   ArtefactLocation.Remote -> return LinkToArtefactStatus.Remote
        }

    /// Makes the link to expect the actual version.
    /// The actual version MUST be available.
    member s.ExpectActualVersionAsync(index: string list) = 
        if index.Length <> artefact.Rank then invalidArg "index" "Index doesn't correspond to the rank of the artefact"
        async {
            let! actual = artefact.ActualVersion.Get index
            lock lockObj (fun() -> expected <- expected |> MdMap.add index actual)
            return ()
        }

    override s.ToString() = sprintf "LinkToArtefact %A [expected version %A]" artefact expected


and [<CustomEquality; CustomComparison>] MethodVertex =
    /// The vertex produces single artefact out of void
    |   Source of SourceVertex
    /// The vertex corresponds to invocation of single CLI command
    |   Command of CommandLineVertex
    with 
        member x.MethodId : MethodId =
            match x with
            | Source src -> src.MethodId
            | Command cmd -> cmd.MethodId

        member x.ExperimentRoot : string = 
            match x with
            | Source src -> src.ExperimentRoot
            | Command cmd -> cmd.ExperimentRoot

        interface IComparable<MethodVertex> with
            member x.CompareTo other = x.MethodId.CompareTo (other.MethodId)  
        interface IComparable with
            member x.CompareTo other =
                match other with
                | null -> nullArg "other"
                | :? MethodVertex as other -> (x:>IComparable<MethodVertex>).CompareTo(other)
                | _ -> invalidArg "other" "Cannot compare values of different types"
        override x.Equals other =
            match other with
            | :? MethodVertex as other -> (x:>IComparable<MethodVertex>).CompareTo(other) = 0
            | _ -> false
        override x.GetHashCode() = x.MethodId.GetHashCode()

/// The vertex produces single artefact out of void
and SourceVertex(methodId: MethodId, output: LinkToArtefact, experimentRoot: string) =
    /// Gets the artefact produced by this source vertex. 
    /// We keep the version of the artefact, so we know what data should be restored if there is just an alph file.
    member s.Output : LinkToArtefact = output
    member s.MethodId : MethodId = methodId
    member s.ExperimentRoot : string = experimentRoot

    override s.ToString() = sprintf "id = %s" methodId


/// Represents a method defined as a command line.
and CommandLineVertex(
                        methodId : MethodId,
                        experimentRoot: string,
                        inputs: LinkToArtefact list,
                        outputs: LinkToArtefact list,
                        command: string,
                        workingDir:ExperimentRelativePath,
                        executionSettings: CommandExecutionSettings) =
    let exitCodeLockObj = obj()
    let outputStatusesLockObj = obj()
    let mutable commandExitCode: MdMap<string,int option> = MdMap.empty

    do
        if not(PathUtils.isDirectory workingDir) then invalidArg "workingDir" "WorkingDirectory doesn't end with directory separator char and considered as a path to a file"
        if Path.IsPathRooted workingDir then invalidArg "workingDir" "WorkingDirectory is absolute"

    member s.MethodId = methodId    
    member s.ExperimentRoot = experimentRoot

    /// For each of the input artefact, we keep the expected version, i.e. what version was used to produce the current outputs.
    member s.Inputs = inputs
    
    /// For each of the output artefact, we keep the expected version, i.e. what version was produced, so we could
    /// check if the what is currently available is the same or not (e.g. it could be edited manually).
    /// This allows to tell if we need to execute the method, i.e. the output was edited externally, or not executed, if it is the expected output.
    member s.Outputs = outputs

    /// This command can be executed to get the up to date versions of outputs
    member s.Command
        with get() =
            command
    
    /// None if the command execution was not launched or not finished yet, other wise holds the execution exit code
    member s.GetExitCode index =
        MdMap.find index commandExitCode

    member s.SetExitCode index exitCode=
        lock exitCodeLockObj (fun () -> 
                                commandExitCode <- MdMap.add index (Some exitCode) commandExitCode)

    /// Command line exit codes that are considered successful
    member s.SuccessfulExitCodes
        with get() = executionSettings.SuccessfulExitCodes

    /// Which of the resources the current command consumes
    member s.ResourceGroups
        with get() = executionSettings.ResourceGroups

    /// From where the Command must be executed.
    /// Experiment root related
    member s.WorkingDirectory 
        with get() =
            workingDir
        
    member s.DoNotCleanOutputs
        with get() = executionSettings.DoNotCleanOutputs

    /// Reads the actual version of the output artefacts, 
    /// updates the expected versions for the input and output artefacts,
    /// and updates the *.alph file.
    member s.OnSucceeded(index: string list) : Async<unit> =
        let expectActualStrict index invalidate (link:LinkToArtefact) =
            async {
                if invalidate then link.Artefact.ActualVersion.Invalidate index
                do! link.ExpectActualVersionAsync index   
            }

        let expectActual index invalidate (link:LinkToArtefact) =
            if List.length index < link.Artefact.Rank then // scatter
                let partialPath = link.Artefact.Id |> PathUtils.idToFullPath experimentRoot |> PathUtils.applyIndex index
                PathUtils.enumeratePath partialPath 
                |> MdMap.toSeq
                |> Seq.map(fun (extraIndex, _) -> 
                    let fullIndex = index @ extraIndex
                    link |> expectActualStrict fullIndex invalidate)
                |> Async.Parallel
                |> Async.Ignore
            else // reduce or one2one
                let strictIndex = index |> List.truncate link.Artefact.Rank 
                link |> expectActualStrict strictIndex invalidate

        async {
            logVerbose DependencyGraph (sprintf "Method %A[%A] succeeded" methodId index)
            let outLinksUpdates =
                s.Outputs 
                |> Seq.map (expectActual index true) 
            // for the inputs we need to set invalidate=true to cover the case
            // when the inputs are restored from up-to-date remote state.
            let inputLinksUpdates =
                s.Inputs 
                |> Seq.map (expectActual index true) 
            do! Seq.append outLinksUpdates inputLinksUpdates |> Async.Parallel |> Async.Ignore            

            logVerbose DependencyGraph (sprintf "Saving alph files for outputs of %A%A" methodId index)
            s.Outputs |> Seq.iter(fun out -> out.Artefact.SaveAlphFile())
        }

    interface System.IComparable with
        member s.CompareTo(other) =
           let typedOther:CommandLineVertex = downcast other
           s.MethodId.CompareTo(typedOther.MethodId)

    override s.Equals(obj1:obj) =
        Object.ReferenceEquals(s,obj1)

    override s.ToString() = sprintf "id = %s, command = %s" methodId command


and internal Edge<'v>(src: 'v, trg: 'v) =
    interface Angara.Graph.IEdge<'v> with
        member s.Source = src
        member s.Target = trg

/// Alpheus dependencies graph
and Graph (experimentRoot:string) = 
    let mutable methodVertices : Map<MethodId,MethodVertex> = Map.empty
    let mutable artefactVertices: Map<ArtefactId,ArtefactVertex> = Map.empty 

    let topoSortArtefacts() =
        let dag = Angara.Graph.DirectedAcyclicGraph<ArtefactVertex, Edge<ArtefactVertex>>()
        let dagWithVertices = artefactVertices |> Map.fold (fun (g:Angara.Graph.DirectedAcyclicGraph<ArtefactVertex, Edge<ArtefactVertex>>) _ (a:ArtefactVertex) -> g.AddVertex a) dag
        let dagWithEdges = 
            artefactVertices 
            |> Map.fold (fun dag _ (a:ArtefactVertex) -> 
                a.UsedIn 
                |> Set.toSeq 
                |> Seq.map(fun (m:CommandLineVertex) -> m.Outputs) 
                |> Seq.collect id
                |> Seq.map(fun link -> Edge<ArtefactVertex>(a, link.Artefact))
                |> Seq.fold(fun (g:Angara.Graph.DirectedAcyclicGraph<ArtefactVertex, Edge<ArtefactVertex>>) e -> g.AddEdge e) dag
                ) dagWithVertices
        let sorted = dagWithEdges |> Angara.Graph.toSeqSorted 
        sorted
    let mutable sortedArtefacts = Lazy<ArtefactVertex list>(topoSortArtefacts)
    let invalidateSortedArtefacts() = sortedArtefacts <- Lazy<ArtefactVertex list>(topoSortArtefacts)
    
    let getMethodId (outputs: ArtefactId seq) : MethodId = 
        outputs
        |> Seq.map(fun o -> o.ToString())
        |> Seq.sort
        |> Seq.head

    member s.Artefacts = sortedArtefacts.Value

    member s.Methods =
        Map.toSeq methodVertices |> Seq.map snd |> Array.ofSeq

    member s.ArtefactsCount =
        Map.count artefactVertices

    member s.MethodsCount =
        Map.count methodVertices

    static member Build (experimentRootPath: string, artefactIds: ArtefactId seq) =
        let experimentRootPath = normalizePath experimentRootPath
        let g = Graph(experimentRootPath)
        let initialArtefacts = artefactIds |> Seq.map(fun id -> g.GetOrAddArtefact id) |> Seq.toList
        g.LoadDependencies initialArtefacts
        g

    /// Adds new command line method to the graph with the given inputs and outputs.
    /// If some input/output artefact has no actual version, the empty version is used as expected.
    /// Creates/rewrites the .alph files corresponding to the output artefacts.
    /// Returns the added method.
    member s.AddMethod (command:string) (inputIds: ArtefactId seq) (outputIds: ArtefactId seq) (workingDir:ExperimentRelativePath) (executionSettings:CommandExecutionSettings) =
        async {
            let idToLink = s.GetOrAddArtefact >> LinkToArtefact
            let inputs = inputIds |> Seq.map idToLink  
            let outputs = outputIds |> Seq.map idToLink
            let outputs = outputs |> Seq.map (fun x -> LinkToArtefact(x.Artefact,x.ExpectedVersion |> MdMap.map(fun _ -> None))) // invalidating outputs (for the case if the previously defined command is updated)
            let method = s.AddOrGetCommand command inputs outputs workingDir executionSettings
            do! outputs 
                |> AsyncSeq.ofSeq
                |> AsyncSeq.iterAsyncParallel (fun output -> System.Threading.Tasks.Task.Run(fun () -> output.Artefact.SaveAlphFile()) |> Async.AwaitTask)
            return method
        }

    /// Takes a list of outputs and builds (recreates using .alph files) a dependency graph
    member s.LoadDependencies (outputs:ArtefactVertex list) =
        let mutable processedOutputs : Set<ArtefactVertex> = Set.empty   
          
        let queue = Queue<ArtefactVertex>()
        // queuing initial outputs            
        List.iter (fun x -> queue.Enqueue x) outputs
        while queue.Count > 0 do
            // Processing the graph artefact vertex
            // to process a vertex A means 1) to allocate verteces for direct A's producer method and it's direct inputs; 2) connect them 3) Enqueue them to be processed
            let dequeuedArtefact = queue.Dequeue()
            let alphFileFullPath = dequeuedArtefact.Id |> idToAlphFileFullPath experimentRoot
            try
                match tryLoad alphFileFullPath with
                | None -> 
                    // Absence of .alph file means that the artefact is initial (not produced)
                    // Thus it corresponds to a source vertex (no inputs)
                    // We must create it now
                    s.AddOrGetSource (LinkToArtefact(dequeuedArtefact, MdMap.scalar None)) |> ignore

                | Some(alphFile) ->
                    // Alph file exists
                    dequeuedArtefact.IsTracked <- alphFile.IsTracked
                    match alphFile.Origin with
                    | SourceOrigin(alphSource) -> // Snapshot in .alph file means that is was snapshoted, thus Tracked
                        s.AddOrGetSource (LinkToArtefact(dequeuedArtefact, alphSource.Hash)) |> ignore
                    | CommandOrigin(alphCommand) -> // produced by some method.
                        // checking weather the internals were modified (weather the output hashes mentioned are valid)
                        let alphCommand = Hash.validateSignature(alphCommand)

                        let makeLink versionArtefact =  
                            let artefact = versionArtefact.RelativePath |> alphRelativePathToId alphFileFullPath experimentRoot |> s.GetOrAddArtefact
                            LinkToArtefact(artefact, versionArtefact.Hash)                    
                        let inputs = alphCommand.Inputs |> List.map makeLink                                                                                
                        let outputs = alphCommand.Outputs |> List.map makeLink

                        let expRootRelatedWorkingDir : ExperimentRelativePath = 
                            let alphFileDir = Path.GetDirectoryName(normalizePath(alphFileFullPath))
                            let path = Path.GetRelativePath(experimentRoot, Path.GetFullPath(Path.Combine(alphFileDir, alphCommand.WorkingDirectory)))
                            if path.EndsWith(Path.DirectorySeparatorChar) then path else path + string Path.DirectorySeparatorChar

                        let settings = {
                            DoNotCleanOutputs = alphCommand.OutputsCleanDisabled
                            SuccessfulExitCodes = alphCommand.SuccessfulExitCodes
                            ResourceGroups = set alphCommand.ResourceGroups
                        }

                        let _ = s.AddOrGetCommand alphCommand.Command inputs outputs expRootRelatedWorkingDir settings
                        inputs |> Seq.map (fun inp -> inp.Artefact) |> Seq.filter(not << processedOutputs.Contains) |> Seq.iter (fun (x:ArtefactVertex) -> queue.Enqueue x)                         
            with
                | :? InvalidDataException as ex ->
                Logger.logError Logger.DependencyGraph (sprintf "Error during building dependency graph, in particular processing artefact %A" dequeuedArtefact)
                raise ex
            processedOutputs <- Set.add dequeuedArtefact processedOutputs

    member private s.GetOrAddArtefact artefactId =
        match artefactVertices |> Map.tryFind artefactId  with
        | Some(vertex) -> 
            vertex
        | None ->
            let vertex = ArtefactVertex(artefactId,experimentRoot)
            artefactVertices <- artefactVertices |> Map.add artefactId vertex 
            invalidateSortedArtefacts()
            vertex
    
    member s.GetArtefact artefactId =
        match artefactVertices |> Map.tryFind artefactId  with
        | Some(vertex) -> 
            Ok vertex
        | None ->
            Error (sprintf "ArtefactID %A is not found in the dependency graph" artefactId |> SystemError)

    member s.GetMethod methodId = 
        match methodVertices |> Map.tryFind methodId  with
        | Some(vertex) -> 
            Ok vertex
        | None ->
            Error (sprintf "MethodID %A is not found in the dependency graph" methodId |> SystemError)

    /// Adds a method vertex for the given artefact.
    member private s.AddOrGetSource (output: LinkToArtefact) : SourceVertex =
        let methodId = getMethodId (Seq.singleton output.Artefact.Id)
        match methodVertices |> Map.tryFind methodId with
        | Some (Source m) -> m
        | Some (_) -> invalidOp (sprintf "Method %A already exists but has wrong type" methodId)
        | None ->
            let vertex = SourceVertex(methodId, output, experimentRoot)
            output.Artefact.ProducedBy <- Source vertex
            methodVertices <- methodVertices |> Map.add methodId (Source vertex) 
            vertex

    /// Adds a method vertex for the given command and its inputs/outputs.
    /// Returns a method if it was added or if it already exists and has the expected type.
    /// Otherwise throws.
    member private s.AddOrGetCommand (command:string) (inputs: LinkToArtefact seq) (outputs: LinkToArtefact seq) (workingDir:ExperimentRelativePath) (executionSettings: CommandExecutionSettings) : CommandLineVertex = 
        let methodId = getMethodId (outputs |> Seq.map(fun out -> out.Artefact.Id))
        match methodVertices |> Map.tryFind methodId with
        | Some (Command m) -> m
        | Some (_) -> invalidOp (sprintf "Method %A already exists but has wrong type" methodId)
        | None ->
            let vertex = CommandLineVertex (methodId,
                                            experimentRoot,
                                            inputs |> Seq.toList,
                                            outputs |> Seq.toList,
                                            command, workingDir, executionSettings)
            inputs |> Seq.iter(fun inp -> inp.Artefact.AddUsedIn vertex)
            outputs |> Seq.iter(fun out -> out.Artefact.ProducedBy <- Command vertex)
            methodVertices <- methodVertices |> Map.add methodId (Command vertex) 
            vertex
