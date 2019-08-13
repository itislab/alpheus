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

let ts = TraceSource("Dependency Graph")

// Disable warning on requiring to override GetHashCode in case of Equals overriding
// As I want vertices to be alphnumerically sorted, but compared by reference
#nowarn "0346"


type ArtefactVertex(id:ArtefactId) =    
    let mutable producer : MethodVertex option = None
    let mutable usedIn : Set<CommandLineVertex> = Set.empty
    let mutable isTracked = false
    /// None means that file/dir does not exist on disk or is not computed yet.
    let mutable actualVersion : ArtefactVersion option = None 
    let lockObj = obj()
    
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
    member s.ActualVersion = actualVersion

    member s.Rank = Artefacts.rank id

    member s.AddUsedIn method = usedIn <- usedIn |> Set.add method 

    /// Computes actual version for the graph artefact item from the disk and updates the *.alph file.
    /// Updates value of ArtefactVertex.ActualVersion.
    /// Updates .hash file on a disk.
    member s.UpdateActualVersion () : Async<unit> =
        async {
            let fullPath = s.Id |> idToFullPath s.ProducedBy.ExperimentRoot 
            let! hash = Hash.hashVectorPathAndSave fullPath

            lock lockObj (fun () ->
                actualVersion <- Some hash
                s.SaveAlphFile() |> Async.RunSynchronously
            )
        }

    /// Computes actual version for the graph artefact item from the disk and updates the *.alph file.
    /// Updates value of ArtefactVertex.ActualVersion.
    /// Updates .hash file on a disk.
    member s.UpdateActualVersion (index: string list) : Async<unit> =
        async {
            let fullPath = s.Id |> idToFullPath s.ProducedBy.ExperimentRoot |> applyIndex index
            let! hash = Hash.hashPathAndSave fullPath

            lock lockObj (fun () ->
                let v = actualVersion |> Option.defaultValue MdMap.empty
                actualVersion <- v |> MdMap.add index hash |> Some
                s.SaveAlphFile() |> Async.RunSynchronously
            )
        }

    
    /// Builds an AlphFile instance describing the artefact.
    member s.SaveAlphFile() : Async<unit> =
        let alphFileFullPath = s.Id |> PathUtils.idToAlphFileFullPath s.ProducedBy.ExperimentRoot
        if not (Path.IsPathRooted alphFileFullPath) then
            raise(ArgumentException(sprintf "alphFileFullPath must contain rooted full path: %s" alphFileFullPath))
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
                    Origin = SourceOrigin snapshotSection
                    IsTracked = sourceVertex.Output.Artefact.IsTracked
                }            
            | MethodVertex.Command(commandVertex) ->
                // dependency graph contains all paths relative to project root
                // alpheus files contains all paths relative to alph file   
                let experimentRoot = commandVertex.ExperimentRoot
                let computeSection =
                    let alphFileRelativeWorkingDir = 
                        let workingDirFull = Path.GetFullPath(Path.Combine(experimentRoot,commandVertex.WorkingDirectory))
                        let candidate = relativePath alphFileFullPath workingDirFull
                        if candidate = "" then ("." + string Path.DirectorySeparatorChar) else candidate
                    let toSection (a:LinkToArtefact) = 
                        let relative = relativePath alphFileFullPath (a.Artefact.Id |> idToFullPath experimentRoot)
                        { RelativePath = relative; Hash = a.ExpectedVersion}
    
                    { Inputs = commandVertex.Inputs |> Seq.map toSection |> Array.ofSeq
                      Outputs = commandVertex.Outputs |> Seq.map toSection |> Array.ofSeq
                      OutputIndex = commandVertex.Outputs |> Seq.findIndex (fun output -> output.Artefact.Id = s.Id)
                      Command = commandVertex.Command                
                      WorkingDirectory = alphFileRelativeWorkingDir
                      Signature = String.Empty
                      OutputsCleanDisabled = commandVertex.DoNotCleanOutputs }
                {
                    Origin = DataOrigin.CommandOrigin { computeSection with Signature = Hash.getSignature computeSection}
                    IsTracked = s.IsTracked
                }
        AlphFiles.saveAsync content alphFileFullPath
        
    interface System.IComparable with
        member s.CompareTo(other) =
            match other with
            | :? ArtefactVertex as otherArtefactVertex ->
                s.Id.ToString().CompareTo(otherArtefactVertex.Id.ToString())
            |  _ -> invalidArg "other" "System.IComaprable.CompareTo must be called on the object of the same types"

    override s.Equals(obj1:obj) =
        // this override prevennts usage of System.IComparable for equality checks
        Object.ReferenceEquals(s,obj1)
       
    override s.ToString() =
        let version =
            match s.ActualVersion with
            |   None -> "not exist"
            |   Some(hash) -> sprintf "%A" (hash |> MdMap.map(Option.map(fun s -> s.Substring(0,6))))
        sprintf "Artefact(%s|%s)" (s.Id.ToString()) version

/// Represents a link to a specific version of an artefact.
and LinkToArtefact(artefact: ArtefactVertex, expectedVersion: ArtefactVersion) = 
    let mutable expected = expectedVersion

    /// Creates a link to the artefact which expects the given actual version.
    new(artefact) = LinkToArtefact(artefact, artefact.ActualVersion |> Option.defaultValue MdMap.empty)

    member s.Artefact : ArtefactVertex = artefact

      /// What version of the corresponding artefact is expected to work with. Empty version means that the artefact version was never fixed.
      /// If the actual version differs from the expected, it should be handled specifically.
    member s.ExpectedVersion : ArtefactVersion = expected

    /// Makes the link to expect the actual version.
    /// The actual version MUST be available.
    member s.ExpectActualVersion(index: string list) = 
        let actual = artefact.ActualVersion.Value |> MdMap.get index
        expected <- expected |> MdMap.set index actual


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


/// Represents a method defined as a command line.
and CommandLineVertex(methodId : MethodId, experimentRoot: string, inputs: LinkToArtefact list, outputs: LinkToArtefact list, command: string) =
    let mutable workingDirectory: ExperimentRelativePath = String.Empty
    let mutable doNotClean = false
    let mutable commandExitCode: int option = None

    member s.MethodId = methodId    
    member s.ExperimentRoot = experimentRoot

    /// For each of the input artefact, we keep the expected version, i.e. what version was used to produce the current outputs.
    member s.Inputs = inputs
    
    /// For each of the output artefact, we keep the expected version, i.e. what version was produced, so we could
    /// check if the what is currently available is the same or not (e.g. it could be edited manually).
    /// This allows to tell if we need to execute the method, i.e. the output was edited externally, or not executed, if it is the expected output.
    member s.Outputs = outputs

    /// This command can be executed to get the up to date versions of outputs
    member s.Command = command
    
    /// None if the command execution was not launched or not finished yet, other wise holds the execution exit code
    member s.ExitCode
        with get() = commandExitCode
        and set v = commandExitCode <- v

    /// From where the Command must be executed.
    /// Experiment root related
    member s.WorkingDirectory 
        with get() = workingDirectory
        and set (v:ExperimentRelativePath) =
            assert (v.EndsWith(Path.DirectorySeparatorChar) || v.EndsWith(Path.AltDirectorySeparatorChar))
            workingDirectory <- v
    
    member s.DoNotCleanOutputs
        with get() = doNotClean
        and set v = doNotClean <- v

    /// Reads the actual version of the output artefacts, 
    /// updates the expected versions for the input and output artefacts,
    /// and updates the *.alph file.
    member s.OnSucceeded(index: string list) : Async<unit> =
        s.Outputs 
        |> Seq.map(fun out -> async { 
                do! out.Artefact.UpdateActualVersion index
                out.ExpectActualVersion index
            }) 
        |> Async.Parallel
        |> Async.Ignore

    interface System.IComparable with
        member s.CompareTo(other) =
           let typedOther:CommandLineVertex = downcast other
           s.MethodId.CompareTo(typedOther.MethodId)

    override s.Equals(obj1:obj) =
        Object.ReferenceEquals(s,obj1)


/// Alpheus dependencies graph
and Graph (experimentRoot:string) = 
    // todo: these collections are populated non-atomically and cause misleading
    let mutable methodVertices : Map<MethodId,MethodVertex> = Map.empty
    let mutable artefactVertices: Map<ArtefactId,ArtefactVertex> = Map.empty 
    
    let getMethodId (outputs: ArtefactId seq) : MethodId = 
        outputs
        |> Seq.map(fun o -> o.ToString())
        |> Seq.sort
        |> Seq.head

    member s.Artefacts =
        Map.toSeq artefactVertices |> Seq.map snd |> Array.ofSeq

    member s.Methods =
        Map.toSeq methodVertices |> Seq.map snd |> Array.ofSeq

    member s.ArtefactsCount =
        Map.count artefactVertices

    member s.MethodsCount =
        Map.count methodVertices

    static member Build (experimentRootPath: string) (artefactIds: ArtefactId seq) =
        let experimentRootPath = normalizePath experimentRootPath
        let g = Graph(experimentRootPath)
        let initialArtefacts = artefactIds |> Seq.map(fun id -> g.GetOrAddArtefact id) |> Seq.toList
        g.LoadDependencies initialArtefacts experimentRootPath |> ignore
        g

    /// Adds new command line method to the graph with the given inputs and outputs.
    /// If some input/output artefact has no actual version, the empty version is used as expected.
    /// Creates/rewrites the .alph files corresponding to the output artefacts.
    /// Returns the added method.
    member s.AddMethod (command:string) (inputIds: ArtefactId seq) (outputIds: ArtefactId seq) =
        async {
            let idToLink = s.GetOrAddArtefact >> LinkToArtefact
            let inputs = inputIds |> Seq.map idToLink  
            let outputs = outputIds |> Seq.map idToLink
            let method = s.AddCommand command inputs outputs

            do! outputs 
                |> AsyncSeq.ofSeq
                |> AsyncSeq.iterAsyncParallel (fun output -> 
                    let alphPath = output.Artefact.Id |> PathUtils.idToAlphFileFullPath experimentRoot
                    output.Artefact.SaveAlphFile())
            return method
        }

    /// Takes a list of outputs and builds (recreates using .alph files) a dependency graph
    /// Returns all found dependencies incl. original "outputs" vertices
    member s.LoadDependencies (outputs:ArtefactVertex list) (experimentRootPath: string) =
        let mutable processedOutputs : Set<ArtefactVertex> = Set.empty   
          
        let queue = Queue<ArtefactVertex>()
        // queuing initial outputs            
        List.iter (fun x -> queue.Enqueue x) outputs
        while queue.Count > 0 do
            // Processing the graph artefact vertex
            // to process a vertex A means 1) to allocate verteces for direct A's producer method and it's direct inputs; 2) connect them 3) Enqueue them to be processed
            let dequeuedArtefact = queue.Dequeue()
            let fullOutputPath = dequeuedArtefact.Id |> idToFullPath experimentRootPath
            let alphFileFullPath = dequeuedArtefact.Id |> idToAlphFileFullPath experimentRootPath
            match tryLoad alphFileFullPath with
            | None -> 
                // Absence of .alph file means that the artefact is initial (not produced)
                // Thus it corresponds to a source vertex (no inputs)
                // We must create it now and fix current disk data version in it
                // so calculating actual disk data version
                let calculatedVersion = Hash.hashVectorPathAndSave fullOutputPath |> Async.RunSynchronously
                s.AddSource (LinkToArtefact(dequeuedArtefact, calculatedVersion)) |> ignore

            | Some(alphFile) ->
                // Alph file exists
                dequeuedArtefact.IsTracked <- alphFile.IsTracked
                match alphFile.Origin with
                | SourceOrigin(alphSource) -> // Snapshot in .alph file means that is was snapshoted, thus Tracked
                    s.AddSource (LinkToArtefact(dequeuedArtefact, alphSource.Hash)) |> ignore

                | CommandOrigin(alphCommand) -> // produced by some method.
                    // checking weather the internals were modified (wether the output hashes mentioned are valid)
                    let alphCommand = Hash.validateSignature(alphCommand)

                    let makeLink versionArtefact =  
                        let artefact = versionArtefact.RelativePath |> alphRelativePathToId alphFileFullPath experimentRootPath |> s.GetOrAddArtefact
                        LinkToArtefact(artefact, versionArtefact.Hash)                    
                    let inputs = alphCommand.Inputs |> Array.map makeLink                                                                                
                    let outputs = alphCommand.Outputs |> Array.map makeLink

                    let method = s.AddCommand alphCommand.Command inputs outputs 
                    method.DoNotCleanOutputs <- alphCommand.OutputsCleanDisabled
                              
                    let expRootRelatedWorkingDir : ExperimentRelativePath = 
                        let alphFileDir = Path.GetDirectoryName(normalizePath(alphFileFullPath))
                        let path = Path.GetRelativePath(experimentRootPath, Path.GetFullPath(Path.Combine(alphFileDir, alphCommand.WorkingDirectory)))
                        if path.EndsWith(Path.DirectorySeparatorChar) then path else path + string Path.DirectorySeparatorChar
                    method.WorkingDirectory <- expRootRelatedWorkingDir

                    inputs |> Seq.map (fun inp -> inp.Artefact) |> Seq.filter(not << processedOutputs.Contains) |> Seq.iter (fun (x:ArtefactVertex) -> queue.Enqueue x)                         

                processedOutputs <- Set.add dequeuedArtefact processedOutputs
        processedOutputs

    /// Computes actual versions for the graph artefacts from the disk.
    /// Updates values of ArtefactVertex.ActualVersion for all of the graph artefacts (unless this method is called, the property returns None).
    /// Updates .hash files on a disk.
    member s.ReadActualVersions() =
        async {
            do! s.Artefacts |> Seq.map (fun a -> a.UpdateActualVersion()) |> Async.Parallel |> Async.Ignore
        }

    member private s.GetOrAddArtefact artefactId =
        match artefactVertices |> Map.tryFind artefactId  with
        | Some(vertex) -> 
            vertex
        | None ->
            let vertex = ArtefactVertex(artefactId)
            artefactVertices <- artefactVertices |> Map.add artefactId vertex 
            vertex
    
    /// Adds a method vertex for the given artefact (fails if it is already added).
    member private s.AddSource (output: LinkToArtefact) : SourceVertex =
        let methodId = getMethodId (Seq.singleton output.Artefact.Id)
        if Map.containsKey methodId methodVertices then
            invalidOp (sprintf "attempt to allocate new vertex \"%s\", but the vertex with this ID is already allocated" methodId)
        let vertex = SourceVertex(methodId, output, experimentRoot)
        output.Artefact.ProducedBy <- Source vertex
        methodVertices <- methodVertices |> Map.add methodId (Source vertex) 
        vertex

    /// Adds a method vertex for the given command and its inputs/outputs (fails if it is already added).
    member private s.AddCommand (command: string) (inputs: LinkToArtefact seq) (outputs: LinkToArtefact seq) : CommandLineVertex = 
        let methodId = getMethodId (outputs |> Seq.map(fun out -> out.Artefact.Id))
        if Map.containsKey methodId methodVertices then
            invalidOp (sprintf "attempt to allocate new vertex \"%s\", but the vertex with this ID is already allocated" methodId)
        let vertex = CommandLineVertex (methodId, experimentRoot, inputs |> Seq.toList, outputs |> Seq.toList, command)
        inputs |> Seq.iter(fun inp -> inp.Artefact.AddUsedIn vertex)
        outputs |> Seq.iter(fun out -> out.Artefact.ProducedBy <- Command vertex)
        methodVertices <- methodVertices |> Map.add methodId (Command vertex) 
        vertex
