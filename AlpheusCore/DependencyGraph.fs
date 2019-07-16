module ItisLab.Alpheus.DependencyGraph

open System.IO
open System.Collections.Generic
open ItisLab.Alpheus.AlphFiles
open System
open System.Diagnostics
open ItisLab.Alpheus

let ts = TraceSource("Dependency Graph")

// Disable warning on requiring to override GetHashCode in case of Equals overriding
// As I want vertices to be alphnumerically sorted, but compared by reference
#nowarn "0346"

type VersionedArtefact = {
    Artefact: ArtefactVertex
    /// Expected version. None means that the artefact version was never fixed
    Version: Hash.HashString option
    /// Which storages (their names) contain the Version of the Artefact
    StoragesContainingVersion: string list
    }

and ArtefactVertex(id:ArtefactId) =    
    let mutable input : MethodVertex option = None
    let mutable outputs : Set<CommandLineVertex> = Set.empty
    let mutable isTracked = false
    let mutable storagesContainingActialHash = []
    let mutable actualHash : Hash.HashString option = None // None means that file/dir does not exist on disk
    
    member s.Id = id
    member s.ProducedBy
        with get() =
            match input with
            |   Some(v) -> v
            |   None -> invalidOp "The artefact vertex has no producer vertex set. Set the ProduceBy before retrieving its value"
        and set v = input <- Some(v)
    member s.UsedIn = outputs
    
    member s.RegisterAsInput consumingComputedVertex =
        outputs <- Set.add consumingComputedVertex outputs
    
    member s.IsTracked
        with get() = isTracked
        and set v = isTracked <- v
    
    /// The artefact hash calculated from the data on the disk
    member s.ActualHash
        with get() = actualHash
        and set v = actualHash <- v

    member s.StoragesContainingActualHash
        with get() = storagesContainingActialHash
        and set v = storagesContainingActialHash <- v
        
    interface System.IComparable with
        member s.CompareTo(other) =
            match other with
            | :? ArtefactVertex as otherArtefactVertex ->
                s.Id.ToString().CompareTo(otherArtefactVertex.Id.ToString())
            |  _ -> invalidArg "other" "System.IComaprable.CompareTo must be called on the object of the same types"

    override s.Equals(obj1:obj) =
        // this override prevennts usage of System.ICOmparable for equaliry checks
        Object.ReferenceEquals(s,obj1)
       
    override s.ToString() =
        let version =
            match s.ActualHash with
            |   None -> "not exist"
            |   Some(hash) -> hash.Substring(0,6)
        sprintf "Artefact(%s|%s)" (s.Id.ToString()) version
and MethodVertex =
    /// The vertex produces single artefact out of void
    |   Source of SourceVertex
    /// The vertex corresponds to invocation of single CLI command
    |   Command of CommandLineVertex
    with 
        member x.MethodId : MethodId =
            match x with
            | Source src -> src.MethodId
            | Command cmd -> cmd.MethodId

and SourceVertex(methodId: MethodId, artefact:VersionedArtefact) =
    let mutable artefact : VersionedArtefact = artefact    
    member s.Artefact
        with get() = artefact
        and set v = artefact <- v
    member s.MethodId = methodId

/// Represents a method defined as a command line.
and CommandLineVertex(methodId : MethodId) =
    let mutable inputs = new List<VersionedArtefact>()
    let mutable outputs = new List<VersionedArtefact>()
    let mutable command: string = String.Empty
    let mutable workingDirectory: string = String.Empty
    let mutable doNotClean = false
    let mutable commandExitCode: int option = None

    member s.MethodId = methodId    
    member s.Inputs
        with get() = inputs.AsReadOnly()
    member s.Outputs
        with get() = outputs.AsReadOnly()
    /// This command can be executed to get the up to date versions of outputs
    member s.Command
        with get() = command
        and set v = command <- v
    
    /// None if the command execution was not launched or not finished yet, other wise holds the execution exit code
    member s.ExitCode
        with get() = commandExitCode
        and set v = commandExitCode <- v
    /// From where the Command must be executed.
    /// Experiment root related
    member s.WorkingDirectory
        with get() = workingDirectory
        and set (v:string) =
            assert (v.EndsWith(Path.DirectorySeparatorChar) || v.EndsWith(Path.AltDirectorySeparatorChar))
            workingDirectory <- v
    member s.DoNotCleanOutputs
        with get() = doNotClean
        and set v = doNotClean <- v

    member s.AddInput input =
        inputs.Add(input)
    member s.AddOutput output =
        outputs.Add(output)

    member s.UpdateArtefacts(update: VersionedArtefact -> VersionedArtefact) =
        inputs <- new List<VersionedArtefact>(inputs |> Seq.map update)
        outputs <- new List<VersionedArtefact>(outputs |> Seq.map update)

    interface System.IComparable with
        member s.CompareTo(other) =
           let typedOther:CommandLineVertex = downcast other
           s.MethodId.CompareTo(typedOther.MethodId)

    override s.Equals(obj1:obj) =
        Object.ReferenceEquals(s,obj1)

let artefactToAlphFile (artefact:ArtefactVertex) (alphFileFullPath:string) (experimentRoot:string): AlphFile =
    if not (Path.IsPathRooted alphFileFullPath) then
        raise(ArgumentException(sprintf "artefactToAlphFile argument alphFileFullPath must contain rooted full path, but the following was received: %s" alphFileFullPath))

    let methodVertex = artefact.ProducedBy
    // Fills in Signature field in compute section object with correct value       
    match methodVertex with
    |   MethodVertex.Source(sourceVertex) ->
            let expectedVersion =
                match sourceVertex.Artefact.Version with
                |   Some(v) -> v
                |   None -> invalidOp "snapshoting artifact without particular version is prohibited"
            let snapshotSection = 
                {
                    Version = expectedVersion
                    Type = if isFullIDDirectory sourceVertex.Artefact.Artefact.Id then DirectoryArtefact else FileArtefact
                }
            {
                Origin = Snapshot snapshotSection
                IsTracked = sourceVertex.Artefact.Artefact.IsTracked
            }            
    |   MethodVertex.Command(computedVertex) ->
        // dependency graph contains all paths relative to project root
        // alpheus files contains all paths relative to alph file
        let fullIDtoRelative = AlphFiles.fullIDtoRelative experimentRoot alphFileFullPath
        
        let computeSection =
            let alphFileRelativeWorkingDir = 
                let alphFileDirFullPath = Path.GetDirectoryName(alphFileFullPath)
                let candidate =
                    Path.GetRelativePath(alphFileDirFullPath, Path.GetFullPath(Path.Combine(experimentRoot,computedVertex.WorkingDirectory)))
                if candidate = "" then ("."+Path.DirectorySeparatorChar.ToString()) else candidate
            let vesionOptionComverter version : Hash.HashString =
                match version with
                |   None -> "---NEVER-PRODUCED---"
                |   Some v -> v
            {
                Inputs = computedVertex.Inputs |> Seq.map (fun x -> {ID= fullIDtoRelative x.Artefact.Id ; Hash= vesionOptionComverter x.Version}) |> Array.ofSeq
                Outputs = computedVertex.Outputs |> Seq.map (fun x -> {ID= fullIDtoRelative x.Artefact.Id; Hash=vesionOptionComverter x.Version}) |> Array.ofSeq
                Command = computedVertex.Command                
                WorkingDirectory = alphFileRelativeWorkingDir
                Signature = String.Empty
                OutputsCleanDisabled = computedVertex.DoNotCleanOutputs
            }
        let computeSection = { computeSection with Signature = getSignature computeSection}
        {
            Origin = DataOrigin.Computed computeSection
            IsTracked = artefact.IsTracked
        }

/// Alpheus dependencies graph
type Graph() = 
    // todo: these collections are populated non-atomically and cause misleading
    let mutable methodVertices : Map<MethodId,MethodVertex> = Map.empty
    let mutable artefactVertices: Map<ArtefactId,ArtefactVertex> = Map.empty 
    
    let getMethodId (outputs: ArtefactId seq) = 
        outputs
        |> Seq.map(fun o -> o.ToString())
        |> Seq.sort
        |> Seq.head

    member s.Artefacts =
        Map.toSeq artefactVertices |> Seq.map (fun t -> let _,a = t in a) |> Array.ofSeq

    member s.Methods =
        Map.toSeq methodVertices |> Seq.map (fun t -> let _,m = t in m) |> Array.ofSeq

    member s.ArtefactsCount =
        Map.count artefactVertices

    member s.MethodsCount =
        Map.count methodVertices

    member s.GetOrAllocateArtefact fullID =
        match Map.tryFind fullID artefactVertices with
        |   Some(vertex) -> vertex
        |   None ->
            let vertex = ArtefactVertex(fullID)
            artefactVertices <- Map.add fullID vertex artefactVertices
            vertex
    
    /// Allocates an artefact if it is not allocated
    member s.AllocateSourceMethod (outputId: ArtefactId) =
        let methodId = getMethodId (seq{ yield outputId })
        if Map.containsKey methodId methodVertices then
            raise(InvalidOperationException(sprintf "attempt to allocate new source vertex \"%s\", but the vertex with this ID is already allocated" methodId))
        else
            let artefact = s.GetOrAllocateArtefact(outputId)
            let source = SourceVertex(methodId, {Artefact = artefact; Version = artefact.ActualHash; StoragesContainingVersion=[]})
            let vertex = Source(source)
            methodVertices <- Map.add methodId vertex methodVertices
            source
    
    member s.GetOrAllocateCommandLineMethod (methodId: MethodId) = // todo: shouldn't be public since allows to use any method id
        match Map.tryFind methodId methodVertices with
        |   Some(vertex) ->
            match vertex with
            |   Command(computed) -> computed
            |   Source(_) -> raise(InvalidOperationException("The vertex with the given id already exists and has different type"))
        |   None ->
            let vertex = CommandLineVertex methodId
            methodVertices <- Map.add methodId (Command vertex) methodVertices
            vertex

    member s.ConnectArtefactAsInput (verArtefact:VersionedArtefact) (method:CommandLineVertex) =
            verArtefact.Artefact.RegisterAsInput(method)
            method.AddInput(verArtefact)
    member s.ConnectArtefactAsOutput (verArtefact:VersionedArtefact) (method:MethodVertex) =
            match method with
            |   Command(computed) ->
                computed.AddOutput(verArtefact)
            |   Source(snapshot) ->
                snapshot.Artefact <- verArtefact
            verArtefact.Artefact.ProducedBy <- method

    
    /// Adds a single method vertex to the graph
    /// adding coresponding grpah connections
    member s.AddMethod (inputs: VersionedArtefact seq) (outputs: VersionedArtefact seq) =        
        let methodId = getMethodId (outputs |> Seq.map (fun o -> o.Artefact.Id))
        let methodVertex = s.GetOrAllocateCommandLineMethod methodId
        Seq.iter (fun x -> s.ConnectArtefactAsOutput x (Command methodVertex)) outputs
        Seq.iter (fun x -> s.ConnectArtefactAsInput x methodVertex) inputs
        methodVertex
            
    /// Takes a list of outputs and builds (recreates using .alph files) a dependency graph
    /// Returns all found dependences inc. original "outputs" vertices
    member s.LoadDependenciesAsync (outputs:ArtefactVertex list) (experimentRootPath: string)=
        async {
            let mutable processedOutputs : Set<ArtefactVertex> = Set.empty        
        
            let queue = Queue<ArtefactVertex>()
            // queuing initial outputs            
            List.iter (fun x -> queue.Enqueue x) outputs
            while queue.Count > 0 do
                // Processing the graph artefact vertex
                // to process a vertex A means 1) to allocate verteces for direct A's producer method and it's direct inputs; 2) connect them 3) Enqueue them to be processed
                let dequeuedArtefact = queue.Dequeue()
                if not (Set.contains dequeuedArtefact processedOutputs) then
                    let fullOutputPath = dequeuedArtefact.Id.GetFullPath(experimentRootPath)
                    let alphFileFullPath = artefactPathToAlphFilePath fullOutputPath
                   
                    let getFullID = relIDtoFullID experimentRootPath alphFileFullPath
                    let! alphFileLoadResult = tryLoadAsync alphFileFullPath                    
                    match alphFileLoadResult with
                    |   None -> 
                        // Absence of .alph file means that the artefact is initial (not produced)
                        // Thus it corresponds to a source vertex (no inputs)
                        // 1) allocating a method vertex
                        let vertex = s.AllocateSourceMethod(dequeuedArtefact.Id)
                        // as there no alph file. We must create it now and fix current disk data version in it
                        // so calculating actual disk data version
                        // writing in expected versions
                        let! calculatedVersion = Hash.fastHashPathAsync fullOutputPath
                        let versionedArtefact =
                            match calculatedVersion with
                            |   None -> 
                                { vertex.Artefact with Version = None }
                                //raise(InvalidDataException(sprintf "The artefact %s does not exist on disk" (AlphFiles.fullIDtoString dequeuedArtefact.FullID)))
                            |   Some(version) ->
                                { vertex.Artefact with Version=Some version }
                        vertex.Artefact <- versionedArtefact
                        // 2a) connect the method vertex to the outputs
                        s.ConnectArtefactAsOutput versionedArtefact (Source vertex)

                        // Dumping alph file to disk
                        // let alphFile = artefactToAlphFile dequeuedArtefact alphFileFullPath experimentRootPath
                        // do! AlphFiles.saveAsync alphFile alphFileFullPath 
                    |   Some(alphFile) ->
                        // Alph file exists
                        dequeuedArtefact.IsTracked <- alphFile.IsTracked
                        match alphFile.Origin with
                        |   DataOrigin.Snapshot(snapshot) ->
                            // Snapshot in .alph file means that is was snapshoted, thus Tracked
                            // 1) allocation a method vertex
                            let vertex = s.AllocateSourceMethod(dequeuedArtefact.Id)

                            match snapshot.Type with
                            |   FileArtefact -> assert(not (isFullIDDirectory dequeuedArtefact.Id))
                            |   DirectoryArtefact -> assert(isFullIDDirectory dequeuedArtefact.Id)

                            // setting up expected version
                            let artefact = { vertex.Artefact with Version=Some snapshot.Version }
                            vertex.Artefact <- artefact
                            // 2a) connect the method vertex to the outputs
                            s.ConnectArtefactAsOutput artefact (Source vertex)
                        |   DataOrigin.Computed(computeSection) ->
                            // produced by some method.

                            // checking weather the internals were modified (weather the output hashes mentioned are valid)
                            let computeSection = checkSignature(computeSection)
                                                        
                            // 1) allocation a method vertex
                            let allOutputsIDs = Array.map (fun x -> x.ID) computeSection.Outputs
                            let allOutputFullIDs = Array.map getFullID allOutputsIDs
                            let methodId = getMethodId allOutputFullIDs
                            let methodVertex = s.GetOrAllocateCommandLineMethod methodId
                            methodVertex.DoNotCleanOutputs <- computeSection.OutputsCleanDisabled
                            methodVertex.Command <- computeSection.Command
                            
                            let expRootRelatedWorkingDir =
                                let alphFileDir = Path.GetDirectoryName(alphFileFullPath)
                                let path = Path.GetRelativePath(experimentRootPath,Path.GetFullPath(Path.Combine(alphFileDir,computeSection.WorkingDirectory)))
                                if path.EndsWith(Path.DirectorySeparatorChar) then path else path+Path.DirectorySeparatorChar.ToString()

                            methodVertex.WorkingDirectory <- expRootRelatedWorkingDir

                            //outputs match check
                            // writing in expected versions
                            let allOutputsHashes = Array.map (fun x -> Some x.Hash) computeSection.Outputs 
                            let allOutputVertices = Array.map s.GetOrAllocateArtefact allOutputFullIDs  
                            let versionedOutputs =
                                Array.map2
                                    (fun (v:ArtefactVertex) hash -> {Artefact=v; Version=hash; StoragesContainingVersion = []})
                                    allOutputVertices allOutputsHashes |> Array.sortBy (fun x -> x.Artefact.Id)
                            let versionedOutputSet = Set.ofArray versionedOutputs
                            if methodVertex.Outputs.Count > 0 then
                                if methodVertex.Outputs |> Set.ofSeq <> versionedOutputSet then
                                    raise(InvalidOperationException(sprintf "different methods produce the same output artefact in the graph: 1) %A 2) %A" methodVertex.Outputs allOutputFullIDs))                                
                            else
                                // 2a) connect the method vertex to the outputs
                                Array.iter (fun a -> s.ConnectArtefactAsOutput a (Command methodVertex)) versionedOutputs                                
                            // 2b) connect inputs to the vertex
                            if methodVertex.Inputs.Count = 0 then
                                let allInputsIDs = Array.map (fun x -> x.ID) computeSection.Inputs
                                let allInputsFullIDs = Array.map getFullID allInputsIDs
                                let allInputVertices = Array.map s.GetOrAllocateArtefact allInputsFullIDs

                                //writing it hashes (versions) from alph file
                                let allInputHashes = Array.map (fun x -> Some x.Hash) computeSection.Inputs
                                let versionedInputs = Array.map2 (fun (v:ArtefactVertex) hash -> {Artefact=v; Version=hash; StoragesContainingVersion = []}) allInputVertices allInputHashes

                                Array.iter (fun (x:VersionedArtefact) -> s.ConnectArtefactAsInput x methodVertex) versionedInputs
                                // 3) enqueue to be processed
                                Array.iter (fun (x:ArtefactVertex) -> queue.Enqueue x) allInputVertices                        
                    processedOutputs <- Set.add dequeuedArtefact processedOutputs
            return processedOutputs
        }

let fillinActualHashesAsync (artefacts:ArtefactVertex seq) (experimentRoot: string) =
    async {
        let fullPaths = Seq.map (fun (a:ArtefactVertex) -> fullIDtoFullPath experimentRoot a.Id) artefacts
        let asyncs = Seq.map Hash.fastHashPathAsync fullPaths |> Array.ofSeq
        let! hashes = Async.Parallel asyncs
        Seq.iter2 (fun hash (art:ArtefactVertex) -> art.ActualHash <- hash ) hashes artefacts
    }

/// Fulls up the StoragesContainingActialHash of the artefacts
let fillinArtefactContainingStoragesAsync (artefacts:ArtefactVertex seq) (getContainingStorageNames: (Hash.HashString option array -> Async<(string list) array>)) =
    async {
        // we fill in only artefacts that are on disk
        let artefactsArray = Seq.filter (fun (art:ArtefactVertex) -> art.ActualHash.IsSome ) artefacts |> Array.ofSeq

        let artefactVersions = artefactsArray |> Array.map (fun (art:ArtefactVertex) -> art.ActualHash)
        let! containigStorages = getContainingStorageNames artefactVersions
        Array.iter2 (fun (art:ArtefactVertex) storages -> art.StoragesContainingActualHash <- storages ) artefactsArray containigStorages
    }

/// fills up Inputs and Outputs of methods with the information about the storages that contain the mentioned versions
let fillinMethodEdgeContainingStoragesAsync (methods:MethodVertex seq) (getContainingStorageNames: ((Hash.HashString option) array -> Async<(string list) array>)) =
    async {
        let methodsArray = Array.ofSeq methods
        // gathering versions
        
        let extractExpectedVersions method =
            match method with
            |   Source(sourceVertex) -> seq { yield sourceVertex.Artefact}
            |   Command(computedVertex) ->
                seq {
                    yield! computedVertex.Inputs;
                    yield! computedVertex.Outputs
                }
        let allVersions = methodsArray |> Seq.collect extractExpectedVersions |> Seq.map (fun art -> art.Version) |> Array.ofSeq
        let! containingStorages = getContainingStorageNames allVersions
        let storagesMap = Seq.zip allVersions containingStorages |> Map.ofSeq
        
        let updateVersionedArtefact (art:VersionedArtefact) =
            { art with StoragesContainingVersion = Map.find art.Version storagesMap }
        let iterator vertex =
            match vertex with
            |   Source(sourceVertex) -> sourceVertex.Artefact <- updateVersionedArtefact sourceVertex.Artefact
            |   Command(computedVertex) -> computedVertex.UpdateArtefacts updateVersionedArtefact
        methodsArray |> Array.iter iterator      
    }
    

/// gets versioned variant by fixing actual artefact hash
let getVersionedArtefact (a: ArtefactVertex) : VersionedArtefact =
    {
        Artefact = a
        Version = a.ActualHash
        StoragesContainingVersion = []
    }