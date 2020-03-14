module ItisLab.Alpheus.Hash

open System.IO
open System
open System.Text
open ItisLab.Alpheus
open AlphFiles
open Angara.Data

        
let hashToString (data : byte array) : HashString = System.BitConverter.ToString(data).Replace("-", System.String.Empty)

let createHashAlgorithm() : System.Security.Cryptography.HashAlgorithm =
    upcast System.Security.Cryptography.SHA512.Create()

/// Hashes the content of the stream by reading it by chunks of supplied size till the end of the stream.
/// Stream must have readable Length property
let hashStream chunkSizeBytes (stream:Stream) =    
    async {
        use hashAlg = createHashAlgorithm()
        
        let bytesCount = stream.Length
        let fullReads = (int)(bytesCount / (int64)chunkSizeBytes)
        let partialBlockSize = (int)(bytesCount % (int64)chunkSizeBytes)
        for i in 0..(fullReads-1) do                
            let! contentBytes = stream.AsyncRead chunkSizeBytes
            let readBytes = hashAlg.TransformBlock(contentBytes,0,chunkSizeBytes,contentBytes,0)
            assert(readBytes = chunkSizeBytes)
        if fullReads = 0 then
            // according to documentation:
            //   "You must call the TransformBlock method before calling the TransformFinalBlock method.
            //    You must call both methods before you retrieve the final hash value. "
            let dummy = Array.zeroCreate<byte> 0
            hashAlg.TransformBlock(dummy,0,0,dummy,0) |> ignore
        let finalBytes = Array.zeroCreate<byte> partialBlockSize
        if partialBlockSize > 0 then
            let! _ = stream.AsyncRead(finalBytes,0,partialBlockSize)                            
            ()
        let finalRead = hashAlg.TransformFinalBlock(finalBytes,0,partialBlockSize).Length
        assert(finalRead = partialBlockSize)
        return hashAlg.Hash
    }

/// Hashes the content of the file supplied
/// Filename as well as attributes are ignored
let hashFile fullPath =
    async {
        let readChunkSize = 100 * 1024 * 1024 //in Bytes
        use f_stream = File.OpenRead fullPath
        let! hash = hashStream readChunkSize f_stream
        f_stream.Dispose()
        return hash
    }

/// Wraps File.GetAttributes into async
let private getFileAttributesAsync filename = async { return File.GetAttributes filename }


/// filter the hidden files among filenames which are described by corresponding fileAttributes.
/// Returns the file name of the non-hidden files
let filterOutHidden filenames fileAttributes = 
    let zippedFile = Array.zip filenames fileAttributes

    /// chooses non-hidden file(directory) names
    let regularChooser file_pair =
        let name,(attrs:FileAttributes) = file_pair
        if (FileAttributes.Hidden &&& attrs) = FileAttributes.Hidden then
            None
        else
            Some(name)

    Array.choose regularChooser zippedFile // omitting hidden files

let rec hashDirectory (fullPath:string) =
    async {
        let fullPath =
            if (fullPath.EndsWith(Path.DirectorySeparatorChar)) || (fullPath.EndsWith(Path.AltDirectorySeparatorChar)) then
                fullPath
            else
                fullPath + Path.DirectorySeparatorChar.ToString()
        let fileNamesAbs =
            Directory.GetFiles fullPath            
            |> Array.filter (fun name -> not(name.EndsWith(".hash") || name.EndsWith(".alph"))) // ignoring hash file and .alph files
            // Assuming that if the code traverses .alph file, it means that the parent directory is being referenced by alpheus
            // And standalone files (also referenced by alpheus and having .alph files) within this directory should not influence the hash of the parent dir
            |> Array.sort
        let! fileAttributes = Array.map getFileAttributesAsync fileNamesAbs |> Async.Parallel

        let fileNamesAbs = filterOutHidden fileNamesAbs fileAttributes
        let fileNamesRel =
            fileNamesAbs
            |> Array.map (fun path -> path.Remove(0,fullPath.Length)) // removing abs path part
        let fileHashComputation = Seq.map (fun filename -> hashFile filename) fileNamesAbs //full paths here   
        let fileNameBytes = fileNamesRel |> Array.collect (fun x -> Encoding.UTF8.GetBytes(x)) //but relative paths in names

        let dirNamesAbs =
            Directory.GetDirectories fullPath
            |> Array.sort
        let dirNames = //relative names
            dirNamesAbs 
            |> Array.map (fun x -> x.Remove(0,fullPath.Length))
        let dirHashComputations = Seq.map (fun dirname -> hashDirectory dirname) dirNamesAbs //full paths are passed here
        let dirNameBytes = dirNames |> Array.collect (fun x -> Encoding.UTF8.GetBytes(x)) // but the relative names are hashed

        let! allHashes = Seq.concat [fileHashComputation; dirHashComputations ] |> Array.ofSeq |> Async.Parallel
        let allNames = Array.append fileNameBytes dirNameBytes
        let flattenHashes = Seq.collect (fun x -> x) allHashes |> Array.ofSeq

        use hashAlg = createHashAlgorithm()
        let namesTransformedBytes = hashAlg.TransformBlock(allNames, 0,allNames.Length,allNames,0)
        assert(namesTransformedBytes = allNames.Length)
        let finalBytes = hashAlg.TransformFinalBlock(flattenHashes,0,flattenHashes.Length)
        assert(finalBytes.Length = flattenHashes.Length)
        return hashAlg.Hash
    }

/// Builds hash for the given path which can be either a file or a directory 
/// (depends on what exists). If neither exists, returns None.
let hashPath fullPath =
    async {
            // todo: let items = PathUtils.enumeratePath fullPath
            if File.Exists fullPath then
                let ct = Logger.logVerboseLongRunningStart Logger.Hash (sprintf "Hashing file %s ..." fullPath)
                let! hash = hashFile fullPath
                Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "Calculated file %s hash" fullPath)
                return Some(hashToString hash)
            elif Directory.Exists fullPath then
                let ct = Logger.logVerboseLongRunningStart Logger.Hash (sprintf "Hashing directory %s ..." fullPath)
                let! hash = hashDirectory fullPath
                Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "Calculated directory %s hash" fullPath)
                return Some(hashToString hash)
            else
                return None
    }

/// Builds hash for the given path which can be either a file or a directory     
/// optimization that caches the computed hashes into *.hash files
let hashPathAndSave (fullPath:string) =
    let hashFilePath = PathUtils.pathToHashFile fullPath
    let hashAndSave() =            
        async {
            let! hashStr = hashPath fullPath
            match hashStr with
            |   None -> return None
            |   Some(hashStr) ->
                // Logger.logVerbose Logger.Hash (sprintf "writing %s " hashFilePath)
                do! File.WriteAllTextAsync(hashFilePath, hashStr) |> Async.AwaitTask
                // Logger.logVerbose Logger.Hash (sprintf "%s written successfully" hashFilePath)
                return Some(hashStr)
        }
    let getFileLastWriteTimeUTCAsync filepath =
        async { return File.GetLastWriteTimeUtc filepath }

    let rec getDirLastWriteTimeDeepUTCAsync dirpath = 
        async {
            let curDirtime = Directory.GetLastWriteTimeUtc(dirpath)
            let subDirs = Directory.GetDirectories(dirpath) 
            let subFiles = Directory.GetFiles(dirpath) |> Array.filter (fun name -> not(name.EndsWith(".hash")))
            let! fileAttributes = Array.map getFileAttributesAsync subFiles |> Async.Parallel
            let subFiles = filterOutHidden subFiles fileAttributes // we do not account hidden files
            let subDirTimesAsyncs = Array.map getDirLastWriteTimeDeepUTCAsync subDirs
            let subFilesTimesTasks = Array.map (fun x -> getFileLastWriteTimeUTCAsync x |> Async.StartAsTask ) subFiles
            let subFilesTimesAsyncs = Array.map Async.AwaitTask subFilesTimesTasks
            if subDirTimesAsyncs.Length > 0 || subFilesTimesAsyncs.Length > 0 then
                let! subTimes = Array.append subFilesTimesAsyncs subDirTimesAsyncs |> Async.Parallel
                let maxSubTimes = Array.max subTimes
                return max curDirtime maxSubTimes
            else // empty directory
                return curDirtime
        }

    async { 
        let ct = Logger.logVerboseLongRunningStart Logger.Hash (sprintf "Artefact %s - acquiring data hash..." fullPath)
        if File.Exists hashFilePath then
            // Logger.logVerbose Logger.Hash (sprintf "%s hash file exists" hashFilePath)
            let modTimeCt = Logger.logVerboseLongRunningStart Logger.Hash (sprintf "Artefact %s - acquiring files modification time" fullPath)
            let precomHashTime = File.GetLastWriteTimeUtc hashFilePath
            let! dataTime =
                async {
                    if File.Exists fullPath then
                        return Some(File.GetLastWriteTimeUtc fullPath)
                    elif Directory.Exists fullPath then
                        let! lastWriteTimeUTC = getDirLastWriteTimeDeepUTCAsync fullPath
                        return Some(lastWriteTimeUTC)
                    else
                        return None
                }
            Logger.logVerboseLongRunningFinish modTimeCt Logger.Hash (sprintf "Artefact %s - modification time extracted" fullPath)
            match dataTime with
            |   None -> //data not exists
                Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "The artefact is absent. Deleting stale %s hash file" hashFilePath)
                File.Delete hashFilePath
                // Logger.logVerbose Logger.Hash (sprintf "sucessfuly deleted %s hash file" hashFilePath)
                return None
            |   Some(dataWriteTime) ->                               
                    if precomHashTime > dataWriteTime then // considered up to date
                        let! hashStr = File.ReadAllTextAsync(hashFilePath) |> Async.AwaitTask
                        let hashStr2 : HashString = hashStr.Trim()
                        Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "Artefact %s hash is extracted from up-to-date .hash file" fullPath)
                        return Some(hashStr2)
                    else
                        let! res =  hashAndSave()
                        Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "Artefact %s hash is recalculated due to data change since the .hash file was written" fullPath)
                        return res
        else
            let! res = hashAndSave()
            Logger.logVerboseLongRunningFinish ct Logger.Hash (sprintf "Artefact %s hash is calculated (didn't find precomputed .hash file)" fullPath)
            return res
    }

/// The give path can contain patterns and is resolved into the actual list of files so for each of the files
/// the hash is computed and saved into a file.
let hashVectorPathAndSave (fullPath:string) =
    async {
        try
            let resolvedFullPaths = PathUtils.enumeratePath fullPath
            return! resolvedFullPaths |> Utils.mapAsync (fun (_, path) -> hashPathAndSave path)
        with
            | :? System.IO.DirectoryNotFoundException -> return MdMap.Empty
    }


/// Computes signature of supplied computeSection (Signature and isTracked member inside this computeSection is ignored during hash calculation)
let getSignature (computeSection:CommandOutput) =
    use sha = System.Security.Cryptography.SHA1.Create()
    let addHash (str:string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(str)
        sha.TransformBlock(bytes,0,bytes.Length,bytes,0) |> ignore
    addHash computeSection.Command 
    addHash computeSection.WorkingDirectory
    let hashArtefact art =
        addHash art.RelativePath
        art.Hash |> MdMap.toSeq |> Seq.map(snd >> Option.map addHash) |> ignore
    Seq.iter hashArtefact (Seq.append computeSection.Inputs computeSection.Outputs)
    sha.TransformFinalBlock(Array.zeroCreate<byte> 0,0,0) |> ignore
    hashToString sha.Hash

/// Computes the actual signature and compares with the expected.
/// If they are different, the hash invalidates in the computeSection.
let validateSignature (computeSection:CommandOutput) =
    let readSignature = computeSection.Signature
    let actualSignature = getSignature computeSection
    if readSignature = actualSignature then
        computeSection
    else
        let invalidate (version:ArtefactVersion) : ArtefactVersion = version |> MdMap.map(fun _ -> None)
        // wiping out result hashes
        {
            computeSection with
                Outputs = computeSection.Outputs |> List.map (fun output -> {output with Hash = invalidate output.Hash}) 
        }