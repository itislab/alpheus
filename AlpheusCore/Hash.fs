module ItisLab.Alpheus.Hash

open System.IO
open System
open System.Text

type HashString = string
        
let hashToString (data : byte array) : HashString = System.BitConverter.ToString(data).Replace("-", System.String.Empty)

let createHashAlgorithm() : System.Security.Cryptography.HashAlgorithm =
    upcast System.Security.Cryptography.SHA512.Create()

/// Hashes the content of the stream by reading it by chunks of supplied size till the end of the stream.
/// Stream must have readable Length property
let hashStreamAsync chunkSizeBytes (stream:Stream) =    
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
let hashFileAsync fullPath =
    async {
        let readChunkSize = 100 * 1024 * 1024 //in Bytes
        use f_stream = File.OpenRead fullPath
        let! hash = hashStreamAsync readChunkSize f_stream
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

let rec hashDirectoryAsync (fullPath:string) =
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
            // And standalong files (also referenced by alpheus and having .alph files) within this directory should not influence the hash of the parent dir
            |> Array.sort
        let! fileAttributes = Array.map getFileAttributesAsync fileNamesAbs |> Async.Parallel

        let fileNamesAbs = filterOutHidden fileNamesAbs fileAttributes
        let fileNamesRel =
            fileNamesAbs
            |> Array.map (fun path -> path.Remove(0,fullPath.Length)) // removing abs path part
        let fileHashComputation = Seq.map (fun filename -> hashFileAsync filename) fileNamesAbs //full paths here   
        let fileNameBytes = fileNamesRel |> Array.collect (fun x -> Encoding.UTF8.GetBytes(x)) //but relative paths in names

        let dirNamesAbs =
            Directory.GetDirectories fullPath
            |> Array.sort
        let dirNames = //relative names
            dirNamesAbs 
            |> Array.map (fun x -> x.Remove(0,fullPath.Length))
        let dirHashComputations = Seq.map (fun dirname -> hashDirectoryAsync dirname) dirNamesAbs //full paths are passed here
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


let hashPathAsync fullPath =
    async {
            Logger.logVerbose Logger.ExperimentFolder (sprintf "Hashing %s ..." fullPath)
            if File.Exists fullPath then
                let! hash = hashFileAsync fullPath
                return Some(hashToString hash)
            elif Directory.Exists fullPath then
                let! hash = hashDirectoryAsync fullPath
                return Some(hashToString hash)
            else
                return None
    }
    
/// optimization that caches the computed hashes into *.hash files
let fastHashPathAsync (fullPath:string) =
    let hashFilePath =
        let prefix =
            if fullPath.EndsWith(Path.DirectorySeparatorChar) then
                fullPath.Substring(0,fullPath.Length-1)
            else fullPath
        sprintf "%s.hash" prefix
    let hashAndSave() =            
        async {
            let! hashStr = hashPathAsync fullPath
            match hashStr with
            |   None -> return None
            |   Some(hashStr) ->
                Logger.logVerbose Logger.ExperimentFolder (sprintf "writing %s " hashFilePath)
                do! File.WriteAllTextAsync(hashFilePath, hashStr) |> Async.AwaitTask
                Logger.logVerbose Logger.ExperimentFolder (sprintf "%s written successfully" hashFilePath)
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
        if File.Exists hashFilePath then
            Logger.logVerbose Logger.ExperimentFolder (sprintf "%s hash file exists" hashFilePath)
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
            Logger.logVerbose Logger.ExperimentFolder (sprintf "%s artefact modification time extracted" fullPath)
            match dataTime with
            |   None -> //data not exists
                Logger.logVerbose Logger.ExperimentFolder (sprintf "deleteing %s hash file as the artefact is absent" hashFilePath)
                File.Delete hashFilePath
                Logger.logVerbose Logger.ExperimentFolder (sprintf "sucessfuly deleted %s hash file" hashFilePath)
                return None
            |   Some(dataWriteTime) ->                               
                    if precomHashTime > dataWriteTime then // considered up to date
                        let! hashStr = File.ReadAllTextAsync(hashFilePath) |> Async.AwaitTask
                        let hashStr2 : HashString = hashStr.Trim()
                        return Some(hashStr2)
                    else
                        return! hashAndSave()                
        else
            return! hashAndSave()
    }
