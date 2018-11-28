module ItisLab.Alpheus.Hash

open System.Security.Cryptography
open System.IO
open System
open System.Text

type HashString = string
        
let hashToString (data : byte array) : HashString = System.BitConverter.ToString(data).Replace("-", System.String.Empty)

let hashFileAsync fullPath =
    let readChunkSize = 100 * 1024 * 1024 //in Bytes
    async {
        use sha = SHA1.Create()
        // filename is not accounted for now, as well as attributes. Only file contents
        // let pathBytes = fname |> System.Text.Encoding.UTF8.GetBytes
        // sha.TransformBlock(pathBytes, 0, pathBytes.Length, pathBytes, 0) |> ignore
        use f_stream = File.OpenRead fullPath
        let bytesCount = f_stream.Length
        let fullReads = (int)(bytesCount / (int64)readChunkSize)
        let partialBlockSize = (int)(bytesCount % (int64)readChunkSize)
        //TODO: check chunked hashing
        for i in 0..(fullReads-1) do                
            let! contentBytes = f_stream.AsyncRead readChunkSize
            sha.TransformBlock(contentBytes,0,readChunkSize,contentBytes,0) |> ignore
        let finalBytes = Array.zeroCreate<byte> partialBlockSize
        if partialBlockSize > 0 then
            let _ = f_stream.AsyncRead(finalBytes,0,partialBlockSize)                            
            ()
        sha.TransformFinalBlock(finalBytes,0,partialBlockSize) |> ignore
        return sha.Hash
    }

let rec hashDirectoryAsync (fullPath:string) =
    async {
        let fullPath =
            if (fullPath.EndsWith(Path.DirectorySeparatorChar)) || (fullPath.EndsWith(Path.AltDirectorySeparatorChar)) then
                fullPath
            else
                fullPath + Path.DirectorySeparatorChar.ToString()
        let fileNamesAbs =
            Directory.GetFiles fullPath            
            |> Array.filter (fun name -> not(name.EndsWith(".hash"))) // ignoring hash file
            |> Array.sort
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

        use sha = SHA1.Create()
        sha.TransformBlock(allNames, 0,allNames.Length,allNames,0) |> ignore
        sha.TransformFinalBlock(flattenHashes,0,flattenHashes.Length) |> ignore
        return sha.Hash
    }


let hashDataAsync fullPath =
    async {                
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
let fastHashDataAsync fullPath =
    let hashFilePath = sprintf "%s.hash" fullPath
    let hashAndSave() =            
        async {
            let! hashStr = hashDataAsync fullPath
            match hashStr with
            |   None -> return None
            |   Some(hashStr) ->
                do! File.WriteAllTextAsync(hashFilePath, hashStr) |> Async.AwaitTask      
                // File.SetAttributes(hashFilePath, FileAttributes.Hidden)
                return Some(hashStr)
        }
    let rec getDirLastWriteTimeDeepUTC dirpath = 
        let curDirtime = Directory.GetLastWriteTimeUtc(dirpath)
        let subDirs = Directory.GetDirectories(dirpath) 
        let subFiles = Directory.GetFiles(dirpath) |> Array.filter (fun name -> not(name.EndsWith(".hash")))
        let subDirTimes = Array.map getDirLastWriteTimeDeepUTC subDirs
        let subFilesTimes = Array.map File.GetLastWriteTimeUtc subFiles
        try 
            let maxSubTimes = Seq.append subFilesTimes subDirTimes |> Seq.max
            max curDirtime maxSubTimes
        with            
            | :? ArgumentException -> curDirtime // empty dir
    async {        
        if File.Exists hashFilePath then
            let precomHashTime = File.GetLastWriteTimeUtc hashFilePath
            let dataTime =
                if File.Exists fullPath then
                    Some(File.GetLastWriteTimeUtc fullPath)
                elif Directory.Exists fullPath then
                    Some(getDirLastWriteTimeDeepUTC fullPath)
                else
                    None
            match dataTime with
            |   None -> //data not exists
                File.Delete hashFilePath
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
