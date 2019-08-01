module ItisLab.Alpheus.StorageLocal

open System.IO
open ItisLab.Alpheus.Storage
open System

type Storage(absPath:string) =
    interface IStorage with
        member s.IsInStorageAsync (version: HashString) =
            async {                
                if String.IsNullOrEmpty version then
                    return ArtefactType.Absent
                else
                    let archivePath = Path.Combine(absPath,version)
                    if File.Exists (archivePath+".file") then
                        return ArtefactType.SingleFile
                    elif File.Exists (archivePath+".dir-0") then
                        return ArtefactType.Directory
                    else
                        return ArtefactType.Absent
            }
        
        member s.getFileSaveStreamAsync version =
            async {                
                let archivePath = Path.Combine(absPath,version)+".file"
                return (File.Create archivePath :> Stream)
            }
        
        member s.getDirSaveStreamsAsync version streamsCount =
            async {
                let archivePath = Path.Combine(absPath,version)+".dir"
                let names = Array.init streamsCount (fun idx -> sprintf "%s-%d" archivePath idx)
                let streams = names |> Array.map (fun fullPath -> File.Create fullPath :> Stream)
                return streams
            }

        member s.getFileRestoreStreamAsync version =
            async {                
                let archivePath = Path.Combine(absPath,version) + ".file"                      
                return File.OpenRead archivePath :> Stream
            }

        member s.getDirRestoreStreamsAsync version =
            async {
                let prefix = Path.Combine(absPath,version)+".dir"
                let filename idx =
                    sprintf "%s-%d" prefix idx
                let names =
                    Seq.initInfinite filename
                    |> Seq.takeWhile (fun name -> File.Exists name)
                let streams = names |> Seq.map (fun name -> File.OpenRead name :> Stream) |> Array.ofSeq
                return streams
            }