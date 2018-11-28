module ItisLab.Alpheus.Storage

open System.IO

type ArtefactType =
    |   SingleFile
    |   Directory
    |   Absent

type IStorage =
    interface
        abstract IsInStorageAsync: version:Hash.HashString -> Async<ArtefactType>        
        abstract getFileSaveStreamAsync: version:Hash.HashString -> Async<Stream>
        abstract getDirSaveStreamsAsync: version:Hash.HashString -> streamsCount:int -> Async<Stream array>
        
        abstract getFileRestoreStreamAsync: version:Hash.HashString -> Async<Stream>
        abstract getDirRestoreStreamsAsync: version:Hash.HashString -> Async<Stream array>
    end


