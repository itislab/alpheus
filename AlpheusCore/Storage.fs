module ItisLab.Alpheus.Storage

open System.IO

type ArtefactType =
    |   SingleFile
    |   Directory
    |   Absent

type IStorage =
    interface
        abstract IsInStorageAsync: version:HashString -> Async<ArtefactType>        
        abstract getFileSaveStreamAsync: version:HashString -> Async<Stream>
        abstract getDirSaveStreamsAsync: version:HashString -> streamsCount:int -> Async<Stream array>
        
        abstract getFileRestoreStreamAsync: version:HashString -> Async<Stream>
        abstract getDirRestoreStreamsAsync: version:HashString -> Async<Stream array>
    end


