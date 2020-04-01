namespace ItisLab.Alpheus

open System
open System.IO

type ComputationContext =
    { ExperimentRoot: string
      Print: string -> unit
      Index: string list
    } with
    /// Makes an absolute path for the given path relative to the experiment root.
    member x.GetAbsolutePath (relativePath:string) = 
        // Note for Path.Combine: if an argument other than the first contains a rooted path, 
        // any previous path components are ignored, and the returned string begins with that rooted path component.
        if Path.IsPathRooted(relativePath) then invalidArg "relativePath" "The given path is absolute"
        else Path.GetFullPath(Path.Combine(x.ExperimentRoot, relativePath))
