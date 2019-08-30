namespace ItisLab.Alpheus

// This file contains basic type definitions for the Alpheus experiment representation.


/// Contains a path relative to the experiment root.
/// Trailing slash indicates that the artifact is folder
type ExperimentRelativePath = string

/// Contains a path relative to an .alph file.
/// Trailing slash indicates that the artifact is folder
type AlphRelativePath = string

/// Path relative to the project root (directory delimiter is always '/' even on windows).
/// Trailing slash indicates that the artefact is folder.
/// For scalar artefacts, it is just a path, while for vectors it contains one or more '*' 
/// and therefore cannot be resolved into a single file path.
type ArtefactId = 
    /// A path an artefact which is relative to the experiment root.
    /// Note that a path of a vector artefact includes asterisks (*) denoting any folder/file.
    Path of ExperimentRelativePath with
    override x.ToString() = match x with Path path -> path

type MethodId = string

type HashString = string


