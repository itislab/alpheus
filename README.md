# Alpheus

| Platform        | Build           |
| ------------- |:-------------:|
| Windows   | [![VS2019 Build Status](https://ci.appveyor.com/api/projects/status/2q24972vyycmjdxx/branch/master?svg=true)](https://ci.appveyor.com/project/dgrechka/alpheus) |
| Linux     | [![Linux Build Status](https://drone.k8s.grechka.family/api/badges/itislab/alpheus/status.svg)](https://drone.k8s.grechka.family/itislab/alpheus)  |

[![codecov](https://codecov.io/gh/itislab/alpheus/branch/master/graph/badge.svg)](https://codecov.io/gh/itislab/alpheus)
[![Tests](https://img.shields.io/appveyor/tests/dgrechka/alpheus/master)](https://ci.appveyor.com/project/dgrechka/alpheus/build/tests)
![NuGet](https://img.shields.io/nuget/v/Alpheus-cli.svg)
[![NuGet](https://img.shields.io/nuget/dt/Alpheus-cli.svg)](https://www.nuget.org/packages/Alpheus-cli/)


A tool for organizing and managing computational experiments. Reproducible research is a way of conducting research allowing to get a provenance of any result and be able to compute it again. Alpheus forces a researcher to follow this way.

Advantages:
_todo: complete the list_

- Builds a dependency graph of operations.
- Incrementally computes only that data which were affected by a change.
- Allows reproducing the data.
- Allows using your usual tools and is based on folders/files.

## Why Alpheus?
Alpheus uses full power of .Net Core threadpool processing, asyncronous IO and asyncronous computations. You will be surprised how effective Alpheus deals with large collection of small files saving them to artefact storages and restoring them back when needed.

## Installation
You need to have [.Net Core SDK 3.1 or newer](https://dotnet.microsoft.com/learn/dotnet/hello-world-tutorial) installed.

If you have it install the latest version of Alpheus with

```
dotnet tool install --global Alpheus-cli
```

## Usage

You can call `alpheus` in the command line

```
C:\project1>alpheus help
USAGE: alpheus [help] [<subcommand> [<options>]]

SUBCOMMANDS:

    init <options>        Make the current directory an Alpheus experiment directory
    config <options>      Modify configuration of research directory
    build <options>       Creates an experiment graph node
    compute <options>     Tries to compute the graph to make the outdated node up to date
    status <options>      Prints the graph status for particular .alph file
    save <options>        Save a copy of file/directory to the storage(s)
    restore <options>     Restore a copy of the file/directory from storage

    Use 'alpheus <subcommand> help' for additional information.

OPTIONS:

    help                  display this list of options.
```

## Build

Before building the code, you need to make sure the machine has the following tools installed:

1. [.Net Core SDK 3.1 or newer](https://dotnet.microsoft.com/download)


Clone the repository and run the following command in the root of the repository:

```
dotnet build
```

You can also open Alpheus.sln using Visual Studio 2019 (or newer) and build the solution.

## Tests

Run the following command in the root of the repository:

```
dotnet test
```

## Documentation

### Initialization

In the root folder of the experiment run the following command:

```
alpheus init
```

This creates new folder `.alpheus` with default settings described in `.alpheus/config.json`. This folder should be committed to the git repository.

Now the root folder can be called the _experiment folder_.

### Adding new method

Experiment is a composition of _methods_ producing and consuming _artefacts_. Each method is a command line operation registered using the command `alpheus build`. An artefact is a file or a folder located within the experiment folder. Folder artefacts are denoted with path with trailing '\\' on Windows and '/' on Linux.

For example, the following command registers a method which produces an output artefact `author.txt` by running command `whoami > author.txt`:

```
alpheus build -o "author.txt" "cmd /c whoami > $out1"
```

Note that this command doesn't actually run anything, but just creates `author.txt.alph` file which describes how `author.txt` can be produced. When there are many methods, these description files allow to build a dependency graph for methods of the experiment.

Let the `scripts/count.py` script contains two arguments: input file and output file, and puts number of characters in the input file to the output file. The following command registers a method which runs the script for the `author.txt` and builds `count.txt`:

```
alpheus build -d "scripts/count.py" -d "author.txt" -o "count.txt" "python $in1 $in2 $out1"
```

Note that we manifest that the new method depends on output of the first method, `author.txt`. This information is stored in the created file `count.txt.alph`. 

All `*.alph` files must be committed to the git repository, so the experiment workflow is shared.

### Computing an artefact

To compute an artefact, use `alpheus compute`. For instance, the following command computes `count.txt`:

```
alpheus compute count.txt
```

Alpheus builds the dependency graph of methods needed in order to produce the required file and then runs only those methods which have no up-to-date outputs. Alpheus automatically determines changes in files/directories, so you don't need to worry if the output is consistent. As a result, we get both `author.txt` and `count.txt`. 

It is up to you whether you want to commit these files to the git repository or push them to an external ertefact storage (described later), or keep them just on the local machine. In the latter case, on other machines these files must be recomputed, if needed.


### Removing an artefact/method

Just delete corresponding `*.alph` files. Note that you can break the dependencies by deleting artefacts required by other methods. In this case, the computation of those methods will fail.

### Getting status of an experiment

You can see the status of the artefact in your experiment folder with the command `alpheus status`

e.g.

```
alpheus status count.txt
```

The command will show you status of the artefact that you've specifed and also the artefacts that are used to produce the current one.

There are two statuses of an artefact:

- *Needs recomputation* indicates that the artefact is obsolete because of one of the following reasons:

    - Input artefact is outded.
    - Output artefact is modified not using alpheus.
    - The command producing the artefact is modified.
    - The order of the inputs is changed.
  
- *Up-to-date* indicates that the correct version of the artefact is available either locally or on the artefact storage, such that this artefact is a result of the command applied to the current inputs.
  

### Vector operations [Experimental - not stable]

If you need to perform an identical operation with multiple artefacts, you should provide both input and output paths with an asterisk (*) when declaring a method:

```
alpheus build -d "scripts/count.py" -d "files/*.txt" -o "counts/*.txt" "python $in1 $in2 $out1"
```

In the given example, the script `count.py` will be executed for each text file in the `files` folder, and 
there will be text files with same name containing counts in the `count` folder.

Maximum number of asterisks in the inputs and number of asterisks in the outputs must be same.
Number of asterisks define dimensionality of the vector operations.

Notes:
- The rule is: an asterisk in a path item (i.e. between directory separators) indicates a directory (e.g. `*/data.csv`);
an asterisk with an extension means a file (e.g. `data/*.csv`)
- An asterisk in the input can change its type in the output (e.g. the input files `data/*.csv` to the output directories `metadata/*/`).
- When a vector method instance runs, an asterisk in the output path is replaced with the value of the corresponding asterisk in the input.
This also means that it is possible to change an extension of the output file (e.g. the input `data/*.csv` to the output `metadata/*.json`).
- Only one input can be a vector (i.e. contain one or more asterisks), others must be scalar. Reason for that
are two unclear issues so far: (1) if there are two vector inputs, what is the joint input? is it cartaesian product of the two or just
pairs with same indices? if the latter, how to order input items? (2) how to name outputs in this case?

To aggregate results of a vector operation, just don't specify an asterisk in the output:

```
alpheus build -o "summary.txt" -d "scripts/build_summary.py" -d "files/*.txt" "python $in1 $in2 $out1"
```

In this case, the script gets the input pattern as an argument, e.g. a full path for "files/*.txt" as `$in2`.



#### When to use vector operations [Experimental - not stable]

You may wonder why not to create a method that enumerates needed files itself. You may and is advised to do so in case you have lightway operation on many small files (e.g. image format conversion). 
Use Alpheus vector operations if your vector element processing is considerably heavy.

As the Alpheus keeps track of readyness status of every vector element (file/dir that match the pattern with \*) it impose the overhead. 
On the other hand Alpheus will skip computation of up-to-date vector element and process only those vector elements that require (re)computation. 
This can help as a checkpoint in case of resuming an interrupted computation.

### Using external storage for artefacts


## Support for standard tools and languages

_todo_

## Common worfklow

_todo_: how the user builds the expemeriment, saves, shares. We recommend to start with adding and debugging scripts manually then register it in the dependency graph.

## Migration from a bunch of scripts and data files

_todo_: builds alpheus experiment when you already have a bunch of scripts and files.

## Sharing and collaborating

_todo_

## Running in Cloud

_todo_

## Guidelines

### Save intermediate artefacts & delete them from disk
To speed up the reproducibility checks even more, it is adviced to save and remove from disk any artefact that is not desired to be used directly (as inputs to newly created methods or for manual inspection). This will free Alheus from obligation to check that the disk version of the artefact has not changed since the artefact had been produced.

### File structure

**Shared or exclusive code?**
Working on a problem, eventually you get a solid results you want to keep, but fork another experiment to try another approach. 
After some time, you might get experiments (e.g. having different models) with some code that can be re-used in the experiments.

The conflict is that from one side the shared code is useful since the code improvement automatically affects all the experiments. From 
the other side, being modified it invalidates the computed results, which is bad.

Therefore we suggest consider not sharing the code, but use copy/paste when you explicitly want to propagate changes from one experiment to another. git will help to resolve changes.

## Questions and Answers

#### Why not to use some build system, like make, cmake, etc.?
Build systems usually determine that the file/folder needs to be recalulated using modification time. This will not work if you push the artefact to some storage and later pull them back to your hard drive. Alpheus uses modification timestamps along with the data checksums to verify that the actual disk version matches the expected version of the artefact.
Alpheus gives you an ability to offload most of the experiment artefact to external storages to free your local disk drive. It would be impossible with build tools, like cmake.
