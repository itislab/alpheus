# Alpheus

| Platform        | Build           |
| ------------- |:-------------:|
| Windows   | [![VS2019 Build Status](https://img.shields.io/appveyor/ci/dgrechka/alpheus.svg)](https://ci.appveyor.com/project/dgrechka/alpheus) |
| Linux     | [![Linux Build Status](https://travis-ci.org/itislab/alpheus.svg?branch=master)](https://travis-ci.org/itislab/alpheus)  |

[![codecov](https://codecov.io/gh/itislab/alpheus/branch/master/graph/badge.svg)](https://codecov.io/gh/itislab/alpheus)
[![Tests](https://img.shields.io/appveyor/tests/dgrechka/alpheus.svg)](https://ci.appveyor.com/project/dgrechka/alpheus/build/tests)
![NuGet](https://img.shields.io/nuget/v/Alpheus-cli.svg)
[![NuGet](https://img.shields.io/nuget/dt/Alpheus-cli.svg)](https://www.nuget.org/packages/Alpheus-cli/)


A tool for organizing and managing computational experiments. Reproducible research is a way of conducting research allowing to get a provenance of any result and be able to compute it again. Alpheus forces a researcher to follow this way.

Advantages:
_todo: complete the list_

- Builds a dependency graph of operations.
- Incrementally computes only that data which were affected by a change.
- Allows reproducing the data.
- Allows using your usual tools and is based on folders/files.


## Installation
You need to have [.Net Core SDK 2.1.300 or newer](https://dotnet.microsoft.com/learn/dotnet/hello-world-tutorial) installed.

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

1. [.Net Core SDK 2.1.300 or newer](https://dotnet.microsoft.com/download)
1. [Node.js](https://nodejs.org/) 8.11.3 or higher.
1. [Yarn](https://yarnpkg.com/) package manager.


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

Experiment is a composition of _methods_ producing and consuming _artefacts_. Each method is a command line operation registered using the command `alpheus build`. An artefact is a file or a folder located within the experiment folder.

For example, the following command registers a method which produces an output artefact `author.txt` by running command `whoami > author.txt`:

```
alpheus build -o "author.txt" "cmd /c whoami > $out1"
```

Note that this command doesn't actually run anything, but just creates `author.txt.alph` file which describes how `author.txt` can be produced. When there are many methods, these description files allow to build a dependency graph for methods of the experiment.

Let the `scripts/count.py` script contains two arguments: input file and output file, and puts number of characters in the input file to the output file. The following command registers a method which runs the script for the `author.txt` and builds `count.txt`:

```
alpheus build -o "count.txt" -d "scripts/count.py" -d "author.txt" "python $in1 $in2 $out1"
```

Note that we manifest that the new method depends on output of the first method, `author.txt`. This information is stored in the created file `count.txt.alph`. 

All `*.alph` files must be committed to the git repository, so the experiment workflow is shared.

### Computing an artefact

To compute an artefact, use `alpheus compute`. For instance, the following command computes `count.txt`:

```
alpheus compute count.txt
```

Alpheus builds the dependency graph of methods needed in order to produce the required file and then runs only those methods which have no up-to-date outputs. Alpheus automatically determines changes in files/directories, so you don't need to worry if the output is consistent. As a result, we get both `author.txt` and `count.txt`. 

It is up to you whether you want to commit these files to the git repository or push them to an external storage, or keep them just on the local machine. In the latter case, on other machines these files must be recomputed, if needed.


### Removing an artefact/method

Just delete corresponding `*.alph` files. Note that you can break the dependencies by deleting artefacts required by other methods. In this case, the computation of those methods will fail.

### Getting status of an experiment

There are two statuses of an artefact:

- *Needs recomputation* indicates that the artefact is obsolete because of one of the following reasons:
  -- Input artefact is outded.
  -- Output artefact is modified not using alpheus.
  -- The command producing the artefact is modified.
  -- The order of the inputs is changed.
  
- *Up-to-date* indicates that the correct version of the artefact is available either locally or on the attached storage, such that this artefact is a result of the command applied to the current inputs.
  

### Vector operations

If you need to perform an identical operation with multiple artefacts, you should provide both input and output paths with an asterisk (*) when declaring a method:

```
alpheus build -o "counts/*.txt" -d "scripts/count.py" -d "files/*.txt" "python $in1 $in2 $out1"
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
