module ItisLab.Alpheus.Tests.OsCommands

open ItisLab.Alpheus.Tests.Utils

let copyFileCommand =
    if isTestRuntimeWindows then
        "cmd /C \"copy $in1 $out1\""
    else
        "/bin/sh -c \"cp $in1 $out1\""

let copyDirCommand =
    if isTestRuntimeWindows then
        "robocopy /E $in1 $out1"
    else
        // ubuntu cp works differently if the dest dir exists.
        // as alpheus creates output dirs before running the command we need to delete it
        "/bin/sh -c \"rm -Rv $out1 ; cp -Rv $in1 $out1\""

let concatCommand = 
    if isTestRuntimeWindows then
        "cmd /C \"cat.cmd $out1 $in1 $in2\""
    else
        "/bin/sh -c \"cat $in1 > $out1; cat $in2 >> $out1\""

// first file is duplicated
let concatCommand2 = 
    if isTestRuntimeWindows then
        "cmd /C \"cat.cmd $out1 $in1 $in1\""
    else
        "/bin/sh -c \"cat $in1 > $out1; cat $in1 >> $out1\""
