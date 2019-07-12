namespace ItisLab.Alpheus.Tests

open System
open Xunit
open System.IO
open System.Threading.Tasks
open ItisLab.Alpheus

type MethodCommandSyntaxTests()=
    [<Theory>]
    [<InlineData("", 0, 0)>]
    [<InlineData("cmd /c time /T", 0, 0)>]
    [<InlineData("cmd /c $arg", 0, 0)>]
    [<InlineData("$script $in $out >> 2", 0, 0)>]
    [<InlineData("$in1", 1, 0)>]
    [<InlineData("$out1", 0, 1)>]
    [<InlineData("cp $in1 $out1", 1, 1)>]
    [<InlineData("cmd /c $in1 $in2 $in4 $out10 $out2", 3, 2)>]
    member s.``Properly parses command``(command:string, inputs: int, outputs: int) = 
        let (inp, out) = MethodCommand.parseReferences command
        Assert.Equal(inputs, inp.Count)
        Assert.Equal(outputs, out.Count)
    
    [<Theory>]
    [<InlineData("cmd /c time /T", 0, 0)>]
    [<InlineData("cmd /c time /T", 1, 0)>]
    [<InlineData("cmd /c time /T", 0, 1)>]
    [<InlineData("$in1", 1, 0)>]
    [<InlineData("$out1", 0, 1)>]
    [<InlineData("$in1", 2, 0)>]
    [<InlineData("$out1", 0, 3)>]
    [<InlineData("cp $in1 $out1", 1, 1)>]
    [<InlineData("cmd /c $in1 $in2 $in4 $out10 $out2", 4, 10)>]
    [<InlineData("cmd /c $in1 $in2 $in4 $out10 $out2", 30, 100)>]
    member s.``Method command validation doesn't fail in case of correct reference numbers``(command:string, inputs: int, outputs: int) = 
        command |> MethodCommand.validate (inputs, outputs)

    [<Theory>]
    [<InlineData("$in1", 0, 0)>]
    [<InlineData("$out1", 0, 0)>]
    [<InlineData("cp $in1 $out1", 0, 1)>]    
    [<InlineData("cp $in1 $out1", 1, 0)>]
    [<InlineData("cmd /c $in1 $in2 $in4 $out10 $out2", 3, 10)>]
    [<InlineData("cmd /c $in1 $in2 $in4 $out10 $out2", 4, 9)>]
    member s.``Method command validation fails in case of incorrect reference numbers``(command:string, inputs: int, outputs: int) = 
        Assert.Throws(typeof<ArgumentException>, fun () -> command |> MethodCommand.validate (inputs, outputs))