module Tests

open System
open Xunit
open System.IO
open System.Threading.Tasks

// Ensure we match the return type xUnit.net is looking for
let toAsyncFact computation : Task = Async.StartAsTask computation :> _

[<Fact>]
let ``Hashing is consistent [short buffer]`` () =
    async {
        let random1 = new Random(1)
        let shortBuffer = Array.zeroCreate<Byte> 1024
        random1.NextBytes(shortBuffer)

        let shortBufferCopy = Array.copy shortBuffer

        use memStream = new MemoryStream(shortBuffer)
        use memStream2 = new MemoryStream(shortBufferCopy)

        let! hash1 = ItisLab.Alpheus.Hash.hashStreamAsync 2048 memStream
        let! hash2 = ItisLab.Alpheus.Hash.hashStreamAsync 2048 memStream2

        Assert.Equal(Array.length hash1,Array.length hash2)
        Array.iter2 (fun (x1:byte) (x2:byte) -> Assert.Equal(x1,x2)) hash1 hash2

    } |> toAsyncFact
