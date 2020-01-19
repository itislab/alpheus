module ItisLab.Alpheus.Tests.UtilsTests

open Xunit
open ItisLab.Alpheus.Tests
open ItisLab.Alpheus

[<Fact>]
let ``singleExecutionGuardAsync executes the tasks``() =
    async {
        let tasksCache = ref Map.empty
        let getAsync arg =
            async { return 12345 + arg}
        let! res = Utils.singleExecutionGuardAsync tasksCache 10 getAsync
        Assert.Equal(12355,res)   
        }

[<Fact>]
let ``singleExecutionGuardAsync executes the task once``() =
    async {
        let tasksCache = ref Map.empty
        let sync = obj()
        let counter = ref 0
        let getAsync arg =
            async { 
                lock sync (fun () -> incr(counter))
                return 12345 + arg
            }
        let! _ = getAsync 1 // not guarded
        let! _ = getAsync 1
        Assert.Equal(2,!counter)
        counter := 0

        let! res1 = Utils.singleExecutionGuardAsync tasksCache 20 getAsync
        let! res2 = Utils.singleExecutionGuardAsync tasksCache 20 getAsync
        Assert.Equal(1,!counter)
        Assert.Equal(12365,res1)   
        Assert.Equal(12365,res2)   
        }

[<Fact>]
let ``singleExecutionGuardAsync executes different tasks for diff args``() =
    async {
        let tasksCache = ref Map.empty
        let sync = obj()
        let counter = ref 0
        let getAsync arg =
            async { 
                lock sync (fun () -> incr(counter))
                return 12345 + arg
            }
        let! _ = getAsync 1 // not guarded
        let! _ = getAsync 1
        Assert.Equal(2,!counter)
        counter := 0

        let! res1 = Utils.singleExecutionGuardAsync tasksCache 20 getAsync
        let! res2 = Utils.singleExecutionGuardAsync tasksCache 30 getAsync
        Assert.Equal(2,!counter) // as args are different
        Assert.Equal(12365,res1)   
        Assert.Equal(12375,res2)   
        }


