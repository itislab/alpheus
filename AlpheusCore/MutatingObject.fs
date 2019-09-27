module ItisLab.Alpheus

type MutatingObject() =
    let mutable timestamp = 0
    let lockMutation = obj()

    member x.ChangeTimestamp = timestamp

    member internal x.Mutate(action : unit -> unit) =
        lock lockMutation (fun() ->
            timestamp <- timestamp + 1
            action())

    member internal x.GetState(get : unit -> 'a * int) =
        lock lockMutation (fun() ->
            get(), timestamp)
