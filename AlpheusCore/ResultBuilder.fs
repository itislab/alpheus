﻿[<AutoOpen>]
module ItisLab.Alpheus.Results

open System

type AlpheusError =
    /// Error which holds the text for the alpheus developers
    |   SystemError of string
    /// Error which is caused by invalid user action
    |   UserError of string

// This code is a modified version of the snippet published in http://fssnip.net/7UJ
type ResultBuilder() =
    member __.Return(x) = Ok x
    member __.ReturnFrom(m: Result<_, _>) = m
    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = 
        match m with
        | Some s -> Ok s
        | None -> Error error
        |> Result.bind f
    member __.Bind((value, predicate, error): ('T * ('T -> bool) * 'E), f) = 
        if predicate value then Ok value else Error error
        |> Result.bind f
    member __.Bind((assertion, error): (bool * 'E), f) = 
        if assertion then Ok() else Error error
        |> Result.bind f
    member __.Zero() = Ok()
    member __.Combine(m, f) = Result.bind f m
    member __.Delay(f: unit -> _) = f
    member __.Run(f) = f()
    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e
    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()
    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)
    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = new ResultBuilder()

