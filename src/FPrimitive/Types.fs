namespace FPrimitive

open System
open System.Threading

open Microsoft.FSharp.Core

/// Exception that gets thrown when a `ReadOnce<_>` instance was being read more than once.
exception AlreadyReadException of string

/// Representation of a value that can only be read once.
type ReadOnce<'a> (value : 'a) =
    member val private Value = ref (value :> obj)
    /// Tries to read the read-once value.
    member this.tryGetValue () =
        let value = Interlocked.Exchange (this.Value, null)
        if value <> null then Some (value :?> 'a)
        else None
    /// Reads the read-once value and throws an `AlreadyReadException` when the value was already been read.
     member this.GetValue () =
        match this.tryGetValue () with
        | Some x -> x
        | None -> raise (AlreadyReadException (sprintf "The value of type %s is already been read" typeof<'a>.Name))
     /// Tries to reads the read-once value and throws an `AlreadyReadException` when the value was already been read.
     member this.TryGetValue (output : outref<'a>) =
        match this.tryGetValue () with
        | Some x -> output <- x; true
        | None -> output <- Unchecked.defaultof<'a>; false

/// Representation of a value that can only be read once.
module ReadOnce =
    /// Wraps a value into a value that can only be read once.
    let value x = ReadOnce x
    /// Tries to read the read-once value.
    let tryGetValue (x : ReadOnce<_>) = x.tryGetValue ()
    /// Tries to read the read-once value.
    let tryGetValueError x error = tryGetValue x |> Result.ofOption error

/// Representation of a value that can be disposed/removed any time.
type Disposable<'a> (value : 'a) =
    member val private RefValue = ref (value :> obj)
    /// Tries to read the possible disposed value.
    member this.tryGetValue () =
        if this.RefValue = ref null then None
        else Some (this.RefValue.contents :?> 'a)
    /// Reads the possible disposed value, throws an `ObjectDisposedException` otherwise.
    member this.GetValue () =
        match this.tryGetValue () with
        | Some x -> x
        | None -> raise (ObjectDisposedException (sprintf "The value of type %s is already been disposed" typeof<'a>.Name))
    /// Tries to read the possible disposed value.
    member this.TryGetValue (output : outref<'a>) =
        match this.tryGetValue () with
        | Some x -> output <- x; true
        | None -> output <- Unchecked.defaultof<'a>; false

    interface IDisposable with
        member this.Dispose () = 
            Interlocked.Exchange (this.RefValue, null) |> ignore

/// Representation of an untrusted value that should be validated first before it can be used. 
type Untrust<'a> (value : 'a) = 
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator) =
        if validator value then Some value
        else None
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator : 'a -> 'b option) = validator value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator : 'a -> Result<'b, string list>) = validator value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member __.TryGetValue (validator : Func<_, _>, output : outref<'a>) =
        if validator = null then nullArg "validator"
        if validator.Invoke value 
        then output <- value; true
        else output <- Unchecked.defaultof<'a>; false
    static member op_Implicit (x : byte) = Untrust x
    static member op_Implicit (x : int) = Untrust x
    static member op_Implicit (x : byte[]) = Untrust x
    static member op_Implicit (x : string) = Untrust x

/// Representation of an untrusted value that should be validated first before it can be used. 
module Untrust = 
    /// Wraps an untrusted value into a representation that only exposes untrusted values after validation.
    [<CompiledName("Create")>]
    let set x = Untrust x
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWith (validator : 'a -> bool) (x : Untrust<'a>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithOption (validator : 'a -> 'b option) (x : Untrust<'a>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithResult (validator : 'a -> Result<'b, string list>) (x : Untrust<'a>) = x.tryGetValue validator

