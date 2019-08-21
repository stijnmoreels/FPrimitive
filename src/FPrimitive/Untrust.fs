namespace FPrimitive

open System

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
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWith (validator : 'a -> bool) (x : Untrust<'a>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithOption (validator : 'a -> 'b option) (x : Untrust<'a>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithResult (validator : 'a -> Result<'b, string list>) (x : Untrust<'a>) = x.tryGetValue validator

