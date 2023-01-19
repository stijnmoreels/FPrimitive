namespace FPrimitive

open System
open System.Diagnostics.CodeAnalysis

/// Representation of an untrusted value that should be validated first before it can be used. 
[<Struct; NoEquality; NoComparison>]
type Untrust<'T> (value : 'T) = 
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator) =
        if validator value then Some value
        else None
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator : 'T -> 'TResult option) = validator value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator : 'T -> Result<'TResult, string list>) = validator value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member internal __.tryGetValue (validator : 'T -> Result<'TResult, 'TError>) = validator value
    
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member __.TryGetValue (validator : Func<'T, Maybe<'TResult>>) = 
      if isNull validator then nullArg "validator"
      validator.Invoke value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member __.TryGetValue (validator : Func<'T, Outcome<'TResult, 'TError>>) =
      if isNull validator then nullArg "validator"
      validator.Invoke value
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    member __.TryGetValue (validator : Func<'T, bool>, output : outref<'T>) =
      if validator = null then nullArg "validator"
      if validator.Invoke value 
      then output <- value; true
      else output <- Unchecked.defaultof<'T>; false
    [<ExcludeFromCodeCoverage>]
    static member op_Implicit (x : byte) = Untrust x
    [<ExcludeFromCodeCoverage>]
    static member op_Implicit (x : int) = Untrust x
    [<ExcludeFromCodeCoverage>]
    static member op_Implicit (x : byte[]) = Untrust x
    [<ExcludeFromCodeCoverage>]
    static member op_Implicit (x : string) = Untrust x
    override __.ToString () = sprintf "Untrust: %A" value

/// Representation of an untrusted value that should be validated first before it can be used. 
type 'T untrust = Untrust<'T>

/// Representation of an untrusted value that should be validated first before it can be used. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Untrust = 
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWith (validator : 'T -> bool) (x : Untrust<'T>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithOption (validator : 'T -> 'TResult option) (x : Untrust<'T>) = x.tryGetValue validator
    /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
    let getWithResult (validator : 'T -> Result<'TResult, 'TError>) (x : Untrust<'T>) = x.tryGetValue validator
