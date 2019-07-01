namespace FPrimitive

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core

/// Representation on how the model should be validated.
type CascadeMode = 
  /// Continue even when a validation requirement failed.
  | Continue 
  /// Stop at the first failed validation requirement.
  | FirstFailure

/// Type alias for the validation requirement.
type Requirement<'a> = 'a -> bool * string

/// Representation of a domain specification that contains the validation for the model.
type Spec<'a> =
  private 
      /// Gets all the requirements of the domain specification that the model should uphold.
    { Requirements : ('a -> bool * string) list
      /// Gets the mode in which the validation of the model should happen.
      Cascade : CascadeMode }

/// Operations on the `Spec<_>` type. 
module Spec =
  open System.Text.RegularExpressions

  /// Start defininig a specification for a type.
  [<CompiledName("Of")>]
  let def<'a> : Spec<'a> = 
    { Requirements = []
      Cascade = Continue } 

  /// Adds a custom requirement to the specification.
  let add req spec =
    { spec with Requirements = req :: spec.Requirements }
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  let equalOf selector value message spec =
    add (fun x -> selector x |> (=) value, message) spec

  /// Adds a requirement to check equality to a specified value.
  let equal value message spec =
    equalOf id value message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  let notEqualOf selector value message spec =
    add (fun x -> selector x |> (<>) value, message) spec

  /// Adds a requirement to check no equality to a specified value.
  let notEqual value message spec =
    notEqualOf id value message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  let notNullOf selector message spec =
    add (fun x -> selector x <> null, message) spec

  /// Adds a requirement to check for not `null`.
  let notNull message spec =
    notNullOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  let notEmptyOf selector message spec =
    add (fun x -> selector x |> (<>) String.Empty, message) spec

  /// Adds a requirement to check for non-empty string.
  let notEmpty message spec =
    notEmptyOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  let notWhiteSpaceOf selector message spec =
    let regex = Regex ("^\s+$")
    add (fun x -> selector x |> regex.IsMatch |> not, message) spec

  /// Adds a requirement to check if the string is a not-whitespace string.
  let notWhiteSpace message spec =
    notWhiteSpaceOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  let notNullOrEmptyOf selector message spec =
    add (fun x -> selector x |> String.IsNullOrEmpty |> not, message) spec

  /// Adds a requirement to check if the string is a not-null, not-empty string.
  let notNullOrEmpty message spec =
    notNullOrEmptyOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty, not-whitespace string.
  let notNullOrWhiteSpaceOf selector message spec =
    add (fun x -> selector x |> String.IsNullOrWhiteSpace |> not, message) spec

  /// Adds a requirement to check if the string is a not-null, not-empty, not-whitespace string.
  let notNullOrWhiteSpace message spec =
    notNullOrWhiteSpaceOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  let nonEmptyOf selector message spec =
    add (fun x -> selector x |> Seq.isEmpty |> not, message) spec

  /// Adds a requirement to check for non-empty sequences.
  let nonEmpty message spec =
    nonEmptyOf id message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  let forallOf selector predicate message spec =
    add (fun x -> selector x |> Seq.forall predicate, message) spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  let forall predicate message spec =
    forallOf id predicate message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  let lengthOf selector l message spec =
    add (fun x -> selector x |> Seq.length |> (=) l, message) spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  let lengthMinOf selector min message spec =
    add (fun x -> selector x |> Seq.length |> (<) min , message) spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  let lengthMaxOf selector max message spec =
    add (fun x -> selector x|> Seq.length |> (>) max, message) spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  let lengthBetweenOf selector min max message spec =
    add (fun x -> let l = selector x |> Seq.length in min < l && l > max, message) spec
  
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  let length l message spec =
    lengthOf id l message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  let lengthMin min message spec =
    lengthMinOf id min message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  let lengthMax max message spec =
    lengthMaxOf id max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  let lengthBetween min max message spec =
    lengthBetweenOf id min max message spec
    
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (`limit > value`) the specified limit.
  let lessThanOf selector limit message spec =
    add (fun x -> selector x |> (>) limit, message) spec

  /// Adds a requirement to check if the value is less than (`limit > value`) the specified limit.
  let lessThan limit message spec = 
    lessThanOf id limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (`limit >= value`) the specified limit.
  let lessThanOrEqualOf selector limit message spec =
    add (fun x -> selector x |> (>=) limit, message) spec

  /// Adds a requirement to check if the value is less than or equal to (`limit >= value`) to the specified limit.
  let lessThanOrEqual limit message spec = 
    lessThanOrEqualOf id limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (`limit < value`) the specified limit.
  let greaterThanOf selector limit message spec =
    add (fun x -> selector x |> (<) limit, message) spec

  /// Adds a requirement to check if the value is greater than (`limit < value`) the specified limit.
  let greaterThan limit message spec =
    greaterThanOf id limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (`limit <= value`) the specified limit.
  let greaterThanOrEqualOf selector limit message spec =
    add (fun x -> selector x |> (<=) limit, message) spec

  /// Adds a requirement to check if the value is greater than or equal to (`limit <= value`) the specified limit.
  let greaterThanOrEqual limit message spec =
    greaterThanOrEqualOf id limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (`min <= value && value >= max`) the specified range.
  let inclusiveBetweenOf selector min max message spec =
    add (fun x -> let x = selector x in min <= x && x >= max, message) spec

  /// Adds a requirement to check if the value is inclusive between (`min <= value && value >= max`) the specified range.
  let inclusiveBetween min max message spec =
    inclusiveBetweenOf id min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value > max`) the specified range.
  let exclusiveBetweenOf selector min max message spec =
    add (fun x -> let x = selector x in min < x && x > max, message) spec

  /// Adds a requirement to check if the value is exclusive between (`min < value && value > max`) the specified range.
  let exclusiveBetween min max message spec =
    exclusiveBetweenOf id min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  let regexOf selector pattern message spec =
    let regex = Regex (pattern)
    add (fun x -> selector x |> regex.IsMatch, message) spec
  
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  let regex pattern message spec =
    regexOf id pattern message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  let isTypeOf<'T> selector message spec =
    add (fun x -> match selector x |> box with | :? 'T -> true, message | _ -> false, message) spec

  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  let isType<'T> message spec =
    isTypeOf<'T> id message spec
  
  /// Change the way the validation of requirements should happen.
  let cascade mode spec = { spec with Cascade = mode }
  
  /// Validate the specified value to the domain specification.
  let validate value { Requirements = xs; Cascade = mode } =
    let checkRequirement f =
      let ok, err = try f value with ex -> false, ex.Message
      if ok then None else Some err
    
    let errors =
      match mode with
      | Continue -> List.choose checkRequirement xs
      | FirstFailure -> 
        let folder acc f = Option.orElseWith (fun () -> checkRequirement f) acc
        xs |> List.fold folder None
           |> Option.toList

    if errors.Length = 0 then Ok value
    else Error errors

  /// Validate the specified untrusted value to the domain specification.
  let validateUntrust (untrusted : Untrust<'a>) (spec : Spec<'a>) =
    untrusted.tryGetValue (fun x -> validate x spec)

  /// Validate the specified value to the domain specification, discarding the error messages.
  let validateOption value spec =
    match validate value spec with
    | Ok x -> Some x
    | _ -> None

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  let createModel f value spec =
    validate value spec
    |> Result.map f

  /// Determine whether the specified value satisfies the domain specification.
  let isSatisfiedBy value spec =
    validate value spec = Ok value

/// Computation expression builder for the domain specification `Spec<_>`.
type SpecBuilder<'a, 'b> internal (validate : Spec<'a> -> 'b) =
  /// Adds a custom requirement to the specification.
  [<CustomOperation("add")>]
  member __.Add (state, req) = Spec.add req state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  [<CustomOperation("equalOf")>]
  member __.EqualOf (state, selector, value, message) = Spec.equalOf selector value message state
  /// Adds a requirement to check equality to a specified value.
  [<CustomOperation("equal")>]
  member __.Equal (state, value, message) = Spec.equal value message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  [<CustomOperation("notEqualOf")>]
  member __.NotEqualOf (state, selector, value, message) = Spec.notEqualOf selector value message state
  /// Adds a requirement to check no equality to a specified value.
  [<CustomOperation("notEqual")>]
  member __.NotEqual (state, value, message) = Spec.notEqual value message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  [<CustomOperation("notNullOf")>]
  member __.NotNullOf (state, selector, message) = Spec.notNullOf selector message state
  /// Adds a requirement to check for not `null`.
  [<CustomOperation("notNull")>]
  member __.NotNull (state, message) = Spec.notNull message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  [<CustomOperation("notEmptyOf")>]
  member __.NotEmptyOf (state, selector, message) = Spec.notEmptyOf selector message state
  /// Adds a requirement to check for non-empty string.
  [<CustomOperation("notEmpty")>]
  member __.NotEmpty (state, message) = Spec.notEmpty message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  [<CustomOperation("notWhiteSpaceOf")>]
  member __.NotWhiteSpaceOf (state, selector, message) = Spec.notNullOrWhiteSpaceOf selector message state
  /// Adds a requirement to check if the string is a not-whitespace string.
  [<CustomOperation("notWhiteSpace")>]
  member __.NotWhiteSpace (state, message) = Spec.notWhiteSpace message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  [<CustomOperation("notNullOrEmptyOf")>]
  member __.NotNullOrEmptyOf (state, selector, message) = Spec.notNullOrEmptyOf selector message state
  /// Adds a requirement to check if the string is a not-null, not-empty string.
  [<CustomOperation("notNullOrEmpty")>]
  member __.NotNullOrEmpty (state, message) = Spec.notNullOrEmpty message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty, not-whitespace string.
  [<CustomOperation("notNullOrWhiteSpaceOf")>]
  member __.NotNullOrWhiteSpaceOf (state, selector, message) = Spec.notNullOrWhiteSpaceOf selector message state
  /// Adds a requirement to check if the string is a not-null, not-empty, not-whitespace string.
  [<CustomOperation("notNullOrWhiteSpace")>]
  member __.NotNullOrWhiteSpace (state, message) = Spec.notNullOrWhiteSpace message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<CustomOperation("nonEmptyOf")>]
  member __.NonEmptyOf (state, selector, message) = Spec.nonEmptyOf selector message state
  /// Adds a requirement to check for non-empty sequences.
  [<CustomOperation("nonEmpty")>]
  member __.NonEmpty (state, message) = Spec.nonEmpty message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<CustomOperation("forallOf")>]
  member __.ForallOf (state, selector, predicate, message) = Spec.forallOf selector predicate message state
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<CustomOperation("forall")>]
  member __.ForAll (state, predicate, message) = Spec.forall predicate message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<CustomOperation("lengthOf")>]
  member __.LengthOf (state, selector, length, message) = Spec.lengthOf selector length message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<CustomOperation("lengthMinOf")>]
  member __.LengthMinOf (state, selector, min, message) = Spec.lengthMinOf selector min message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<CustomOperation("lengthMaxOf")>]
  member __.LengthMaxOf (state, selector, max, message) = Spec.lengthMaxOf selector max message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<CustomOperation("lengthBetweenOf")>]
  member __.LengthBetweenOf (state, selector, min, max, message) = Spec.lengthBetweenOf selector min max message state
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<CustomOperation("length")>]
  member __.Length (state, length, message) = Spec.length length message state
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<CustomOperation("lengthMin")>]
  member __.LengthMin (state, min, message) = Spec.lengthMin min message state
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<CustomOperation("lengthMax")>]
  member __.LengthMax (state, max, message) = Spec.lengthMax max message state
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<CustomOperation("lengthBetween")>]
  member __.LengthBetween (state, min, max, message) = Spec.lengthBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (`limit > value`) the specified limit.
  [<CustomOperation("lessThanOf")>]
  member __.LessThanOf (state, selector, limit, message) = Spec.lessThanOf selector limit message state
  /// Adds a requirement to check if the value is less than (`limit > value`) the specified limit.
  [<CustomOperation("lessThan")>]
  member __.LessThan (state, limit, message) = Spec.lessThan limit message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (`limit >= value`) the specified limit.
  [<CustomOperation("lessThanOrEqualOf")>]
  member __.LessThanOrEqualOf (state, selector, limit, message) = Spec.lessThanOrEqualOf selector limit message state
  /// Adds a requirement to check if the value is less than or equal to (`limit >= value`) to the specified limit.
  [<CustomOperation("lessThanOrEqual")>]
  member __.LessThanOrEqual (state, limit, message) = Spec.lessThanOrEqual limit message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (`limit < value`) the specified limit.
  [<CustomOperation("greaterThanOf")>]
  member __.GreaterThanOf (state, selector, limit, message) = Spec.greaterThanOf selector limit message state
  /// Adds a requirement to check if the value is greater than (`limit < value`) the specified limit.
  [<CustomOperation("greaterThan")>]
  member __.GreaterThan (state, limit, message) = Spec.greaterThan limit message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (`limit <= value`) the specified limit.
  [<CustomOperation("greaterThanOrEqualOf")>]
  member __.GreaterThanOrEqualOf (state, selector, limit, message) = Spec.greaterThanOrEqualOf selector limit message state
  /// Adds a requirement to check if the value is greater than or equal to (`limit <= value`) the specified limit.
  [<CustomOperation("greaterThanOrEqual")>]
  member __.GreaterThanOrEqual (state, limit, message) = Spec.greaterThanOrEqual limit message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (`min <= value && value >= max`) the specified range.
  [<CustomOperation("inclusiveBetweenOf")>]
  member __.InclusiveBetweenOf (state, selector, min, max, message) = Spec.inclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is inclusive between (`min <= value && value >= max`) the specified range.
  [<CustomOperation("inclusiveBetween")>]
  member __.InclusiveBetween (state, min, max, message) = Spec.inclusiveBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value > max`) the specified range.
  [<CustomOperation("exclusiveBetweenOf")>]
  member __.ExclusiveBetweenOf (state, selector, min, max, message) = Spec.exclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is exclusive between (`min < value && value > max`) the specified range.
  [<CustomOperation("exclusiveBetween")>]
  member __.ExclusiveBetween (state, min, max, message) = Spec.exclusiveBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<CustomOperation("regexOf")>]
  member __.RegexOf (state, selector, pattern, message) = Spec.regexOf selector pattern message state
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<CustomOperation("regex")>]
  member __.Regex (state, pattern, message) = Spec.regex pattern message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  [<CustomOperation("isTypeOf")>]
  member __.IsTypeOf<'T> (state, selector, message) = Spec.isTypeOf<'T> selector message state
  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  [<CustomOperation("isType")>]
  member __.IsType<'T> (state, message) = Spec.isType<'T> message state
  member __.Yield (_) = Spec.def<'a>
  member __.Run (spec) = validate spec

/// Exposure values/functions for the `Spec<_>` type.
[<AutoOpen>]
module SpecExposure =
  /// Computation builder to create an `Spec<_>` instance.
  let spec<'a> = new SpecBuilder<'a, Spec<'a>> (id)
  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Result<_, string list>` when runned.
  let specResult<'a> value = new SpecBuilder<'a, Result<'a, string list>> (Spec.validate value)
  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Option<_>` when runned.
  let specOption<'a> value = new SpecBuilder<'a, 'a option> (Spec.validateOption value)
  /// Computation builder to build an `Spec,_>` instance that gets validated from an `Untrust<_>` to a `Result<_, string list>` when runned.
  let specUntrust<'a> untrusted = new SpecBuilder<'a, Result<'a, string list>> (Spec.validateUntrust untrusted)
  /// Computation builder to build an `Spec<_>` instance that gets validated a custom domain model when runned.
  let specModel<'a, 'b> createModel value = new SpecBuilder<'a, Result<'b, string list>> (Spec.createModel createModel value)

/// Result type when a value is validated against a domain specification `Spec<_>`.
type ValidationResult<'a> internal (result) =
  /// Gets the value that was validated (possible `null` when the validation failed).
  member __.Value : 'a = Result.getOrValue Unchecked.defaultof<'a> result
  /// Gets a value indicating whether the validation succeeded.
  member __.IsValid = Result.isOk result
  /// Gets the series of validation errors that describe to what domain requirements the validated value doesn't satisfy.
  member __.Errors : string array = Result.either (fun _ -> Array.empty) Array.ofList result
  static member op_Implicit (validationResult : ValidationResult<'a>) = 
    validationResult.Value

/// Exception thrown when the validation of a value against a domain specification failed.
exception ValidationFailureException of string

/// Extensions on the `Spec<_>` type to use in C# context.
[<Extension>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
type SpecExtensions =

  /// Adds a custom requirement to the specification.
  [<Extension>]
  static member Add (spec, (requirement : Func<_, ValueTuple<_, _>>)) =
    if requirement = null then nullArg "requirement"
    Spec.add (requirement.Invoke >> fun t -> t.ToTuple()) spec

  /// Adds a custom requirement to the specification.
  [<Extension>]
  static member Add (spec, (requirement : Func<_, _>), message) =
    if requirement = null then nullArg "requirement"
    Spec.add (fun x -> requirement.Invoke (x), message) spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  [<Extension>]
  static member Equal (spec, (selector : Func<_, _>), value, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.equalOf selector.Invoke value message spec
  
  /// Adds a requirement to check equality to a specified value.
  [<Extension>]
  static member Equal (spec, value, message) =
    if message = null then nullArg "message"
    Spec.equal value message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  [<Extension>]
  static member NotEqual (spec, (selector : Func<_, _>), value, message) =  
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notEqualOf selector.Invoke value message spec

  /// Adds a requirement to check no equality to a specified value.
  [<Extension>]
  static member NotEqual (spec, value, message) = 
    if message = null then nullArg "message"
    Spec.notEqual value message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  [<Extension>]
  static member NotNull (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notNullOf selector.Invoke message spec

  /// Adds a requirement to check for not `null`.
  [<Extension>]
  static member NotNull (spec, message) =
    if message = null then nullArg "message"
    Spec.notNull message spec

  /// Adds a requirement to check for non-empty string.
  [<Extension>]
  static member NotEmpty (spec, message) =
    if message = null then nullArg "message"
    Spec.notEmpty message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  [<Extension>]
  static member NotEmpty (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notEmptyOf selector.Invoke message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  [<Extension>]
  static member NotWhiteSpace (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notWhiteSpaceOf selector.Invoke message spec

  /// Adds a requirement to check if the string is a not-whitespace string.
  [<Extension>]
  static member NotWhiteSpace (spec, message) =
    if message = null then nullArg "message"
    Spec.notWhiteSpace message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  [<Extension>]
  static member NotNullOrEmpty (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notNullOrEmptyOf selector.Invoke message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty (spec, message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All (spec, (selector : Func<_, _>), (predicate : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All (spec, (predicate : Func<_, _>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length (spec, (selector : Func<_, IEnumerable<_>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length (spec, (selector : Func<_, IList<_>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length (spec, (selector : Func<_, _[]>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length (spec, (selector : Func<_, ICollection<_>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<IEnumerable<'a>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<IList<'a>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec
  
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<ICollection<'a>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'a[]>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec
  
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin (spec, (selector : Func<_, IEnumerable<_>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin (spec, (selector : Func<_, IList<_>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin (spec, (selector : Func<_, ICollection<_>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin (spec, (selector : Func<_, _[]>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<IEnumerable<_>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<ICollection<_>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<IList<_>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<_[]>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax (spec, (selector : Func<_, IEnumerable<_>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec
 
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax (spec, (selector : Func<_, IList<_>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

     /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax (spec, (selector : Func<_, ICollection<_>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

     /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax (spec, (selector : Func<_, _[]>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<IEnumerable<_>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<IList<_>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<ICollection<_>>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<_[]>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween (spec, (selector : Func<_, IEnumerable<_>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec
  
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween (spec, (selector : Func<_, IList<_>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

    /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween (spec, (selector : Func<_, ICollection<_>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

    /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween (spec, (selector : Func<_, _[]>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<IEnumerable<_>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<ICollection<_>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

    /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<IList<_>>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

    /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<_[]>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (`limit > value`) the specified limit.
  [<Extension>]
  static member LessThan (spec, (selector : Func<_, _>), limit, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lessThanOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is less than (`limit > value`) the specified limit.
  [<Extension>]
  static member LessThan (spec, limit, message) =
    if message = null then nullArg "message"
    Spec.lessThan limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (`limit < value`) the specified limit.
  [<Extension>]
  static member GreaterThan (spec, (selector : Func<_, _>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOf selector.Invoke limit message spec
  
  /// Adds a requirement to check if the value is greater than (`limit < value`) the specified limit.
  [<Extension>]
  static member GreaterThan (spec, limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThan limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (`limit >= value`) the specified limit.
  [<Extension>]
  static member LessThanOrEqual (spec, (selector : Func<_, _>), limit, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lessThanOrEqualOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is less than or equal to (`limit >= value`) to the specified limit.
  [<Extension>]
  static member LessThanOrEqual (spec, limit, message) =
    if message = null then nullArg "message"
    Spec.lessThanOrEqual limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (`limit <= value`) the specified limit.
  [<Extension>]
  static member GreaterThanOrEqual (spec, (selector : Func<_, _>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOrEqualOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is greater than or equal to (`limit <= value`) the specified limit.
  [<Extension>]
  static member GreaterThanOrEqual (spec, limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOrEqual limit message spec
  
  /// Adds a requirement to check if the value is inclusive between (`min <= value && value >= max`) the specified range.
  [<Extension>]
  static member InclusiveBetween (spec, min, max, message) =
    if message = null then nullArg "message"
    Spec.inclusiveBetween message min max spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (`min <= value && value >= max`) the specified range.
  [<Extension>]
  static member InclusiveBetween (spec, (selector : Func<_, _>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.inclusiveBetweenOf selector.Invoke min max message spec

  /// Adds a requirement to check if the value is exclusive between (`min < value && value > max`) the specified range.
  [<Extension>]
  static member ExclusiveBetween (spec, min, max, message) =
    if message = null then nullArg "message"
    Spec.exclusiveBetween min max message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value > max`) the specified range.
  [<Extension>]
  static member ExclusiveBetween (spec, (selector : Func<_, _>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.exclusiveBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Extension>]
  static member Regex (spec, (selector : Func<_, _>), pattern, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.regexOf selector.Invoke pattern message spec

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Extension>]
  static member Regex (spec, pattern, message) =
    if message = null then nullArg "message"
    Spec.regex pattern message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  [<Extension>]
  static member IsType<'T> (spec, (selector : Func<_, _>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.isTypeOf<'T> selector.Invoke message spec

  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  [<Extension>]
  static member IsType<'T> (spec, message) =
    if message = null then nullArg "message"
    Spec.isType<'T> message spec

  /// Change the way the validation of requirements should happen.
  [<Extension>]
  static member Cascade (spec, mode) = 
    Spec.cascade mode spec

  /// Determine whether the specified value satisfies the domain specification.
  [<Extension>]
  static member IsSatisfiedBy (spec, value) =
    Spec.isSatisfiedBy value spec

  /// Validate the specified value to the domain specification.
  [<Extension>]
  static member Validate (spec, value) =
    Spec.validate value spec |> ValidationResult

  /// Validate the specified value to the domain specification.
  [<Extension>]
  static member ValidateThrow<'T, 'TException when 'TException :> exn> (spec, value, message : string) : 'T =
    if message = null then nullArg "message" 
    Spec.validate value spec
    |> Result.getOrElse (fun _ -> 
      raise (Activator.CreateInstance(typeof<'TException>, [| message :> obj |]) :?> 'TException))

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModel (spec, value, (creator : Func<_, _>)) =
    if creator = null then nullArg "creator"
    Spec.createModel creator.Invoke value spec |> ValidationResult

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member TryCreateModel<'T, 'TResult> ((spec : Spec<'T>), value, (creator : Func<'T, 'TResult>), result : outref<'TResult>) =
    match Spec.createModel creator.Invoke value spec with
    | Ok x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'TResult>; false

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult, 'TException when 'TException :> exn> (spec, value, (creator : Func<'T, 'TResult>), message) =
    if creator = null then nullArg "creator"
    Spec.createModel creator.Invoke value spec
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (Activator.CreateInstance (typeof<'TException> , [| message :> obj |]) :?> 'TException))
  
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult> (spec, value, (creator : Func<_, _>), message) =
    if creator = null then nullArg "creator"
    Spec.createModel creator.Invoke value spec
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (ValidationFailureException message))