namespace FPrimitive

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.RegularExpressions

open Microsoft.FSharp.Core

/// Representation on how the model should be validated.
type CascadeMode = 
  /// Continue even when a validation requirement failed.
  | Continue
  /// Stop at the first failed validation requirement.
  | FirstFailure

/// Type alias for the validation requirement.
type Req<'T> = 'T -> bool * string

/// Representation of a domain specification that contains the validation for the model.
type Spec<'T> =
  private 
      /// Gets all the requirements of the domain specification that the model should uphold.
    { Requirements : ('T -> bool * string) list
      /// Gets the mode in which the validation of the model should happen.
      Cascade : CascadeMode
      /// Gets the child specifications.
      Dependents : Spec<'T> list }

/// Operations on the `Spec<_>` type. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Spec =
  /// Start defininig a specification for a type.
  let def<'a> : Spec<'a> = 
    { Requirements = []
      Cascade = FirstFailure
      Dependents = [] }

  /// Adds a custom requirement to the specification.
  let add req spec =
    { spec with Requirements = req :: spec.Requirements }
    
  /// Adds a custom requirement to the specification.
  let verify verifier msg spec = add (fun x -> verifier x, msg) spec

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

  /// Adds a requirement for the reuslt of the specified mapping,
  /// whcih defines that the result should be a string starting with the specified header.
  let startsWithOf selector header message spec =
    add (selector >> fun (s : string) -> s.StartsWith (header), message) spec

  /// Adds a requirement to check if the string starts with the specified header.
  let startsWith header message spec =
    startsWithOf id header message spec

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
  /// which defines that all elements of the sequence should be unique.
  let uniqueOf selectSeq selectKey message spec =
    add (fun x -> selectSeq x |> Seq.groupBy selectKey |> Seq.forall (fun (_, g) -> Seq.length g = 1), message) spec

  /// Adds a requirement to check for sequences with only unique elements.
  let unique selectKey message spec =
    uniqueOf id selectKey message spec

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
    add (fun x -> let l = selector x |> Seq.length in min <= l && l <= max, message) spec
  
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
  /// which defines that the result should be inclusive between (`min <= value && value <= max`) the specified range.
  let inclusiveBetweenOf selector min max message spec =
    add (fun x -> let x = selector x in min <= x && x <= max, message) spec

  /// Adds a requirement to check if the value is inclusive between (`min <= value && value <= max`) the specified range.
  let inclusiveBetween min max message spec =
    inclusiveBetweenOf id min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value < max`) the specified range.
  let exclusiveBetweenOf selector min max message spec =
    add (fun x -> let x = selector x in min < x && x < max, message) spec

  /// Adds a requirement to check if the value is exclusive between (`min < value && value < max`) the specified range.
  let exclusiveBetween min max message spec =
    exclusiveBetweenOf id min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  let matchesOf selector pattern message spec =
    let regex = Regex (pattern)
    add (fun x -> selector x |> regex.IsMatch, message) spec

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  let matches pattern message spec =
    matchesOf id pattern message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Obsolete("Use 'matchesOf' instead")>]
  let regexOf selector pattern message spec =
    matchesOf selector pattern message spec
  
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Obsolete("Use 'matches' instead")>]
  let regex pattern message spec =
    matchesOf id pattern message spec

  let private alphabet = ['a'..'z'] @ ['A'..'Z']

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string containing only charaters in the alphabet.
  let alphabeticalOf selector message spec =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphabet), message) spec

  /// Adds a requirement to check for strings containing only charaters in the alphabet.
  let alphabetical message spec =
    alphabeticalOf id message spec

  let private alphanumerical = alphabet @ List.map (string >> char) [0..9]

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value.
  let alphanumOf selector message spec =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphanumerical), message) spec

  /// Adds a requirement to check for alphanumerical values.
  let alphanum message spec =
    alphanumOf id message spec

  let private alphanumerical_extra =
    alphanumerical
    @ [ ' '; '!'; '\'';  '#'; '$'; '%'; '&'; '\''; '\"'; '('; ')'; '*'; '+';  ','; '-'; '.'; '/'; ':'; ';'; '<'; '='; '>'; '?'; '@'; '['; '\\'; ']'; '^'; '_'; '`'; '{'; '|'; '}'; '~' ]

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value or special charater.
  let alphanumSpecialOf selector message spec =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphanumerical_extra), message) spec
  
  /// Adds a requirement to check for alphanumerical values or special charaters.
  let alphanumSpecial message spec =
    alphanumSpecialOf id message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  let isTypeOf<'T, 'TResult, 'TExpected> (selector : 'T -> 'TResult) message spec =
    add (fun x -> match selector x |> box with | :? 'TExpected -> true, message | _ -> false, message) spec

  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  let isType<'T, 'TExpected> message (spec : Spec<'T>) =
    isTypeOf id message spec

  /// Change the way the validation of requirements should happen.
  let cascade mode spec = { spec with Cascade = mode }
  
  /// Validate the specified value to the domain specification.
  let rec validate value { Requirements = xs; Cascade = mode; Dependents = ds } =
    let checkRequirement f =
      let ok, err = try f value with ex -> false, "Exception thrown during validating requirement: " + ex.Message
      if ok then None else Some err
    
    let dependantResult =
      List.map (validate value) ds 
      |> Result.sequence
      |> Result.map (fun _ -> value)

    match dependantResult with
    | Ok _ ->
        let errors =
          match mode with
          | Continue -> List.choose checkRequirement xs
          | FirstFailure -> 
            let folder acc f = Option.orElseWith (fun () -> checkRequirement f) acc
            xs |> List.fold folder None
               |> Option.toList
        if errors.Length = 0 then Ok value
        else Error errors
    | x -> x

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

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  let createModelWith f value spec =
    validate value spec
    |> Result.bind f

  /// Determine whether the specified value satisfies the domain specification.
  let isSatisfiedBy value spec =
    validate value spec = Ok value

  /// Adds another specification on which the current specification depends on.
  /// This dependent specification will run before the current specification and contain only the errors of the dependents in case of a validation failure.
  let dependsOn dependent spec =
    { spec with Dependents = dependent :: spec.Dependents }

  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  let merge spec1 spec2 =
    { def with Dependents = [spec1; spec2] }

  /// Maps the to-be-validated value before the requirements are verified, therefore controlling the input value of this specification and further added requirements.
  let rec comap f spec =
    { Requirements = List.map (fun r -> f >> r) spec.Requirements
      Cascade = spec.Cascade
      Dependents = List.map (comap f) spec.Dependents }

  /// Maps the requirements of this specification to another requirement function structure.
  let rec map (f : Req<'a> -> Req<'b>) spec =
    { Requirements = List.map f spec.Requirements
      Cascade = spec.Cascade
      Dependents = List.map (map f) spec.Dependents }

  /// Adds a pre-valildation function to all the requirements of this specification.
  let rec preval f spec =
    map (fun r -> fun x -> match f x with | Ok y -> r y | Error msg -> false, msg) spec

  /// Maps this specification to work with a list of values.
  let rec list spec =
    let asList r xs =
      List.map r xs 
      |> List.reduce (fun (x, err1) (y, err2) -> x && y, err1 + Environment.NewLine + err2)
    map asList spec

  /// Maps this specification to work with array of values.
  let rec array spec =
      let asArray r xs =
        Array.map r xs 
        |> Array.reduce (fun (x, err1) (y, err2) -> x && y, err1 + Environment.NewLine + err2)
      map asArray spec

  /// Maps this specification to work with sequences of values.
  let rec seq spec =
      let asSeq r xs =
        Seq.map r xs |> Seq.reduce (fun (x, err1) (y, err2) -> x && y, err1 + Environment.NewLine + err2)
      map asSeq spec

  let private reduceCascade cs =
    if List.contains FirstFailure cs
    then FirstFailure 
    else Continue

  /// Makes a new specification that's describes an invariant relation between two given specifications.
  let invariant spec1 spec2 =
    let r1 = List.map (fun f -> fst >> f) spec1.Requirements
    let r2 = List.map (fun f -> snd >> f) spec2.Requirements
    let rs = r1 @ r2

    let cascade = reduceCascade [ spec1.Cascade; spec2.Cascade ] 

    let d1 = List.map (comap fst) spec1.Dependents
    let d2 = List.map (comap snd) spec2.Dependents
    let ds = d1 @ d2

    { Requirements = rs
      Cascade = cascade
      Dependents = ds }

  let private x__ (x, _, _) = x
  let private _x_ (_, y, _) = y
  let private __x (_, _, z) = z

  /// Makes a new specification that's describes an invariant relation between three given specifications.
  let invariant3 spec1 spec2 spec3 =
    let r1 = List.map (fun f -> x__ >> f) spec1.Requirements
    let r2 = List.map (fun f -> _x_ >> f) spec2.Requirements
    let r3 = List.map (fun f -> __x >> f) spec3.Requirements
    let rs = List.concat [ r1; r2; r3 ]

    let cascade = reduceCascade [ spec1.Cascade; spec2.Cascade; spec3.Cascade ] 

    let d1 = List.map (comap x__) spec1.Dependents
    let d2 = List.map (comap _x_) spec2.Dependents
    let d3 = List.map (comap __x) spec3.Dependents
    let ds = List.concat [ d1; d2; d3 ]

    { Requirements = rs
      Cascade = cascade
      Dependents = ds }

/// Representation of a domain specification that contains the validation for the model.
type Spec =
  /// Start defininig a specification for a type.
  static member Of<'T>() : Spec<'T> =
      { Requirements = []
        Cascade = Continue
        Dependents = [] }
  
  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  static member Merge (spec1 : Spec<'T>, spec2) = Spec.merge spec1 spec2
  /// Makes a new specification that's describes an invariant relation between two given specifications.
  static member Invariant (spec1 : Spec<'T1>, spec2 : Spec<'T2>) = Spec.invariant spec1 spec2 |> Spec.comap (fun (t : ValueTuple<_, _>) -> t.ToTuple())
  /// Makes a new specification that's describes an invariant relation between three given specifications.
  static member Invariant (spec1 : Spec<'T1>, spec2 : Spec<'T2>, spec3 : Spec<'T3>) = Spec.invariant3 spec1 spec2 spec3 |> Spec.comap (fun (t : ValueTuple<_, _, _>) -> t.ToTuple())

/// Computation expression builder for the domain specification `Spec<_>`.
type SpecBuilder<'a, 'b> internal (validate : Spec<'a> -> 'b, ?start) =
  let start = Option.defaultValue Spec.def<'a> start
  /// Adds a custom requirement to the specification.
  [<CustomOperation("add")>]
  member __.Add (state, req) = Spec.add req state
  /// Adds a custom requirement to the specification.
  [<CustomOperation("verify")>]
  member __.Verify (state, verifier, msg) = Spec.verify verifier msg state
  /// Change the way the validation of requirements should happen.
  [<CustomOperation("cascade")>]
  member __.Cascade (state, mode) = Spec.cascade mode state
  /// Adds another specification on which the current specification depends on.
  /// This dependent specification will run before the current specification and contain only the errors of the dependents in case of a validation failure.
  [<CustomOperation("dependsOn")>]
  member __.DependsOn (state, dependent) = Spec.dependsOn dependent state
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
  /// Adds a requirement to check if the string starts with the specified header.
  [<CustomOperation("startsWith")>]
  member __.StartsWith (state, header, message) = Spec.startsWith header message state
  /// Adds a requirement for the reuslt of the specified mapping,
  /// whcih defines that the result should be a string starting with the specified header.
  [<CustomOperation("startsWithOf")>]
  member __.StartsWithOf (state, selector, header, message) = Spec.startsWithOf selector header message state
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
  /// which defines that all elements of the sequence should be unique.
  [<CustomOperation("uniqueOf")>]
  member __.UniqueOf (state, selectSeq, selectKey, message) = Spec.uniqueOf selectSeq selectKey message state
  /// Adds a requirement to check for sequences with only unique elements.
  [<CustomOperation("unique")>]
  member __.Unique (state, selectKey, message) = Spec.unique selectKey message state
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
  /// which defines that the result should be inclusive between (`min <= value && value <= max`) the specified range.
  [<CustomOperation("inclusiveBetweenOf")>]
  member __.InclusiveBetweenOf (state, selector, min, max, message) = Spec.inclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is inclusive between (`min <= value && value <= max`) the specified range.
  [<CustomOperation("inclusiveBetween")>]
  member __.InclusiveBetween (state, min, max, message) = Spec.inclusiveBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value < max`) the specified range.
  [<CustomOperation("exclusiveBetweenOf")>]
  member __.ExclusiveBetweenOf (state, selector, min, max, message) = Spec.exclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is exclusive between (`min < value && value < max`) the specified range.
  [<CustomOperation("exclusiveBetween")>]
  member __.ExclusiveBetween (state, min, max, message) = Spec.exclusiveBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<CustomOperation("regexOf"); Obsolete("Use 'matchesOf' instead")>]
  member __.RegexOf (state, selector, pattern, message) = Spec.matchesOf selector pattern message state
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<CustomOperation("regex"); Obsolete("Use 'matches' instead")>]
  member __.Regex (state, pattern, message) = Spec.matches pattern message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<CustomOperation("matchesOf")>]
  member __.MatchesOf (state, selector, pattern, message) = Spec.matchesOf selector pattern message state
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<CustomOperation("matches")>]
  member __.Mathces (state, pattern, message) = Spec.matches pattern message state
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string containing only charaters in the alphabet.
  [<CustomOperation("alphabeticalOf")>]
  member __.AlphabeticalOf (state, selector, message) = Spec.alphabeticalOf selector message state
  /// Adds a requirement to check for strings containing only charaters in the alphabet.
  [<CustomOperation("alphabetical")>]
  member __.Alphabetical (state, message) = Spec.alphabetical message state
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value.
  [<CustomOperation("alphanumOf")>]
  member __.AlphanumOf (state, selector, message) = Spec.alphanumOf selector message state
  /// Adds a requirement to check for an alphanumerical value.
  [<CustomOperation("alphanum")>]
  member __.Alphanum (state, message) = Spec.alphanum message state
  /// Adds a requirement to the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value or special charater.
  [<CustomOperation("alphanumSpecialOf")>]
  member __.AlphanumSpecialOf (state, selector, message) = Spec.alphanumSpecialOf selector message state
  /// Adds a requirement to check for alphanumerical values or special charaters
  [<CustomOperation("alphanumSpecial")>]
  member __.AlphanumSpecial (state, message) = Spec.alphanumSpecial message state
  member __.Yield (_) = start
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
  /// Computation builder to build an `Spec<_>` instance that gets validated a custom domain model when runned.
  let specModelWith<'a, 'b> createModel value = new SpecBuilder<'a, Result<'b, string list>> (Spec.createModelWith createModel value)
  /// Makes a new specification that's describes an invariant relation between two given specifications.
  let specInvariant spec1 spec2 = new SpecBuilder<_, Spec<_>> (id, Spec.invariant spec1 spec2)
    
/// Thrown when the value of a validation result is called on an invalid validation result.
exception InvalidValidationResultException of string

#nowarn "1001"

/// Result type when a value is validated against a domain specification `Spec<_>`.
[<Struct; NoEquality; NoComparison>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
type ValidationResult<'T> internal (result : Result<'T, string list>) =
  /// Initializes a new instance of the `ValidationResult` class.
  new (value : 'T) = ValidationResult<'T> (Ok value)
  /// Initializes a new instance of the `ValidationResult` class.
  new (errors : string seq) = ValidationResult<'T> (Error (List.ofSeq errors))
  /// Initializes a new instance of the `ValidationResult` class.
  internal new ([<ParamArray>] errors : string [] []) = ValidationResult<'T> (Error (Array.concat errors |> List.ofArray))
  /// Gets the F# result from this validation result.
  member internal __.Result = result
  /// Gets the value that was validated (possible `null` when the validation failed).
  member __.Value : 'T = Result.getOrValue Unchecked.defaultof<'T> result
  /// Gets a value indicating whether the validation succeeded.
  member __.IsValid = Result.isOk result
  /// Gets the series of validation errors that describe to what domain requirements the validated value doesn't satisfy.
  member __.Errors : string array = Result.either (fun _ -> Array.empty) Array.ofList result

  /// Creates a successful validation result with a specified value.
  static member Success (value : 'T) = ValidationResult<'T> (value = value)
  /// Creats an unsuccessful validation result with the specified errors.
  static member Failure ([<ParamArray>] errors : string []) = ValidationResult<'T> (errors = errors)

  /// Tries to get the value that was validated.
  member __.TryGetValue (output : outref<'T>) =
    match result with
    | Ok x -> output <- x; true
    | _ -> output <- Unchecked.defaultof<'T>; false
  /// Gets the validated value or the absence of the value.
  member __.ToMaybe ([<Optional>] handleErrors : Action<_>) =
    let handleErrors = if isNull handleErrors then fun _ -> () else handleErrors.Invoke
    match result with
    | Ok x -> Maybe.Just x
    | Error errs -> handleErrors (Array.ofList errs); Maybe<'T>.Nothing

/// Result type when a value is validated against a domain specification `Spec<_>`.
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
type ValidationResult private () =
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TResult> 
    ( (validation1 : ValidationResult<'TFirst>), 
      (validation2 : ValidationResult<'TSecond>), 
      (resultSelector : Func<_, _, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value))
    else ValidationResult<'TResult> (validation1.Errors, validation2.Errors)
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TThird, 'TResult>
    ( (validation1 : ValidationResult<'TFirst>),
      (validation2 : ValidationResult<'TSecond>),
      (validation3 : ValidationResult<'TThird>),
      (resultSelector : Func<'TFirst, 'TSecond, 'TThird, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid && validation3.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value, validation3.Value))
    else ValidationResult<'TResult> (validation1.Errors, validation2.Errors, validation3.Errors)
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TThird, 'TFourth, 'TResult>
    ( (validation1 : ValidationResult<'TFirst>),
      (validation2 : ValidationResult<'TSecond>),
      (validation3 : ValidationResult<'TThird>),
      (validation4 : ValidationResult<'TFourth>),
      (resultSelector : Func<'TFirst, 'TSecond, 'TThird, 'TFourth, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid && validation3.IsValid && validation4.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value, validation3.Value, validation4.Value))
    else ValidationResult<'TResult> (validation1.Errors, validation2.Errors, validation3.Errors, validation4.Errors)

/// Delegate of: 'T -> bool * string.
type Requirement<'T> = delegate of 'T -> bool * string

/// Extensions on the `ValidationResult<_>` type to use in C# context.
[<Extension>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
type ValidationResultExtensions =
  /// Projects the validated result to another value.
  [<Extension>]
  static member Select (this : ValidationResult<'T>, selector : Func<'T, 'TResult>) =
    if isNull selector then nullArg "selector"
    if this.IsValid then ValidationResult<'TResult> (selector.Invoke this.Value)
    else ValidationResult<'TResult> this.Errors
  /// Projects the validated reuslt to another validation result.
  [<Extension>]
  static member SelectMany (this : ValidationResult<'T>, selector : Func<'T, ValidationResult<'TResult>>) =
    if isNull selector then nullArg "selector"
    if this.IsValid then selector.Invoke (this.Value)
    else ValidationResult<'TResult> this.Errors
  /// Projects the validated reuslt to another validation result.
  [<Extension>]
  static member Then (this : ValidationResult<'T>, selector) = 
    if isNull selector then nullArg "selector"
    this.SelectMany (selector)
  /// Filters out the validated value with yet another predicate and a series of validation error messages to create a fresh validation result in case the predicate doesn't hold.
  [<Extension>]
  static member Where (this : ValidationResult<'T>, predicate : Func<_, _>, [<ParamArray>] errors) =
    if isNull predicate then nullArg "predicate"
    if isNull errors then nullArg "errors"
    if Array.isEmpty errors then invalidArg "errors" "at least a single validation error message should be given"
    ValidationResult<'T> (Result.filter predicate.Invoke (List.ofArray errors) this.Result)
  /// Filters out the validated value with yet another specification.
  [<Extension>]
  static member Where (this : ValidationResult<'T>, otherSpec : Spec<'T>) =
    ValidationResult<'T> (Result.bind (fun x -> Spec.validate x otherSpec) this.Result)
  /// Combines validation results into a new validation result.
  [<Extension>]
  static member Zip (this : ValidationResult<'T>, other : ValidationResult<'TOther>) =
    if this.IsValid && other.IsValid
    then ValidationResult<'T * 'TOther> ((this.Value, other.Value))
    else ValidationResult<'T * 'TOther> (this.Errors, other.Errors)
  /// Combines validation results into a new validation result.
  [<Extension>]
  static member And (this : ValidationResult<'T>, other : ValidationResult<'TOther>) = this.Zip (other)
  /// Combines validation results into a new validation result.
  [<Extension>]
  static member Zip (this : ValidationResult<'T>, other : ValidationResult<'TOther>, selector : Func<'T, 'TOther, 'TResult>) =
    if isNull selector then nullArg "selector"
    if this.IsValid && other.IsValid
    then ValidationResult<'TResult> (selector.Invoke (this.Value, other.Value))
    else ValidationResult<'TResult> (this.Errors, other.Errors)
  /// Aggregates the validated value to another value when the validation result was successful.
  [<Extension>]
  static member Aggregate (this : ValidationResult<'T>, seed : 'TAccummulate, aggregator : Func<'TAccummulate, 'T, 'TAccummulate>) =
    if isNull seed then nullArg "seed"
    if isNull aggregator then nullArg "aggregator"
    if this.IsValid then aggregator.Invoke (seed, this.Value) else seed
  /// Traverse the given sequence, running the a given selector function over the elements, collecting the outcomes into a new validation result.
  [<Extension>]
  static member Traverse (source : IEnumerable<'T>, selector : Func<'T, ValidationResult<'TResult>>) : ValidationResult<IEnumerable<'TResult>> =
    if isNull source then nullArg "source"
    if isNull selector then nullArg "selector"
    ValidationResult<IEnumerable<'TResult>> (result = 
      Result.traverseSeq (selector.Invoke >> fun x -> x.Result) source)
  /// Transforms a sequence of validation results into a validation results of a sequence.
  [<Extension>]
  static member Sequence (source : IEnumerable<ValidationResult<'T>>) =
    if isNull source then nullArg "source"
    ValidationResult<IEnumerable<'T>> (result=Result.sequenceSeq (source |> Seq.map (fun x -> x.Result)))

/// Exception thrown when the validation of a value against a domain specification failed.
exception ValidationFailureException of string

/// Extensions on the `Spec<_>` type to use in C# context.
[<Extension>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
type SpecExtensions =
  /// Adds a custom requirement to the specification.
  [<Extension>]
  static member Add (spec : Spec<'T>, requirement : Func<'T, ValueTuple<bool, string>>) =
    if isNull requirement then nullArg "requirement"
    Spec.add (requirement.Invoke >> fun t -> 
      let t = t.ToTuple() in if isNull (snd t) then invalidArg "message" "" else t) spec

  /// Adds a custom requirement to the specification.
  [<Extension>]
  static member Add (spec : Spec<'T>, requirement : Func<'T, bool>, message) =
    if isNull requirement then nullArg "requirement"
    if isNull message then nullArg "message"
    Spec.add (fun x -> requirement.Invoke (x), message) spec

  /// Adds another specification on which the current specification depends on.
  /// This dependent specification will run before the current specification and contain only the errors of the dependents in case of a validation failure.
  [<Extension>]
  static member DependsOn (spec : Spec<'T>, dependent : Spec<'T>) =
    Spec.dependsOn dependent spec

  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  [<Extension>]
  static member Merge (spec1 : Spec<'T>, spec2 : Spec<'T>) =
    Spec.merge spec1 spec2

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  [<Extension>]
  static member Equal<'T, 'TResult when 'TResult : equality> ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), value, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.equalOf selector.Invoke value message spec
  
  /// Adds a requirement to check equality to a specified value.
  [<Extension>]
  static member Equal<'T when 'T : equality> ((spec : Spec<'T>), value, message) =
    if message = null then nullArg "message"
    Spec.equal value message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  [<Extension>]
  static member NotEqual<'T, 'TResult when 'TResult : equality> ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), value, message) =  
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notEqualOf selector.Invoke value message spec

  /// Adds a requirement to check no equality to a specified value.
  [<Extension>]
  static member NotEqual<'T when 'T : equality> ((spec : Spec<'T>), value, message) = 
    if message = null then nullArg "message"
    Spec.notEqual value message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  [<Extension>]
  static member NotNull ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notNullOf selector.Invoke message spec

  /// Adds a requirement to check for not `null`.
  [<Extension>]
  static member NotNull<'T when 'T : null and 'T : equality> ((spec : Spec<'T>), message) =
    if message = null then nullArg "message"
    Spec.notNull message spec

  /// Adds a requirement to check for non-empty string.
  [<Extension>]
  static member NotEmpty ((spec : Spec<string>), message) =
    if message = null then nullArg "message"
    Spec.notEmpty message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  [<Extension>]
  static member NotEmpty ((spec : Spec<'T>), (selector : Func<'T, string>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notEmptyOf selector.Invoke message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  [<Extension>]
  static member NotWhiteSpace ((spec : Spec<'T>), (selector : Func<'T, string>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notWhiteSpaceOf selector.Invoke message spec

  /// Adds a requirement to check if the string is a not-whitespace string.
  [<Extension>]
  static member NotWhiteSpace ((spec : Spec<string>), message) =
    if message = null then nullArg "message"
    Spec.notWhiteSpace message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  [<Extension>]
  static member NotNullOrEmpty ((spec : Spec<'T>), (selector : Func<'T, string>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notNullOrEmptyOf selector.Invoke message spec

  /// Adds a requirement to check if the string is a not-null, not-empty string.
  [<Extension>]
  static member NotNullOrEmpty ((spec : Spec<string>), message) =
    if message = null then nullArg "message"
    Spec.notNullOrEmpty message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty, not-whitespace string.
  [<Extension>]
  static member NotNullOrWhiteSpace ((spec : Spec<'T>), (selector : Func<'T, string>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.notNullOrWhiteSpaceOf selector.Invoke message spec

  /// Adds a requirement to check if the string is a not-null, not-empty, not-whitespace string.
  [<Extension>]
  static member NotNullOrWhiteSpace (spec, message) =
    if message = null then nullArg "message"
    Spec.notNullOrWhiteSpace message spec

  /// Adds a requirement to check if the string starts with the specified header.
  [<Extension>]
  static member StartsWith (spec, header, message) =
    if isNull header then nullArg "header"
    if isNull message then nullArg "message"
    Spec.startsWith header message spec

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string that starts with a specified header.
  [<Extension>]
  static member StartsWith (spec : Spec<'T>, selector : Func<'T, string>, header, message) =
    if isNull selector then nullArg "selector"
    if isNull header then nullArg "header"
    if isNull message then nullArg "message"
    Spec.startsWithOf selector.Invoke header message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty ((spec : Spec<IEnumerable<'T>>), message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty ((spec : Spec<ICollection<'T>>), message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty ((spec : Spec<IList<'T>>), message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty ((spec : Spec<'T[]>), message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement to check for non-empty sequences.
  [<Extension>]
  static member NonEmpty ((spec : Spec<IDictionary<'TKey, 'TValue>>), message) =
    if message = null then nullArg "message"
    Spec.nonEmpty message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), (predicate : Func<'TResult, bool>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), (predicate : Func<'TResult, bool>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), (predicate : Func<'TResult, bool>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec
  
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), (predicate : Func<'TResult, bool>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), (predicate : Func<KeyValuePair<'TKey, 'TValue>, bool>), message) =
    if selector = null then nullArg "selector"
    if predicate = null then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<IEnumerable<'T>>), (predicate : Func<'T, bool>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<ICollection<'T>>), (predicate : Func<'T, bool>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<IList<'T>>), (predicate : Func<'T, bool>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<'T[]>), (predicate : Func<'T, bool>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  [<Extension>]
  static member All ((spec : Spec<IDictionary<'TKey, 'TValue>>), (predicate : Func<KeyValuePair<'TKey, 'TValue>, bool>), message) =
    if predicate = null then nullArg "predicate"
    if message = null then nullArg "message"
    Spec.forall predicate.Invoke message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), length, message) = 
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthOf selector.Invoke length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<IEnumerable<'T>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<IList<'T>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec
  
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<ICollection<'T>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<'T[]>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec
  
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<Extension>]
  static member Length ((spec : Spec<IDictionary<'TKey, 'TValue>>), length, message) = 
    if message = null then nullArg "message"
    Spec.length length message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), min, message) =
    if selector = null then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<IEnumerable<'T>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<ICollection<'T>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<IList<'T>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<'T[]>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<Extension>]
  static member LengthMin ((spec : Spec<IDictionary<'TKey, 'TValue>>), min, message) =
    if message = null then nullArg "message"
    Spec.lengthMin min message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec
 
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<IEnumerable<'T>>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<IList<'T>>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<ICollection<'T>>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<'T[]>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<Extension>]
  static member LengthMax ((spec : Spec<IDictionary<'TKey, 'TValue>>), max, message) =
    if message = null then nullArg "message"
    Spec.lengthMax max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T>), (selector : Func<'T, IEnumerable<'TResult>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec
  
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T>), (selector : Func<'T, IList<'TResult>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T>), (selector : Func<'T, ICollection<'TResult>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T>), (selector : Func<'T, 'TResult[]>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T>), (selector : Func<'T, IDictionary<'TKey, 'TValue>>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<IEnumerable<'T>>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<ICollection<'T>>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<IList<'T>>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<'T[]>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<Extension>]
  static member LengthBetween ((spec : Spec<IDictionary<'TKey, 'TValue>>), min, max, message) =
    if message = null then nullArg "message"
    Spec.lengthBetween min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (`limit > value`) the specified limit.
  [<Extension>]
  static member LessThan ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), limit, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lessThanOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is less than (`limit > value`) the specified limit.
  [<Extension>]
  static member LessThan ((spec : Spec<'T>), limit, message) =
    if message = null then nullArg "message"
    Spec.lessThan limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (`limit < value`) the specified limit.
  [<Extension>]
  static member GreaterThan ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOf selector.Invoke limit message spec
  
  /// Adds a requirement to check if the value is greater than (`limit < value`) the specified limit.
  [<Extension>]
  static member GreaterThan ((spec : Spec<'T>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThan limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (`limit >= value`) the specified limit.
  [<Extension>]
  static member LessThanOrEqual ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), limit, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.lessThanOrEqualOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is less than or equal to (`limit >= value`) to the specified limit.
  [<Extension>]
  static member LessThanOrEqual ((spec : Spec<'T>), limit, message) =
    if message = null then nullArg "message"
    Spec.lessThanOrEqual limit message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (`limit <= value`) the specified limit.
  [<Extension>]
  static member GreaterThanOrEqual ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOrEqualOf selector.Invoke limit message spec

  /// Adds a requirement to check if the value is greater than or equal to (`limit <= value`) the specified limit.
  [<Extension>]
  static member GreaterThanOrEqual ((spec : Spec<'T>), limit, message) =
    if message = null then nullArg "message"
    Spec.greaterThanOrEqual limit message spec
  
  /// Adds a requirement to check if the value is inclusive between (`min <= value && value <= max`) the specified range.
  [<Extension>]
  static member InclusiveBetween ((spec : Spec<'T>), min, max, message) =
    if message = null then nullArg "message"
    Spec.inclusiveBetween min max message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (`min <= value && value <= max`) the specified range.
  [<Extension>]
  static member InclusiveBetween ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.inclusiveBetweenOf selector.Invoke min max message spec

  /// Adds a requirement to check if the value is exclusive between (`min < value && value < max`) the specified range.
  [<Extension>]
  static member ExclusiveBetween ((spec : Spec<'T>), min, max, message) =
    if message = null then nullArg "message"
    Spec.exclusiveBetween min max message spec
  
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (`min < value && value < max`) the specified range.
  [<Extension>]
  static member ExclusiveBetween ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), min, max, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.exclusiveBetweenOf selector.Invoke min max message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Extension; Obsolete("Use 'Matches' instead")>]
  static member Regex ((spec : Spec<'T>), (selector : Func<'T, string>), pattern, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.matchesOf selector.Invoke pattern message spec

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Extension; Obsolete("Use 'Matches' instead")>]
  static member Regex ((spec : Spec<string>), pattern, message) =
    if message = null then nullArg "message"
    Spec.matches pattern message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Extension>]
  static member Regex ((spec : Spec<'T>), (selector : Func<'T, string>), expr : Regex, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.matchesOf selector.Invoke (expr.ToString()) message spec

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Extension>]
  static member Regex ((spec : Spec<string>), expr : Regex, message) =
    if message = null then nullArg "message"
    Spec.matches (expr.ToString()) message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Extension>]
  static member Matches ((spec : Spec<'T>), (selector : Func<'T, string>), pattern, message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.matchesOf selector.Invoke pattern message spec

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Extension>]
  static member Matches ((spec : Spec<string>), pattern, message) =
    if message = null then nullArg "message"
    Spec.matches pattern message spec

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string containing only charaters in the alphabet.
  [<Extension>]
  static member Alphabetical (spec : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphabeticalOf selector.Invoke message spec

  /// Adds a requirement to check for strings containing only charaters in the alphabet.
  [<Extension>]
  static member Alphabetical (spec, message) =
    if isNull message then nullArg "message"
    Spec.alphabetical message spec

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value.
  [<Extension>]
  static member Alphanum ((spec : Spec<'T>), selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphanumOf selector.Invoke message spec

  /// Adds a requirement to check for an alphanumerical value.
  [<Extension>]
  static member Alphanum (spec, message) =
    if isNull message then nullArg "message"
    Spec.alphanum message spec

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value or special charaters.
  [<Extension>]
  static member AlphanumSpecial (spec : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphanumSpecialOf selector.Invoke message spec

  /// Adds a requirement to check for alphanumerical values.
  [<Extension>]
  static member AlphanumSpecial (spec, message) =
    if isNull message then nullArg "message"
    Spec.alphanumSpecial message spec

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  [<Extension>]
  static member IsType ((spec : Spec<'T>), (selector : Func<'T, 'TResult>), message) =
    if selector = null then nullArg "selector"
    if message = null then nullArg "message"
    Spec.isTypeOf selector.Invoke message spec

  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  [<Extension>]
  static member IsType ((spec : Spec<'T>), message) =
    if message = null then nullArg "message"
    Spec.isType message spec

  /// Change the way the validation of requirements should happen.
  [<Extension>]
  static member Cascade ((spec : Spec<'T>), mode) = 
    Spec.cascade mode spec

  /// Maps the requirements of this specification to antother requirement function structure.
  [<Extension>]
  static member Select (spec : Spec<'T>, selector : Func<Requirement<'T>, Requirement<'TResult>>) =
    if isNull selector then nullArg "selector"
    Spec.map (fun r -> let f = selector.Invoke(Requirement r) in if isNull f then nullArg "selector return value" else f.Invoke) spec

  /// Maps the to-be-validated value before the requirements are verified, therefore controlling the input value of this specification and further added requirements.
  [<Extension>]
  static member PreSelect (spec, preSelector : Func<_, _>) =
    if isNull preSelector then nullArg "preSelector"
    Spec.comap preSelector.Invoke spec

  /// Adds a pre-valildation function to all the requirements of this specification.
  [<Extension>]
  static member PreValidate (spec, preValidator : Func<_, ValidationResult<_>>) =
    if isNull preValidator then nullArg "preValidator"
    let f x = preValidator.Invoke(x).Result |> Result.mapError (fun errs -> String.Join (Environment.NewLine, errs))
    Spec.preval f spec

  /// Transforms this specification to work with a array of values.
  [<Extension>]
  static member ForArray (spec) = Spec.array spec

  /// Maps this specification to work with sequences of values.
  [<Extension>]
  static member ForEnumerable (spec) = Spec.seq spec

  /// Determine whether the specified value satisfies the domain specification.
  [<Extension>]
  static member IsSatisfiedBy ((spec : Spec<'T>), value) =
    Spec.isSatisfiedBy value spec

  /// Validate the specified value to the domain specification.
  [<Extension>]
  static member Validate ((spec : Spec<'T>), value) =
    ValidationResult<'T> (result=Spec.validate value spec)

  /// Validate the specified value to the domain specification.
  [<Extension>]
  static member ValidateThrow<'T, 'TException when 'TException :> exn> ((spec : Spec<'T>), value, message : string) : 'T =
    if message = null then nullArg "message" 
    Spec.validate value spec
    |> Result.getOrElse (fun _ -> 
      raise (Activator.CreateInstance(typeof<'TException>, [| message :> obj |]) :?> 'TException))

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModel ((spec : Spec<'T>), value, (creator : Func<'T, 'TResult>)) =
    if creator = null then nullArg "creator"
    ValidationResult<'TResult> (result=Spec.createModel creator.Invoke value spec)

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModel (spec : Spec<'T>, value, creator : Func<'T, ValidationResult<'TResult>>) =
    if isNull creator then nullArg "creator"
    ValidationResult<'TResult> (result=Spec.createModelWith (creator.Invoke >> fun r -> r.Result) value spec)

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member TryCreateModel ((spec : Spec<'T>), value, (creator : Func<'T, 'TResult>), result : outref<'TResult>) =
    if creator = null then nullArg "creator"
    match Spec.createModel creator.Invoke value spec with
    | Ok x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'TResult>; false

  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult, 'TException when 'TException :> exn> ((spec : Spec<'T>), value, (creator : Func<'T, 'TResult>), message) =
    if creator = null then nullArg "creator"
    Spec.createModel creator.Invoke value spec
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (Activator.CreateInstance (typeof<'TException> , [| message :> obj |]) :?> 'TException))
  
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  [<Extension>]
  static member CreateModelOrThrow ((spec : Spec<'T>), value, (creator : Func<'T, 'TResult>), message) =
    if creator = null then nullArg "creator"
    Spec.createModel creator.Invoke value spec
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (ValidationFailureException message))

  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  [<Extension>]
  static member CreateModel (specification, untrusted, (creator : Func<'T, 'TResult>)) =
    if creator = null then nullArg "creator"
    ValidationResult<'TResult> (result=Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted)

  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  [<Extension>]
  static member TryCreateModel (specification, untrusted, (creator : Func<'T, 'TResult>), result : outref<'TResult>) =
    if creator = null then nullArg "creator"
    match Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted with
    | Ok x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'TResult>; false

  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  [<Extension>]
  static member CreateModelOrThrow (specification, untrusted, (creator : Func<'T, 'TResult>), message) =
    if creator = null then nullArg "creator"
    Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (ValidationFailureException message))

  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult, 'TException when 'TException :> exn> (specification, untrusted, (creator : Func<'T, 'TResult>), message) =
    if creator = null then nullArg "creator"
    Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (Activator.CreateInstance (typeof<'TException> , [| message :> obj |]) :?> 'TException))