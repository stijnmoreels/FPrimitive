namespace FPrimitive

open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.RegularExpressions

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.FSharp.Core

/// Representation on how the model should be validated.
type CascadeMode = 
  /// Continue even when a validation requirement failed.
  | Continue
  /// Stop at the first failed validation requirement.
  | FirstFailure

/// Type alias for the validation requirement.
type Req<'T> = 'T -> bool * string

type ErrorsByTag = Map<string, string list>

/// Representation of a domain specification that contains the validation for the model.
type Spec<'T> =
  private 
    { /// Gets the name to describe the set of requirements for a specific type.
      Tag : string
      /// Gets all the requirements of the domain specification that the model should uphold.
      Requirements : ('T -> bool * string) list
      /// Gets the mode in which the validation of the model should happen.
      Cascade : CascadeMode
      /// Gets the child specifications.
      Dependents : Spec<'T> list
      /// Gets the logger instance to write diagnostic trace messages and failures during the model validation.
      Logger : ILogger }

/// Operations on the `Spec<_>` type. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Spec =
  /// Creates a specification with a name and logger.
  let create name logger = 
     { Tag = name
       Requirements = [] 
       Cascade = FirstFailure
       Dependents = [] 
       Logger = logger }
  
  /// Start defininig a specification for a type with a name.
  let tag name =
    { Tag = name
      Requirements = []
      Cascade = FirstFailure
      Dependents = []
      Logger = NullLogger.Instance }

  /// Start defininig a specification for a type.
  let def<'a> : Spec<'a> = tag "[without tag]"

  /// Starts defining a specification for a type, adding a logger while doing so.
  [<Obsolete("Removing logging capabilities in future release")>]
  [<ExcludeFromCodeCoverage>]
  let defl<'a> logger : Spec<'a> = create "[without tag]" logger

  /// Creates an validation error from a tag and message.
  let error tag error = Error <| Map.create tag [error]

  /// Adds a custom requirement to the specification.
  let add req specification =
    { specification with Requirements = req :: specification.Requirements }
    
  /// Adds a conditional requirement to the specification.
  let conditional predicate requirement specification =
    { specification with Requirements = (fun x -> if predicate x then requirement x else true, "") :: specification.Requirements }

  /// Adds a logger to the specification.
  [<Obsolete("Removing logging capabilities in future release")>]
  [<ExcludeFromCodeCoverage>]
  let logger instance specification = { specification with Logger = instance }

  /// Adds a custom requirement to the specification.
  let verify verifier msg specification = add (fun x -> verifier x, msg) specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  let equalOf selector value message specification =
    add (fun x -> selector x |> (=) value, message) specification

  /// Adds a requirement to check equality to a specified value.
  let equal value message specification =
    equalOf id value message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  let notEqualOf selector value message specification =
    add (fun x -> selector x |> (<>) value, message) specification

  /// Adds a requirement to check no equality to a specified value.
  let notEqual value message specification =
    notEqualOf id value message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  let notNullOf selector message specification =
    add (fun x -> selector x <> null, message) specification

  /// Adds a requirement to check for not `null`.
  let notNull message specification =
    notNullOf id message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  let notEmptyOf selector message specification =
    add (fun x -> selector x |> (<>) String.Empty, message) specification

  /// Adds a requirement to check for non-empty string.
  let notEmpty message specification =
    notEmptyOf id message specification

  let private whitespaceRegex = Regex ("^\s+$", RegexOptions.Compiled) 

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  let notWhiteSpaceOf selector message specification =
    add (fun x -> selector x |> whitespaceRegex.IsMatch |> not, message) specification

  /// Adds a requirement to check if the string is a not-whitespace string.
  let notWhiteSpace message specification =
    notWhiteSpaceOf id message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  let notNullOrEmptyOf selector message specification =
    add (fun x -> selector x |> String.IsNullOrEmpty |> not, message) specification

  /// Adds a requirement to check if the string is a not-null, not-empty string.
  let notNullOrEmpty message specification =
    notNullOrEmptyOf id message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty, not-whitespace string.
  let notNullOrWhiteSpaceOf selector message specification =
    add (fun x -> selector x |> String.IsNullOrWhiteSpace |> not, message) specification

  /// Adds a requirement to check if the string is a not-null, not-empty, not-whitespace string.
  let notNullOrWhiteSpace message specification =
    notNullOrWhiteSpaceOf id message specification

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string starting with the specified header.
  let startsWithOf selector header message specification =
    add (selector >> fun (s : string) -> s.StartsWith (header : string), message) specification

  /// Adds a requirement to check if the string starts with the specified header.
  let startsWith header message specification =
    startsWithOf id header message specification

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string ending with the specified trailer.
  let endsWithOf selector trailer message specification =
    add (selector >> fun (s : string) -> s.EndsWith (trailer : string), message) specification

  /// Adds a requirement to check if the string ends with the specified trailer.
  let endsWith trailer message specification =
    endsWithOf id trailer message specification

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a sequence equal (item-wise) to another sequence.
  let seqEqualOf selector (ys : _ seq) message specification =
    add (fun x -> selector x |> fun xs -> System.Linq.Enumerable.SequenceEqual (xs, ys), message) specification

  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  let seqEqual ys message specification =
    seqEqualOf id ys message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  let nonEmptyOf selector message specification =
    add (fun x -> selector x |> Seq.isEmpty |> not, message) specification

  /// Adds a requirement to check for non-empty sequences.
  let nonEmpty message specification =
    nonEmptyOf id message specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  let forallOf selector predicate message specification =
    add (fun x -> selector x |> Seq.forall predicate, message) specification

  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  let forall predicate message specification =
    forallOf id predicate message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none of the elements of the sequence should satisfy the specified predicate.
  let fornoneOf selector predicate message specification =
    add (fun x -> selector x |> Seq.forall (not << predicate), message) specification

  /// Adds a requirement for the sequence to check that none of the elements of the sequence satisfy the specified predicate.
  let fornone predicate message specification =
    fornoneOf id predicate message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none of the elements of the sequence should be equal to `null`.
  let fornoneNullOf selector message specification =
    fornoneOf selector (fun x -> obj.ReferenceEquals (x, null)) message specification

  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  let fornoneNull message specification =
    fornoneNullOf id message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should be present in the sequence.
  let containsOf selector item message specification =
    add (fun x -> selector x |> Seq.contains item, message) specification

  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  let contains item message specification =
    containsOf id item message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should not be present in the sequence.
  let notContainsOf selector item message specification =
    add (fun x -> selector x |> Seq.contains item |> not, message) specification

  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  let notContains item message specification =
    notContainsOf id item message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given set of items should all be present in the sequence.
  let containsAllOf selector items message specification =
    add (fun x -> selector x |> fun xs -> Seq.forall (fun y -> Seq.contains y xs) items, message) specification

  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  let containsAll items message specification =
    containsAllOf id items message specification

  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is present in the string.
  let stringContainsOf selector sub message specification =
    add (fun x -> selector x |> fun (str : string) ->  not <| isNull str && str.Contains (sub : string), message) specification

  /// Adds a requirement for the string to verify if the given substring is present in the string.
  let stringContains sub message specification =
    stringContainsOf id sub message specification

  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is not present in the string
  let stringNotContainsOf selector sub message specification =
    add (fun x -> selector x |> fun (str : string) -> not <| isNull str && not <| str.Contains (sub : string), message) specification

  /// Adds a requirement for the string to verify if a given substring is not present in the string.
  let stringNotContains sub message specification =
    stringNotContainsOf id sub message specification

  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substrings are all present in the string.
  let stringContainsAllOf selector subs message specification =
    add (fun x -> selector x |> fun (str : string) -> not <| isNull str && Seq.forall (fun sub -> str.Contains (sub : string)) subs, message) specification

  /// Adds a requirement for the string to verify if the given substrings are all present in the string.
  let stringContainsAll subs message specification =
    stringContainsAllOf id subs message specification

  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  let inEnumOf selector enumType message specification =
    add (fun x -> Enum.IsDefined (enumType, selector x), message) specification

  /// Adds a requirement that verifies if the value is defined in the given enumeration.
  let inEnum enumType message specification =
    inEnumOf id enumType message specification

  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  let inEnumOfT<'T, 'TEnum when 'TEnum : enum<int32>> (selector : 'T -> 'TEnum) message (specification : Spec<'T>) =
    inEnumOf selector typeof<'TEnum> message specification

  /// Adds a requirement that verifies if the value is defined in the given enumeration.
  let inEnumT<'TEnum when 'TEnum : enum<int32>> message specification =
    inEnumOfT<'TEnum, 'TEnum> id message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that any item in the sequence to satisy the specified predicate.
  let existsOf selector predicate message specification =
    add (fun x -> selector x |> Seq.exists predicate, message) specification

  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  let exists predicate message specification =
    existsOf id predicate message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none item in the sequence satisfy the specified predicate.
  let notExistsOf selector predicate message specification =
    add (fun x -> selector x |> Seq.exists predicate |> not, message) specification

  /// Adds a requirement for the sequence to verify if none item satisfy the specified predicate.
  let notExists predicate message specification =
    notExistsOf id predicate message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that only a single element in the sequence should satisfy the specified predicate.
  let singleOf selector predicate message specification =
    add (fun x -> selector x |> fun xs -> Seq.filter predicate xs |> Seq.length = 1, message) specification

  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  let single predicate message specification =
    singleOf id predicate message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that all elements of the sequence should be unique.
  let uniqueOf selectSeq selectKey message specification =
    add (fun x -> selectSeq x |> Seq.groupBy selectKey |> Seq.forall (fun (_, g) -> Seq.length g = 1), message) specification

  /// Adds a requirement to check for sequences with only unique elements.
  let unique selectKey message specification =
    uniqueOf id selectKey message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that all the elements of the sequence should be unique.
  let uniqueItemsOf selector message specification =
    uniqueOf selector id message specification

  /// Adds a requirement to check if the sequence only contains unique elements.
  let uniqueItems message specification =
    uniqueItemsOf id message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  let structureOf selector plan message (specification : Spec<'a>) =
    let req xs =
      let xs = selector xs
      if Seq.length xs <> Seq.length plan
      then specification.Logger.LogWarning (
             "Cannot validate the `structure` requirement of type '{Type}' because the length of the incoming sequence doesn't match with the structure's plan", typeof<'a>.Name)
           false, "length of structure plan doesn't match length of input sequence"
      else Seq.mapi2 (fun i x f -> if f x then -1 else i) xs plan
           |> Seq.filter ((<>) -1)
           |> fun xs -> Seq.isEmpty xs, sprintf "%s (indexes: %s)" message (String.Join (", ", xs))
    add req specification

  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  let structure plan message specification =
    structureOf id plan message specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  let lengthOf selector l message specification =
    add (fun x -> selector x |> Seq.length |> (=) l, message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the string should have a length of the specified length.
  let stringLengthOf selector l message specification =
    add (fun x -> selector x |> String.length |> (=) l, message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  let lengthMinOf selector min message specification =
    add (fun x -> selector x |> Seq.length |> (<=) min , message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a minimum length.
  let stringLengthMinOf selector min message specification =
    add (fun x -> selector x |> String.length |> (<=) min, message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  let lengthMaxOf selector max message specification =
    add (fun x -> selector x |> Seq.length |> (>=) max, message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a maximum length.
  let stringLengthMaxOf selector max message specification =
    add (fun x -> selector x |> String.length |> (>=) max, message) specification

  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  let lengthBetweenOf selector min max message specification =
    add (fun x -> let l = selector x |> Seq.length in min <= l && l <= max, message) specification
  
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a length within the specified range (min, max).
  let stringLengthBetweenOf selector min max message specification =
    add (fun x -> let l = selector x |> String.length in min <= l && l <= max, message) specification

  /// Adds a requirement for the sequence to check if the length matches the specified length.
  let length l message specification =
    lengthOf id l message specification

  /// Adds a requirement for the string to check if the length matches the specified length.
  let stringLength l message specification =
    stringLengthOf id l message specification

  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  let lengthMin min message specification =
    lengthMinOf id min message specification

  /// Adds a requirement for the string to check if the sequence has a minimum length.
  let stringLengthMin min message specification =
    stringLengthMinOf id min message specification

  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  let lengthMax max message specification =
    lengthMaxOf id max message specification

  /// Adds a requirement for the string to check if the sequence has a maximum length.
  let stringLengthMax max message specification =
    stringLengthMaxOf id max message specification

  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  let lengthBetween min max message specification =
    lengthBetweenOf id min max message specification

  /// Adds a requirement for the string to check if the sequence has a length within the specified range (min, max).
  let stringLengthBetween min max message specification =
    stringLengthBetweenOf id min max message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (`limit > value`) the specified limit.
  let lessThanOf selector limit message specification =
    add (fun x -> selector x |> (>) limit, message) specification

  /// Adds a requirement to check if the value is less than (`limit > value`) the specified limit.
  let lessThan limit message specification = 
    lessThanOf id limit message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (`limit >= value`) the specified limit.
  let lessThanOrEqualOf selector limit message specification =
    add (fun x -> selector x |> (>=) limit, message) specification

  /// Adds a requirement to check if the value is less than or equal to (`limit >= value`) to the specified limit.
  let lessThanOrEqual limit message specification = 
    lessThanOrEqualOf id limit message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (`limit < value`) the specified limit.
  let greaterThanOf selector limit message specification =
    add (fun x -> selector x |> (<) limit, message) specification

  /// Adds a requirement to check if the value is greater than (`limit < value`) the specified limit.
  let greaterThan limit message specification =
    greaterThanOf id limit message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (`limit <= value`) the specified limit.
  let greaterThanOrEqualOf selector limit message specification =
    add (fun x -> selector x |> (<=) limit, message) specification

  /// Adds a requirement to check if the value is greater than or equal to (`limit <= value`) the specified limit.
  let greaterThanOrEqual limit message specification =
    greaterThanOrEqualOf id limit message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (min &lt;= value && value &lt;= max) the specified range.
  let inclusiveBetweenOf selector min max message specification =
    add (fun x -> let x = selector x in min <= x && x <= max, message) specification

  /// Adds a requirement to check if the value is inclusive between (min &lt;= value && value &lt;= max) the specified range.
  let inclusiveBetween min max message specification =
    inclusiveBetweenOf id min max message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (min &lt; value && value &lt; max) the specified range.
  let exclusiveBetweenOf selector min max message specification =
    add (fun x -> let x = selector x in min < x && x < max, message) specification

  /// Adds a requirement to check if the value is exclusive between (min &lt; value && value &lt; max) the specified range.
  let exclusiveBetween min max message specification =
    exclusiveBetweenOf id min max message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  let matchesOf selector pattern message specification =
    let regex = Regex (pattern)
    add (fun x -> selector x |> regex.IsMatch, message) specification

  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  let matches pattern message specification =
    matchesOf id pattern message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  [<Obsolete("Use 'matchesOf' instead")>]
  let regexOf selector pattern message specification =
    matchesOf selector pattern message specification
  
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  [<Obsolete("Use 'matches' instead")>]
  let regex pattern message specification =
    matchesOf id pattern message specification

  let private alphabet = ['a'..'z'] @ ['A'..'Z']

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string containing only charaters in the alphabet.
  let alphabeticalOf selector message specification =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphabet), message) specification

  /// Adds a requirement to check for strings containing only charaters in the alphabet.
  let alphabetical message specification =
    alphabeticalOf id message specification

  let private alphanumerical = alphabet @ List.map (string >> char) [0..9]

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value.
  let alphanumOf selector message specification =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphanumerical), message) specification

  /// Adds a requirement to check for alphanumerical values.
  let alphanum message specification =
    alphanumOf id message specification

  let private alphanumerical_extra =
    alphanumerical
    @ [ ' '; '!'; '\'';  '#'; '$'; '%'; '&'; '\''; '\"'; '('; ')'; '*'; '+';  ','; '-'; '.'; '/'; ':'; ';'; '<'; '='; '>'; '?'; '@'; '['; '\\'; ']'; '^'; '_'; '`'; '{'; '|'; '}'; '~' ]

  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value or special charater.
  let alphanumSpecialOf selector message specification =
    add (fun x -> selector x |> String.forall (fun c -> List.contains c alphanumerical_extra), message) specification
  
  /// Adds a requirement to check for alphanumerical values or special charaters.
  let alphanumSpecial message specification =
    alphanumSpecialOf id message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type.
  let isTypeOf selector expectedType message specification =
    add (fun x -> (selector x).GetType() = expectedType, message) specification

  /// Adds a requirement to check if the value is an instance of the specified type.
  let isType expectedType message specification =
    isTypeOf id expectedType message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  let isTypeOfT<'T, 'TResult, 'TExpected> (selector : 'T -> 'TResult) message specification =
    add (fun x -> match selector x |> box with | :? 'TExpected -> true, message | _ -> false, message) specification

  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  let isTypeT<'T, 'TExpected> message (specification : Spec<'T>) =
    isTypeOfT<'T, 'T, 'TExpected> id message specification

  /// Change the tag of the specification.
  let withTag name specification = { specification with Tag = name }

  /// Change the way the validation of requirements should happen.
  let cascade mode specification = { specification with Cascade = mode }

  /// Validate the specified value to the domain specification.
  let rec validate (value : 'T) { Tag = tag; Requirements = xs; Cascade = mode; Dependents = ds; Logger = log } : Result<'T, ErrorsByTag> =
    let checkRequirement f =
      let ok, err = 
        try f value with ex -> 
            log.LogError(ex, "Cannot check requirement for type '{Type}' because an exception was thrown during validation", typeof<'T>.Name) 
            false, "Faulted requirement due to an internal failure" 
      if ok then None 
      else Some err
    
    let tagOrProp error =
      let error = if isNull error then String.Empty else error
      let m = Regex.Match (error, "@\w+")
      if m.Success then sprintf "%s.%s" tag (m.Value.TrimStart '@')
      else tag

    let dependantResult =
      List.rev ds
      |> List.map (validate value)
      |> Result.traverse (Result.mapError Map.toList)
      |> Result.mapBoth (fun _ -> value) Map.ofListg

    match dependantResult with
    | Ok _ ->
        let errors =
          match mode with
          | Continue -> List.rev xs |> List.choose checkRequirement
          | FirstFailure -> 
            let folder f acc = Option.orElseWith (fun () -> checkRequirement f) acc
            List.foldBack folder xs None
            |> Option.toList
        if errors.Length = 0 then Ok value
        else let groupedErrors = List.groupBy tagOrProp errors |> Map.ofListg
             log.LogError ("Validation failure for '{Type}' {Tag}: {Errors}", typeof<'T>.Name, tag, groupedErrors)
             Error groupedErrors
    | Error groupedErrors ->
        log.LogError ("Validation failure for '{Type}' {Tag}: {Errors}", typeof<'T>.Name, tag, groupedErrors)
        Error groupedErrors

  /// Validate the specified untrusted value to the domain specification.
  let validateUntrust (untrusted : Untrust<'T>) (specification : Spec<'T>) : Result<'T, ErrorsByTag> =
    untrusted.tryGetValue (fun x -> validate x specification)

  /// Validate the specified value to the domain specification, discarding the error messages.
  let validateOption value specification =
    match validate value specification with
    | Ok x -> Some x
    | _ -> None

  /// Create a domain model after the validation of the specifed value to the domain specification succeeds.
  let createModel f value specification =
    validate value specification |> Result.map f

  /// Create a domain model after the validation of the specifed value to the domain specification succeeds,
  /// using a binding creation function with the possibility to fail.
  let createModelWith f value specification =
    validate value specification |> Result.bind f
  
  /// Makes an optional creation of a domain model using an optional specification that only happens when the given filter succeeds.
  /// Example: `Spec.optional Option.ofObj Ok "some possible null value"`
  let optional filter creator value =
    Option.fold (fun _ x -> creator x |> Result.map Some) (Ok None) (filter value)

  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Result<_, string list>` when runned and filter succeeds.
  let validateIf filter value specification =
    optional filter (fun x -> validate x specification) value

  /// Create a domain model when the given filter returns `Some x` and after the validation of the specifed value to the domain specification succeeds.
  let createModelIf filter f value specification =
    optional filter (fun x -> createModel f x specification) value

  /// Create a domain model when the given filter returns `Some x` and after the validation of the specifed value to the domain specification succeeds,
  /// using a binding creation function with the possibility to fail.
  let createModelWithIf filter f value specification =
    optional filter (fun x -> createModelWith f x specification) value

  /// Determine whether the specified value satisfies the domain specification.
  let isSatisfiedBy value specification =
    validate value specification = Ok value

  /// Adds another specification on which the current specification depends on.
  /// This dependent specification will run before the current specification and contain only the errors of the dependents in case of a validation failure.
  let dependsOn dependent specification =
    { specification with Dependents = dependent :: specification.Dependents }

  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  let merge spec1 spec2 =
    { def with Dependents = [spec1; spec2] }

  /// Maps the to-be-validated value before the requirements are verified, therefore controlling the input value of this specification and further added requirements.
  let rec comap f specification =
    { Tag = specification.Tag
      Requirements = List.map (fun r -> f >> r) specification.Requirements
      Cascade = specification.Cascade
      Dependents = List.map (comap f) specification.Dependents
      Logger = specification.Logger }

  /// Maps the requirements of this specification to another requirement function structure.
  let rec map (f : Req<'a> -> Req<'b>) specification =
    { Tag = specification.Tag
      Requirements = List.map f specification.Requirements
      Cascade = specification.Cascade
      Dependents = List.map (map f) specification.Dependents
      Logger = specification.Logger }

  /// Adds a pre-validation function to all the requirements of this specification.
  let preval f specification =
    map (fun r -> fun x -> match f x with | Ok y -> r y | Error msg -> false, msg) specification

  /// Adds a pre-filter function to all the requirements of this specification.
  let prefilter predicate specification =
    map (fun r -> fun x -> if predicate x then r x else true, "") specification

  /// Adds a conditional requirement to the specification.
  let filter predicate dependent specification =
    prefilter predicate dependent
    |> dependsOn <| specification

  /// Adds a conditional requirement to the specification that is the result of the specified selection.
  let filterOf selector predicate dependent specification =
    prefilter predicate dependent
    |> comap selector
    |> dependsOn <| specification

  /// Adds a conditional requirement to the specification that is the result of the specified selection and is not `default`.
  let filterT (selector : 'T -> 'TResult) dependent specification =
    prefilter ((<>) Unchecked.defaultof<'TResult>) dependent
    |> comap selector
    |> dependsOn <| specification

  /// Adds a dependent specification that will validate a subset of a resulting value of the original specification.
  let subset (f : 'T -> 'TResult) (dependent : Spec<'TResult>) (specification : Spec<'T>) =
    comap f dependent
    |> dependsOn <| specification

  let private combine_bool_message (x, err1) (y, err2) = x && y, err1 + Environment.NewLine + err2

  /// Maps this specification to work with a list of values.
  let rec list specification =
    let asList r xs =
      if List.isEmpty xs then true, ""
      else List.map r xs |> List.reduce combine_bool_message
    map asList specification

  /// Maps this specification to work with array of values.
  let rec array specification =
      let asArray r xs =
        if Array.isEmpty xs then true, ""
        else Array.map r xs |> Array.reduce combine_bool_message
      map asArray specification

  /// Maps this specification to work with sequences of values.
  let rec seq specification =
      let asSeq r xs =
        if Seq.isEmpty xs then true, ""
        else Seq.map r xs |> Seq.reduce combine_bool_message
      map asSeq specification

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

    { Tag = sprintf "%s+%s" spec1.Tag spec2.Tag
      Requirements = rs
      Cascade = cascade
      Dependents = ds
      Logger = spec1.Logger }

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

    { Tag = sprintf "%s+%s+%s" spec1.Tag spec2.Tag spec3.Tag
      Requirements = rs
      Cascade = cascade
      Dependents = ds
      Logger = spec1.Logger }

/// Computation expression builder for the domain specification `Spec<_>`.
[<ExcludeFromCodeCoverage>]
type SpecBuilder<'a, 'b> internal (validate : Spec<'a> -> 'b, ?start) =
  let start = Option.defaultValue Spec.def<'a> start
  /// Adds a custom requirement to the specification.
  [<CustomOperation("add")>]
  member __.Add (state, req) = Spec.add req state
  /// Adds a conditional requirement to the specification.
  [<CustomOperation("conditional")>]
  member __.AddConditional (state, predicate, requirement) = Spec.conditional predicate requirement state
  /// Adds a custom requirement to the specification.
  [<CustomOperation("verify")>]
  member __.Verify (state, verifier, msg) = Spec.verify verifier msg state
  /// Adds a conditional requirement to the specification.
  [<CustomOperation("filter")>]
  member __.Filter (state, predicate, dependent) = Spec.filter predicate dependent state
  /// Adds a conditional requirement to the specification that is the result of the specified selection.
  [<CustomOperation("filterOf")>]
  member __.FilterOf (state, selector, predicate, dependent) = Spec.filterOf selector predicate dependent state
  /// Change the way the validation of requirements should happen.
  [<CustomOperation("cascade")>]
  member __.Cascade (state, mode) = Spec.cascade mode state
  /// Change the tag of the specification.
  [<CustomOperation("tag")>]
  member __.Tag (state, name) = Spec.withTag name state
  /// Adds a logger to the specification.
  [<CustomOperation("logger")>]
  member __.Logger (state, instance) = Spec.logger instance state
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
  /// which defines that the result should be a string ending with the specified trailer.
  [<CustomOperation("endsWithOf")>]
  member __.EndsWithOf (state, selector, trailer, message) = Spec.endsWithOf selector trailer message state
  /// Adds a requirement to check if the string ends with the specified trailer.
  [<CustomOperation("endsWith")>]
  member __.EndsWith (state, trailer, message) = Spec.endsWith trailer message state
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a sequence equal (item-wise) to another sequence.
  [<CustomOperation("seqEqualOf")>]
  member __.SeqEqualOf (state, selector, ys, message) = Spec.seqEqualOf selector ys message state
  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  [<CustomOperation("seqEqual")>]
  member __.SeqEqual (state, ys, message) = Spec.seqEqual ys message state
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
  /// which defines that none of the elements of the sequence should satisfy the specified predicate.
  [<CustomOperation("fornoneOf")>]
  member __.ForNoneOf (state, selector, predicate, message) = Spec.fornoneOf selector predicate message state
  /// Adds a requirement for the sequence to check that none the elements of the sequence satisfy the specified predicate.
  [<CustomOperation("fornone")>]
  member __.ForNone (state, predicate, message) = Spec.fornone predicate message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none of the elements of the sequence should be equal to `null`.
  [<CustomOperation("fornoneNullOf")>]
  member __.ForNoneNullOf (state, selector, message) = Spec.fornoneNullOf selector message state
  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  [<CustomOperation("fornoneNull")>]
  member __.ForNoneNull (state, message) = Spec.fornoneNull message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should be present in the sequence.
  [<CustomOperation("containsOf")>]
  member __.ContainsOf (state, selector, item, message) = Spec.containsOf selector item message state
  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  [<CustomOperation("contains")>]
  member __.Contains (state, item, message) = Spec.contains item message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should not be present in the sequence.
  [<CustomOperation("notContainsOf")>]
  member __.NotContainsOf (state, selector, item, message) = Spec.notContainsOf selector item message state
  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  [<CustomOperation("notContains")>]
  member __.NotContains (state, item, message) = Spec.notContains item message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should not be present in the sequence.
  [<CustomOperation("containsAllOf")>]
  member __.ContainsAllOf (state, selector, items, message) = Spec.containsAllOf selector items message state
  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  [<CustomOperation("containsAll")>]
  member __.ContainsAll (state, items, message) = Spec.containsAll items message state
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is present in the string.
  [<CustomOperation("stringContainsOf")>]
  member __.StringContainsOf (state, selector, sub, message) = Spec.stringContainsOf selector sub message state
  /// Adds a requirement for the string to verify if the given substring is present in the string.
  [<CustomOperation("stringContains")>]
  member __.StringContains (state, sub, message) = Spec.stringContains sub message state
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is not present in the string
  [<CustomOperation("stringNotContainsOf")>]
  member __.StringNotContainsOf (state, selector, sub, message) = Spec.stringNotContainsOf selector sub message state
  /// Adds a requirement for the string to verify if a given substring is not present in the string.
  [<CustomOperation("stringNotContains")>]
  member __.StringNotContains (state, sub, message) = Spec.stringNotContains sub message state
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substrings are all present in the string.
  [<CustomOperation("stringContainsAllOf")>]
  member __.StringContainsAllOf (state, selector, subs, message) = Spec.stringContainsAllOf selector subs message state
  /// Adds a requirement for the string to verify if the given substrings are all present in the string.
  [<CustomOperation("stringContainsAll")>]
  member __.StringContainsAll (state, subs, message) = Spec.stringContainsAll subs message state
  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  [<CustomOperation("inEnumOf")>]
  member __.InEnumOf (state, selector, enumType, message) = Spec.inEnumOf selector enumType message state
  /// Adds a requirement that verifies if the value is defined in the given enumeration.
  [<CustomOperation("inEnum")>]
  member __.InEnum (state, enumType, message) = Spec.inEnum enumType message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that any item in the sequence to satisy the specified predicate.
  [<CustomOperation("existsOf")>]
  member __.ExistsOf (state, selector, predicate, message) = Spec.existsOf selector predicate message state
  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  [<CustomOperation("exists")>]
  member __.Exists (state, predicate, message) = Spec.exists predicate message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none item in the sequence to satisy the specified predicate.
  [<CustomOperation("notExistsOf")>]
  member __.NotExistsOf (state, selector, predicate, message) = Spec.notExistsOf selector predicate message state
  /// Adds a requirement for the sequence to verify if none element satisfy the specified predicate.
  [<CustomOperation("notExists")>]
  member __.NotExists (state, predicate, message) = Spec.notExists predicate message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that only a single element in the sequence should satisfy the specified predicate.
  [<CustomOperation("singleOf")>]
  member __.SingleOf (state, selector, predicate, message) = Spec.singleOf selector predicate message state
  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  [<CustomOperation("single")>]
  member  __.Single (state, predicate, message) = Spec.single predicate message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that all elements of the sequence should be unique.
  [<CustomOperation("uniqueOf")>]
  member __.UniqueOf (state, selectSeq, selectKey, message) = Spec.uniqueOf selectSeq selectKey message state
  /// Adds a requirement to check for sequences with only unique elements.
  [<CustomOperation("unique")>]
  member __.Unique (state, selectKey, message) = Spec.unique selectKey message state
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  [<CustomOperation("structureOf")>]
  member __.StructureOf (state, selector, plan, message) = Spec.structureOf selector plan message state
  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  [<CustomOperation("structure")>]
  member __.Structure (state, plan, message) = Spec.structure plan message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  [<CustomOperation("lengthOf")>]
  member __.LengthOf (state, selector, length, message) = Spec.lengthOf selector length message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a length of the specified length.
  [<CustomOperation("stringLengthOf")>]
  member __.StringLengthOf (state, selector, length, message) = Spec.stringLengthOf selector length message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  [<CustomOperation("lengthMinOf")>]
  member __.LengthMinOf (state, selector, min, message) = Spec.lengthMinOf selector min message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a minimum length.
  [<CustomOperation("stringLengthMinOf")>]
  member __.StringLengthMinOf (state, selector, min, message) = Spec.stringLengthMinOf selector min message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  [<CustomOperation("lengthMaxOf")>]
  member __.LengthMaxOf (state, selector, max, message) = Spec.lengthMaxOf selector max message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a maximum length.
  [<CustomOperation("stringLengthMaxOf")>]
  member __.StringLengthMaxOf (state, selector, max, message) = Spec.stringLengthMaxOf selector max message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  [<CustomOperation("lengthBetweenOf")>]
  member __.LengthBetweenOf (state, selector, min, max, message) = Spec.lengthBetweenOf selector min max message state
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the string should have a length within the specified range (min, max).
  [<CustomOperation("stringLengthBetweenOf")>]
  member __.StringLengthBetweenOf (state, selector, min, max, message) = Spec.stringLengthBetweenOf selector min max message state
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  [<CustomOperation("length")>]
  member __.Length (state, length, message) = Spec.length length message state
  /// Adds a requirement for the string to check if the length matches the specified length.
  [<CustomOperation("stringLength")>]
  member __.StringLength (state, length, message) = Spec.stringLength length message state
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  [<CustomOperation("lengthMin")>]
  member __.LengthMin (state, min, message) = Spec.lengthMin min message state
  /// Adds a requirement for the string to check if the sequence has a minimum length.
  [<CustomOperation("stringLengthMin")>]
  member __.StringLengthMin (state, min, message) = Spec.stringLengthMin min message state
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  [<CustomOperation("lengthMax")>]
  member __.LengthMax (state, max, message) = Spec.lengthMax max message state
  /// Adds a requirement for the string to check if the sequence has a maximum length.
  [<CustomOperation("stringLengthMax")>]
  member __.StringLengthMax (state, max, message) = Spec.stringLengthMax max message state
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  [<CustomOperation("lengthBetween")>]
  member __.LengthBetween (state, min, max, message) = Spec.lengthBetween min max message state
  /// Adds a requirement for the string to check if the sequence has a length within the specified range (min, max).
  [<CustomOperation("stringLengthBetween")>]
  member __.StringLengthBetween (state, min, max, message) = Spec.stringLengthBetween min max message state
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
  /// which defines that the result should be inclusive between (min &lt;= value && value &lt;= max) the specified range.
  [<CustomOperation("inclusiveBetweenOf")>]
  member __.InclusiveBetweenOf (state, selector, min, max, message) = Spec.inclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is inclusive between (min &lt;= value && value &lt;= max) the specified range.
  [<CustomOperation("inclusiveBetween")>]
  member __.InclusiveBetween (state, min, max, message) = Spec.inclusiveBetween min max message state
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (min &lt; value && value &lt; max) the specified range.
  [<CustomOperation("exclusiveBetweenOf")>]
  member __.ExclusiveBetweenOf (state, selector, min, max, message) = Spec.exclusiveBetweenOf selector min max message state
  /// Adds a requirement to check if the value is exclusive between (min &lt; value && value &lt; max) the specified range.
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
  member __.Run (specification) = validate specification

/// Exposure values/functions for the `Spec<_>` type.
[<AutoOpen>]
module SpecExposure =
  /// Computation builder to create an `Spec<_>` instance.
  let spec<'a> = new SpecBuilder<'a, Spec<'a>> (id)
  /// Computation builder to create a specification with a embedded logger.
  let specLog<'a> logger = new SpecBuilder<'a, Spec<'a>> (id, Spec.defl<'a> logger)
  /// Computation builder to create a specification with a embedded model tag.
  let specTag<'a> tag = new SpecBuilder<'a, Spec<'a>> (id, Spec.tag tag)
  /// Computation builder to create a specification with a embedded model tag and logger.
  let specWith<'a> tag logger = new SpecBuilder<'a, Spec<'a>> (id, Spec.create tag logger)
  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Result<_, string list>` when runned.
  let specResult<'a> value = new SpecBuilder<'a, Result<'a, ErrorsByTag>> (Spec.validate value)
  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Result<_ option, string list>` when runned and the filter succeeds.
  let specResultIf<'a> filter value = new SpecBuilder<'a, Result<'a option, ErrorsByTag>> (Spec.validateIf filter value)
  /// Computation builder to build an `Spec<_>` instance that gets validated to a `Option<_>` when runned.
  let specOption<'a> value = new SpecBuilder<'a, 'a option> (Spec.validateOption value)
  /// Computation builder to build an `Spec,_>` instance that gets validated from an `Untrust<_>` to a `Result<_, string list>` when runned.
  let specUntrust<'a> untrusted = new SpecBuilder<'a, Result<'a, ErrorsByTag>> (Spec.validateUntrust untrusted)
  /// Computation builder to build an `Spec<_>` instance that gets validated a custom domain model when runned.
  let specModel<'a, 'b> createModel value = new SpecBuilder<'a, Result<'b, ErrorsByTag>> (Spec.createModel createModel value)
  /// Computation builder to build an `Spec<_>` instance that gets validated a custom domain model when runned.
  let specModelWith<'a, 'b> createModel value = new SpecBuilder<'a, Result<'b, ErrorsByTag>> (Spec.createModelWith createModel value)
  /// Makes a new specification that's describes an invariant relation between two given specifications.
  let specInvariant spec1 spec2 = new SpecBuilder<_, Spec<_>> (id, Spec.invariant spec1 spec2)
  /// Makes an optional creation of a domain model using an optional specification that only happens when the given filter succeeds.
  let specModelIf<'a, 'b> filter createModel value = new SpecBuilder<'a, Result<'b option, ErrorsByTag>> (Spec.createModelIf filter createModel value)

/// Extra operations on the `Result<_, _>` type with `Map<_, _>` as error type.
module Result =
  /// Lifts a two argument function to work with result types.
  let lift2Map f xResult yResult =
    Result.lift2With Map.append f xResult yResult
  /// Applies a function `f` in `Ok f` to a `x` in `Ok x` when both are `Ok` values.
  let applyMap fResult xResult =
    Result.applyWith Map.append fResult xResult
  /// Traverse over a sequence running a given mapping function over the elements, 
  /// collecting the outcomes into a result.
  let traverseSeqMap f m =
    Result.traverseSeqWith Map.append f m
  /// Transforms a sequence of results into a result of a sequence.
  let sequenceSeqMap m = traverseSeqMap id m

module internal Map = let others xs = Map.create "other" xs

/// Thrown when the value of a validation result is called on an invalid validation result.
exception InvalidValidationResultException of string

#nowarn "1001"

/// <summary>
/// Represents the result type when a value is validated against a domain specification <see cref="Spec{T}" />.
/// </summary>
[<Struct; NoEquality; NoComparison>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
[<DebuggerDisplay("{IsValid ? \"Success: \" + Value : \"Failure: \" + System.String.Join(\", \", Errors)}")>]
[<ExcludeFromCodeCoverage>]
type ValidationResult<'T> internal (result : Result<'T, ErrorsByTag>) =
  /// <summary>
  /// Initializes a new instance of the <see cref="ValidationResult"/> class with a successful validation result.
  /// </summary>
  /// <param name="value">The successful validation result value.</param>
  new (value : 'T) = ValidationResult<'T> (Ok value)
  /// <summary>
  /// Initializes a new instance of the <see cref="ValidationResult"/> class with a faulted validation errors.
  /// </summary>
  /// <param name="errors">The error descriptions of the faulted validation result.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="errors"/> doesn't contain any validation errors.</exception>
  new (errors : string seq) = 
    if Seq.isEmpty errors then invalidArg "errors" "Requires at least a single validation error"
    ValidationResult<'T> (Error (Map.others (List.ofSeq errors)))
  /// <summary>
  /// Initializes a new instance of the <see cref="ValidationResult"/> class with several faulted validation errors.
  /// </summary>
  /// <param name="errors">The error descriptions of the faulted validation result.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="errors"/> doesn't contain any validation errors.</exception>
  internal new ([<ParamArray>] errors : string [] []) = 
    if Seq.isEmpty errors then invalidArg "errors" "Requires at least a single validation error"
    ValidationResult<'T> (Error (Map.others (Array.concat errors |> List.ofArray)))
  /// <summary>
  /// Initializes a new instance of the <see cref="ValidationResult"/> class with detailed faulted validation errors.
  /// </summary>
  /// <param name="details">The error details of the validation result.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="details"/> doesn't contain any validation errors.</exception>
  internal new ([<ParamArray>] details : IReadOnlyDictionary<string, string array> []) = 
    if Seq.isEmpty details then invalidArg "details" "Requires at least a single validation error"
    let errors =
      details |> Seq.collect (fun dic -> Seq.map (fun (KeyValue (k, vs)) -> k, Seq.ofArray vs) dic)
              |> Map.ofSeqg
              |> Map.mapv List.ofSeq
    if Seq.isEmpty errors then invalidArg "details" "Requires at least a single validation error"
    if Seq.isEmpty (Seq.concat (Map.values errors)) then invalidArg "details" "Requires at least a single validation error"
    ValidationResult<'T> (Error errors)
  /// Gets the F# result from this validation result.
  member internal __.Result = result
  /// Gets the value that was validated (possible `null` when the validation failed).
  member __.Value : 'T = Result.getOrValue Unchecked.defaultof<'T> result
  /// Gets a value indicating whether the validation succeeded.
  member __.IsValid = Result.isOk result
  /// Gets the series of validation errors that describe to what domain requirements the validated value doesn't satisfy.
  member __.Errors : string array = Result.either (fun _ -> Array.empty) (Map.values >> List.concat >> Array.ofList) result
  /// Gets the series of validation error details that describe what domain requirements for each validated property value doesn't satisfy.
  member __.Details : IReadOnlyDictionary<string, string array> = Result.either (fun _ -> readOnlyDict []) (Map.mapv Array.ofList >> Map.toReadOnlyDict) result

  /// Creates a successful validation result with a specified value.
  static member Success (value : 'T) = ValidationResult<'T> (value = value)
  /// <summary>
  /// Creats an unsuccessful validation result with the specified errors.
  /// </summary>
  /// <param name="errors">The error descriptions of the faulted validation result.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="errors"/> doesn't contain any validation errors.</exception>
  static member Failure ([<ParamArray>] errors : string []) = ValidationResult<'T> (errors = errors)

  static member op_Implicit (result : Result<'T, string list>) = 
    ValidationResult<'T> (Result.mapError Map.others result)

  static member op_Implicit (result : Result<'T, ErrorsByTag>) =
    ValidationResult<'T> (result)
  
  static member op_Implicit (result : ValidationResult<'T>) = 
    result.Result
    |> Result.mapError (Map.mapv Array.ofList >> Map.toDict)
    |> Outcome.OfFSharpResult
  
  static member op_Implicit (outcome : Outcome<'T, string array>) = 
    let result = Outcome.ToFSharpResult outcome |> Result.mapError (List.ofArray >> Map.others)
    ValidationResult<'T> (result=result)

  static member op_Implicit (outcome : Outcome<'T, IDictionary<string, string array>>) = 
      let result = Outcome.ToFSharpResult outcome |> Result.mapError (Map.ofDict >> Map.mapv List.ofArray)
      ValidationResult<'T> (result=result)

  /// Tries to get the value that was validated.
  member __.TryGetValue (output : outref<'T>) =
    match result with
    | Ok x -> output <- x; true
    | _ -> output <- Unchecked.defaultof<'T>; false
  /// Gets the validated value or the absence of the value.
  member this.ToMaybe ([<Optional>] handleErrors : Action<_>) =
    let handleErrors = if isNull handleErrors then fun _ -> () else handleErrors.Invoke
    match result with
    | Ok x -> Maybe.Just x
    | Error _ -> handleErrors this.Errors; Maybe<'T>.Nothing
  /// Transforms the validation result to an abstracted outcome result.
  member this.ToOutcome () = ValidationResult.op_Implicit this
  /// Returns a string that represents either the string representation of the successful value, or the concatenation of validation errors.
  override this.ToString () = 
    if this.IsValid 
    then sprintf "Success: %A" this.Value 
    else let toPropOverview prop errors = String.Join (Environment.NewLine, [| yield sprintf "%s: " prop; yield! Array.map (sprintf " -> %s") errors |])
         String.Join (Environment.NewLine, Seq.map (fun (KeyValue (prop, errors)) -> toPropOverview prop errors) this.Details)

/// Result type when a value is validated against a domain specification `Spec<_>`.
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
[<ExcludeFromCodeCoverage>]
type ValidationResult private () =
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TResult> 
    ( (validation1 : ValidationResult<'TFirst>), 
      (validation2 : ValidationResult<'TSecond>), 
      (resultSelector : Func<_, _, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value))
    else ValidationResult<'TResult> (validation1.Details, validation2.Details)
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TThird, 'TResult>
    ( (validation1 : ValidationResult<'TFirst>),
      (validation2 : ValidationResult<'TSecond>),
      (validation3 : ValidationResult<'TThird>),
      (resultSelector : Func<'TFirst, 'TSecond, 'TThird, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid && validation3.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value, validation3.Value))
    else ValidationResult<'TResult> (validation1.Details, validation2.Details, validation3.Details)
  /// Combines validation results into a new validation result.
  static member Combine<'TFirst, 'TSecond, 'TThird, 'TFourth, 'TResult>
    ( (validation1 : ValidationResult<'TFirst>),
      (validation2 : ValidationResult<'TSecond>),
      (validation3 : ValidationResult<'TThird>),
      (validation4 : ValidationResult<'TFourth>),
      (resultSelector : Func<'TFirst, 'TSecond, 'TThird, 'TFourth, 'TResult>) ) =
    if validation1.IsValid && validation2.IsValid && validation3.IsValid && validation4.IsValid
    then ValidationResult<'TResult> (value=resultSelector.Invoke (validation1.Value, validation2.Value, validation3.Value, validation4.Value))
    else ValidationResult<'TResult> (validation1.Details, validation2.Details, validation3.Details, validation4.Details)
  /// Creates an validation error for a given tag and a message.
  static member Error<'T> (tag, message) =
    ValidationResult<'T> (result = Spec.error tag message)

/// Representation of a domain specification that contains the validation for the model.
[<ExcludeFromCodeCoverage>]
type Spec =
  /// Start defininig a specification for a type.
  static member Of<'T>() = Spec.def<'T>
  /// Start defininig a specification for a type.
  static member Of<'T>(name) : Spec<'T> = Spec.tag name
  /// Start defininig a specification for a type, adding a logger while doing so.
  static member Of<'T>(name, logger) : Spec<'T> = Spec.create name logger
  /// Starts defining a specification for a type, adding a logger while doing so.
  static member Of<'T>(logger) : Spec<'T> = Spec.defl<'T> logger
  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  static member Merge (spec1 : Spec<'T>, spec2) = Spec.merge spec1 spec2
  /// Makes a new specification that's describes an invariant relation between two given specifications.
  static member Invariant (spec1 : Spec<'T1>, spec2 : Spec<'T2>) = Spec.invariant spec1 spec2 |> Spec.comap (fun (t : ValueTuple<_, _>) -> t.ToTuple())
  /// Makes a new specification that's describes an invariant relation between three given specifications.
  static member Invariant (spec1 : Spec<'T1>, spec2 : Spec<'T2>, spec3 : Spec<'T3>) = Spec.invariant3 spec1 spec2 spec3 |> Spec.comap (fun (t : ValueTuple<_, _, _>) -> t.ToTuple())
  /// Makes an optional creation of a domain model using an optional specification that only happens when the given filter succeeds.
  static member Optional (filter : Func<'T, Maybe<'TValue>>, creator : Func<'TValue, ValidationResult<'TResult>>, value) =
    if isNull filter then nullArg "filter"
    if isNull creator then nullArg "creator"
    Spec.optional (filter.Invoke >> Maybe.ToOption) (creator.Invoke >> fun x -> x.Result) value 
    |> Result.map Maybe.OfOption
    |> ValidationResult<Maybe<'TResult>>
  /// Makes an optional creation of a domain model using an optional specification that only happens when the given filter succeeds.
  static member Optional (filter : Func<'T, bool>, creator : Func<'T, ValidationResult<'TResult>>, value) =
    if isNull filter then nullArg "filter"
    if isNull creator then nullArg "creator"
    Spec.optional (fun x -> if filter.Invoke x then Some x else None) (creator.Invoke >> fun x -> x.Result) value
    |> Result.map Maybe.OfOption
    |> ValidationResult<Maybe<'TResult>>

/// Delegate of: 'T -> bool * string.
type Requirement<'T> = delegate of 'T -> bool * string

/// Extensions on the `ValidationResult<_>` type to use in C# context.
[<Extension>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
[<ExcludeFromCodeCoverage>]
type ValidationResultExtensions =
  /// <summary>
  /// Projects the validated result to another value.
  /// </summary>
  /// <param name="result">The validation result to transform.</param>
  /// <param name="selector">The transformation function to select another value from the validation <paramref name="result"/>.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Select (result : ValidationResult<'T>, selector : Func<'T, 'TResult>) =
    if isNull selector then nullArg "selector"
    if result.IsValid then ValidationResult<'TResult> (selector.Invoke result.Value)
    else ValidationResult<'TResult> result.Errors
  /// <summary>
  /// Projects the validated result to another validation result.
  /// </summary>
  /// <param name="result">The validation result to transform.</param>
  /// <param name="selector">The transformation function to select another value from the validation <paramref name="result"/>.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member SelectMany (result : ValidationResult<'T>, selector : Func<'T, ValidationResult<'TResult>>) =
    if isNull selector then nullArg "selector"
    if result.IsValid then selector.Invoke (result.Value)
    else ValidationResult<'TResult> result.Errors
  /// <summary>
  /// Projects the validated reuslt to another validation result.
  /// </summary>
  /// <param name="result">The validation result to transform.</param>
 /// <param name="selector">The transformation function to select another value from the validation <paramref name="result"/>.</param>
 /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Then (result : ValidationResult<'T>, selector) = 
    if isNull selector then nullArg "selector"
    result.SelectMany (selector)
  /// <summary>
  /// Filters out the validated value with yet another predicate and a series of validation error messages to create a fresh validation result in case the predicate doesn't hold.
  /// </summary>
  /// <param name="this">The validation result to inspect.</param>
  /// <param name="predicate">The filtering function that determines whether or not the validation result is considered valid.</param>
  /// <param name="errors">The validation error descriptions that will result in a fresh validation result when the <paramref name="predicate"/> doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="predicate"/> or <paramref name="errors"/> is <c>null</c>.</exception>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="errors"/> doesn't contain any validation errors.</exception>
  [<Extension>]
  static member Where (this : ValidationResult<'T>, predicate : Func<_, _>, [<ParamArray>] errors) =
    if isNull predicate then nullArg "predicate"
    if isNull errors then nullArg "errors"
    if Seq.isEmpty errors then invalidArg "errors" "Requires at least a single validation error"
    if Array.isEmpty errors then invalidArg "errors" "at least a single validation error message should be given"
    ValidationResult<'T> (Result.filter predicate.Invoke (Map.ofList [ "[without tag]", List.ofArray errors ]) this.Result)
  /// Filters out the validated value with yet another specification.
  [<Extension>]
  static member Where (this : ValidationResult<'T>, otherSpecification : Spec<'T>) =
    ValidationResult<'T> (Result.bind (fun x -> Spec.validate x otherSpecification) this.Result)
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
    let result = Result.traverseSeqMap (selector.Invoke >> fun x -> x.Result) source
    ValidationResult<IEnumerable<'TResult>> (result = result)
  /// Transforms a sequence of validation results into a validation results of a sequence.
  [<Extension>]
  static member Sequence (source : IEnumerable<ValidationResult<'T>>) =
    if isNull source then nullArg "source"
    let result = Result.sequenceSeqMap (source |> Seq.map (fun x -> x.Result))
    ValidationResult<IEnumerable<'T>> (result = result)

/// Exception thrown when the validation of a value against a domain specification failed.
exception ValidationFailureException of string

/// Extensions on the `Spec<_>` type to use in C# context.
[<Extension>]
[<CompilerMessage("Not designed for F#", 1001, IsHidden = true)>]
[<ExcludeFromCodeCoverage>]
type SpecExtensions =
  /// <summary>
  /// Adds a custom requirement to the specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="requirement">The requirement to add to the <paramref name="specification"/>.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="requirement"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Add (specification : Spec<'T>, requirement : Func<'T, ValueTuple<bool, string>>) =
    if isNull requirement then nullArg "requirement"
    Spec.add (requirement.Invoke >> fun t -> 
      let t = t.ToTuple() in if isNull (snd t) then invalidArg "message" "" else t) specification

  /// <summary>
  /// Adds a custom requirement to the specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="requirement">The requirement to add to the <paramref name="specification"/>.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="requirement"/> or <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Add (specification : Spec<'T>, requirement : Func<'T, bool>, message) =
    if isNull requirement then nullArg "requirement"
    if isNull message then nullArg "message"
    Spec.add (fun x -> requirement.Invoke (x), message) specification

  /// <summary>
  /// Adds a conditional requirement to the specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The predicate that determines whether or not the <paramref name="requirement"/> should be included in the <paramref name="specification"/>.</param>
  /// <param name="requirement">The requirement that will be added to the <paramref name="specification"/> when the <paramref name="predicate"/> holds.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="predicate"/> or <paramref name="requirement"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Conditional (specification : Spec<'T>, predicate : Func<'T, bool>, requirement : Func<'T, ValueTuple<bool, string>>) =
    if isNull predicate then nullArg "predicate"
    if isNull requirement then nullArg "requirement"
    Spec.conditional predicate.Invoke (fun x -> requirement.Invoke(x).ToTuple()) specification

  /// <summary>
  /// Adds a conditional requirement to the specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The predicate that determines whether or not the <paramref name="dependent"/> should be included in the <paramref name="specification"/>.</param>
  /// <param name="dependent">The dependent specification that will be added to the <paramref name="specification"/> when the <paramref name="predicate"/> holds.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="predicate"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Where (specification : Spec<'T>, predicate : Func<'T, bool>, dependent : Spec<'T>) =
    if isNull predicate then nullArg "predicate"
    Spec.filter predicate.Invoke dependent specification

  /// <summary>
  /// Adds a conditional requirement to the specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The selection function to determine the dependent value.</param>
  /// <param name="predicate">The predicate that determines whether or not the <paramref name="dependent"/> should be included in the <paramref name="specification"/>.</param>
  /// <param name="dependent">The dependent specification that will be added to the <paramref name="specification"/> when the <paramref name="predicate"/> holds.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/>, <paramref name="predicate"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Where (specification : Spec<'T>, selector : Func<'T, 'TResult>, predicate : Func<'TResult, bool>, dependent : Spec<'TResult>) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    Spec.filterOf selector.Invoke predicate.Invoke dependent specification

  /// <summary>
  /// Adds a conditional requirement to the specification that is the result of the specified selection and is not `default`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The selection function to determine the dependent value.</param>
  /// <param name="dependent">The dependent specification that will be added to the <paramref name="specification"/> when the <paramref name="selector"/> holds.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Where (specification : Spec<'T>, selector : Func<'T, 'TResult>, dependent : Spec<'TResult>) =
    if isNull selector then nullArg "selector"
    Spec.filterT selector.Invoke dependent specification

  /// <summary>
  /// Adds another specification on which the current specification depends on.
  /// This dependent specification will run before the current specification and contain only the errors of the dependents in case of a validation failure.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="dependent">The specification that should run before the current <paramref name="specification"/>.</param>
  [<Extension>]
  static member DependsOn (specification : Spec<'T>, dependent : Spec<'T>) =
    Spec.dependsOn dependent specification

  /// <summary>
  /// Combines two specifications as dependents of a new specification which will run before any extra requirements that's been added after this point.
  /// </summary>
  /// <param name="specification1">The first of two specifications that should be merged as an dependent specifications of a new one.</param>
  /// <param name="specification2">The second of two specifications that should be merged as an dependent specifications of a new one.</param>
  [<Extension>]
  static member Merge (specification1 : Spec<'T>, specification2 : Spec<'T>) =
    Spec.merge specification1 specification2

  /// <summary>
  /// Adds a dependent specification that will validate a subset of a resulting value of the original specifiation.
  /// </summary>
  /// <param name="specification">The specification that specifies how the model should look like.</param>
  /// <param name="selector">The function to select a sub-value of the value which this <paramref name="specification"/> validates.</param>
  /// <param name="dependent">The sub-specification that will validate the result of the <paramref name="selector"/>.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Subset (specification : Spec<'T>, selector : Func<'T, 'TResult>, dependent : Spec<'TResult>) =
    if isNull selector then nullArg "selector"
    Spec.subset selector.Invoke dependent specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be equal to the specified value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="value">The value that should be equal to the value that's being validated in the <paramref name="specification"/>.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Equal<'T, 'TResult when 'TResult : equality> (specification : Spec<'T>, selector : Func<'T, 'TResult>, value, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.equalOf selector.Invoke value message specification
  
  /// <summary>
  /// Adds a requirement to check equality to a specified value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="value">The value that should be equal to the value that's being validated in the <paramref name="specification"/>.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Equal<'T when 'T : equality> (specification : Spec<'T>, value, message) =
    if isNull message then nullArg "message"
    Spec.equal value message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be equal to the specified value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="value">The other value to compare with the current value in this specification.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member NotEqual<'T, 'TResult when 'TResult : equality> (specification : Spec<'T>, selector : Func<'T, 'TResult>, value, message) =  
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notEqualOf selector.Invoke value message specification

  /// <summary>
  /// Adds a requirement to check no equality to a specified value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="value">The other value to compare with the current value in this specification.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotEqual<'T when 'T : equality> (specification : Spec<'T>, value, message) = 
    if isNull message then nullArg "message"
    Spec.notEqual value message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotNull (specification : Spec<'T>, selector : Func<'T, 'TResult>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notNullOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for not `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotNull<'T when 'T : null and 'T : equality> (specification : Spec<'T>, message) =
    if isNull message then nullArg "message"
    Spec.notNull message specification

  /// <summary>
  /// Adds a requirement to check for non-empty string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotEmpty (specification : Spec<string>, message) =
    if isNull message then nullArg "message"
    Spec.notEmpty message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member NotEmpty (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notEmptyOf selector.Invoke message specification
  
  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should not be a not-whitespace string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotWhiteSpace (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notWhiteSpaceOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check if the string is a not-whitespace string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotWhiteSpace (specification : Spec<string>, message) =
    if isNull message then nullArg "message"
    Spec.notWhiteSpace message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotNullOrEmpty (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notNullOrEmptyOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check if the string is a not-null, not-empty string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotNullOrEmpty (specification : Spec<string>, message) =
    if isNull message then nullArg "message"
    Spec.notNullOrEmpty message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a not-null, not-empty, not-whitespace string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member NotNullOrWhiteSpace (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notNullOrWhiteSpaceOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check if the string is a not-null, not-empty, not-whitespace string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotNullOrWhiteSpace (specification, message) =
    if isNull message then nullArg "message"
    Spec.notNullOrWhiteSpace message specification

  /// <summary>
  /// Adds a requirement to check if the string starts with the specified header.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="header">The prefix string value that should be the beginning of the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member StartsWith (specification, header, message) =
    if isNull header then nullArg "header"
    if isNull message then nullArg "message"
    Spec.startsWith header message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string that starts with a specified header.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="header">The prefix string value that should be the beginning of the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member StartsWith (specification : Spec<'T>, selector : Func<'T, string>, header, message) =
    if isNull selector then nullArg "selector"
    if isNull header then nullArg "header"
    if isNull message then nullArg "message"
    Spec.startsWithOf selector.Invoke header message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string ending with the specified trailer.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="trailer">The postfix string value that should be the beginning of the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member EndsWith (specification : Spec<'T>, selector : Func<'T, string>, trailer, message) =
    if isNull selector then nullArg "selector"
    if isNull trailer then nullArg "trailer"
    if isNull message then nullArg "message"
    Spec.endsWithOf selector.Invoke trailer message specification

  /// <summary>
  /// Adds a requirement to check if the string ends with the specified trailer.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="trailer">The postfix string value that should be the beginning of the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member EndsWith (specification : Spec<string>, trailer, message) =
    if isNull message then nullArg "message"
    Spec.endsWith trailer message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a sequence equal (item-wise) to another sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="items">The specific items the value verified in this specification should contain.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member SequenceEqual (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, items, message) =
    if isNull selector then nullArg "selector"
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.seqEqualOf selector.Invoke items message specification
  
  /// <summary>
  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The specific items the value verified in this specification should contain.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member SequenceEqual (specification : Spec<IEnumerable<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.seqEqual items message specification
  
  /// <summary>
  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The specific items the value verified in this specification should contain.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member SequenceEqual (specification : Spec<IList<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.seqEqual items message specification

  /// <summary>
  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The specific items the value verified in this specification should contain.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member SequenceEqual (specification : Spec<ICollection<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.seqEqual items message specification

  /// <summary>
  /// Adds a requirement to check if the sequence is equal (item-wise) to another sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The specific items the value verified in this specification should contain.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member SequenceEqual (specification : Spec<'T array>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.seqEqual items message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a non-empty sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member NonEmpty (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.nonEmptyOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for non-empty sequences.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NonEmpty (specification : Spec<IEnumerable<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.nonEmpty message specification

  /// <summary>
  /// Adds a requirement to check for non-empty sequences.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NonEmpty (specification : Spec<ICollection<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.nonEmpty message specification

  /// <summary>
  /// Adds a requirement to check for non-empty sequences.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NonEmpty (specification : Spec<IList<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.nonEmpty message specification

  /// <summary>
  /// Adds a requirement to check for non-empty sequences.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NonEmpty (specification : Spec<'T[]>, message) =
    if isNull message then nullArg "message"
    Spec.nonEmpty message specification

  /// <summary>
  /// Adds a requirement to check for non-empty sequences.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NonEmpty (specification : Spec<IDictionary<'TKey, 'TValue>>, message) =
    if isNull message then nullArg "message"
    Spec.nonEmpty message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that all elements of the sequence should satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member All (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, predicate : Func<'TResult, bool>, message) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    Spec.forallOf selector.Invoke predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member All (specification : Spec<IEnumerable<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.forall predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member All (specification : Spec<ICollection<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.forall predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member All (specification : Spec<IList<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.forall predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member All (specification : Spec<'T[]>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.forall predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that all the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which each item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  static member All (specification : Spec<IDictionary<'TKey, 'TValue>>, predicate : Func<KeyValuePair<'TKey, 'TValue>, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.forall predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none of the elements of the sequence should satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="predicate">The function to which none item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member None (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, predicate : Func<'TResult, bool>, message) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.fornoneOf selector.Invoke predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that non of the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which none item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member None (specification : Spec<IEnumerable<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.fornone predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that non of the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which none item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member None (specification : Spec<IList<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.fornone predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that non of the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which none item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member None (specification : Spec<ICollection<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.fornone predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that non of the elements of the sequence satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function to which none item in the sequence has to match.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member None (specification : Spec<'T[]>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.fornone predicate.Invoke message specification

  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none of the elements of the sequence should be equal to `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member NoneNull (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.fornoneNullOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NoneNull (specification : Spec<IEnumerable<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.fornoneNull message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NoneNull (specification : Spec<IList<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.fornoneNull message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NoneNull (specification : Spec<ICollection<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.fornoneNull message specification

  /// <summary>
  /// Adds a requirement for the sequence to check that none of the elements of the sequence are equal to `null`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NoneNull (specification : Spec<'T[]>, message) =
    if isNull message then nullArg "message"
    Spec.fornoneNull message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should be present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="item">The element that should be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, item, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.containsOf selector.Invoke item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<IEnumerable<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.contains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<IList<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.contains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<ICollection<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.contains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<'T[]>, item, message) =
    if isNull message then nullArg "message"
    Spec.contains item message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given item should not be present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="item">The element that should not be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, item, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.notContainsOf selector.Invoke item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should not be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<IEnumerable<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.notContains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should not be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<IList<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.notContains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should not be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<ICollection<'T>>, item, message) =
    if isNull message then nullArg "message"
    Spec.notContains item message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given item is not present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="item">The element that should not be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<'T[]>, item, message) =
    if isNull message then nullArg "message"
    Spec.notContains item message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that a given set of items should all be present in th sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="items">The elements that should all be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, items, message) =
    if isNull selector then nullArg "selector"
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.containsAllOf selector.Invoke items message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The elements that should all be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<IEnumerable<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.containsAll items message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The elements that should all be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<ICollection<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.containsAll items message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The elements that should all be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<IList<'T>>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.containsAll items message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if a given set of items are all present in the sequence.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="items">The elements that should all be in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<'T array>, items, message) =
    if isNull items then nullArg "items"
    if isNull message then nullArg "message"
    Spec.containsAll items message specification

  /// <summary>
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is present in the string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="subString">The section that should be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<'T>, selector : Func<'T, string>, subString, message) =
    if isNull selector then nullArg "selector"
    if isNull subString then nullArg "subString"
    if isNull message then nullArg "message"
    Spec.stringContainsOf selector.Invoke subString message specification

  /// <summary>
  /// Adds a requirement for the string to verify if the given substring is present in the string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="subString">The section that should be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Contains (specification : Spec<string>, subString, message) =
    if isNull subString then nullArg "subString"
    if isNull message then nullArg "message"
    Spec.stringContains subString message specification

  /// <summary>
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substring is not present in the string
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="subString">The section that should not be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<'T>, selector : Func<'T, string>, subString, message) =
    if isNull selector then nullArg "selector"
    if isNull subString then nullArg "subString"
    if isNull message then nullArg "message"
    Spec.stringNotContainsOf selector.Invoke subString message specification

  /// <summary>
  /// Adds a requirement for the string to verify if a given substring is not present in the string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="subString">The section that should not be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotContains (specification : Spec<string>, subString, message) =
    if isNull subString then nullArg "subString"
    if isNull message then nullArg "message"
    Spec.stringNotContains subString message specification

  /// <summary>
  /// Adds a requirement for the resulting string of the specified mapping,
  /// which defines that the given substrings are all present in the string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="subStrings">The section that should be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<'T>, selector : Func<'T, string>, subStrings, message) =
    if isNull selector then nullArg "selector"
    if isNull subStrings then nullArg "subStrings"
    if isNull message then nullArg "message"
    Spec.stringContainsAllOf selector.Invoke subStrings message specification

  /// <summary>
  /// Adds a requirement for the string to verify if the given substrings are all present in the string.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="subStrings">The section that should be in the string.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ContainsAll (specification : Spec<string>, subStrings, message) =
    if isNull subStrings then nullArg "subStrings"
    if isNull message then nullArg "message"
    Spec.stringContainsAll subStrings message specification

  /// <summary>
  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the enumeration value from another value.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member InEnum<'T, 'TEnum when 'TEnum : enum<int32>> (specification : Spec<'T>, selector : Func<'T, 'TEnum>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.inEnumOfT<'T, 'TEnum> selector.Invoke message specification

  /// <summary>
  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member InEnum<'TEnum when 'TEnum : enum<int32>> (specification : Spec<'TEnum>, message) =
    if isNull message then nullArg "message"
    Spec.inEnumT<'TEnum> message specification

  /// <summary>
  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the enumeration value from another value.</param>
  /// <param name="enumType">The type of the enumeration.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member InEnum (specification : Spec<'T>, selector : Func<'T, 'TEnum>, enumType : Type, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.inEnumOf selector.Invoke enumType message specification

  /// <summary>
  /// Adds a requirement for the resulting value of the specified mapping 
  /// that defines that the given value should be defined in the given enumeration.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="enumType">The type of the enumeration.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member InEnum (specification : Spec<'TEnum>, enumType : Type, message) =
    if isNull message then nullArg "message"
    Spec.inEnum enumType message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that any item in the sequence to satisy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="predicate">The function that any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Any (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, predicate : Func<'TResult, bool>, message) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.existsOf selector.Invoke predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Any (specification : Spec<IEnumerable<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.exists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Any (specification : Spec<IList<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.exists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="predicate">The function that any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Any (specification : Spec<ICollection<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.exists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if any element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Any (specification : Spec<'T[]>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.exists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that none item in the sequence to satisy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="predicate">The function that not any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotAny (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, predicate : Func<'TResult, bool>, message) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.notExistsOf selector.Invoke predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if none element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that not any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotAny (specification : Spec<IEnumerable<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.notExists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if none element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="predicate">The function that not any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotAny (specification : Spec<IList<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.notExists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if none element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that not any items should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotAny (specification : Spec<ICollection<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.notExists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if none element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that all items should satisfy.</param>
    /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member NotAny (specification : Spec<'T[]>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.notExists predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that all elements of the sequence should be unique.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="selectKey">The function to select the key that determines uniqueness.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Unique (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, selectKey : Func<'TResult, 'TKey>, message) =
    if isNull selector then nullArg "selector"
    if isNull selectKey then nullArg "selectKey"
    if isNull message then nullArg "message"
    Spec.uniqueOf selector.Invoke selectKey.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for sequences with only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selectKey">The function to select the key that determines uniqueness.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Unique (specification : Spec<IEnumerable<'T>>, selectKey : Func<'T, 'TKey>, message) =
    if isNull selectKey then nullArg "selectKey"
    if isNull message then nullArg "message"
    Spec.unique selectKey.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for sequences with only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selectKey">The function to select the key that determines uniqueness.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Unique (specification : Spec<IList<'T>>, selectKey : Func<'T, 'TKey>, message) =
    if isNull selectKey then nullArg "selectKey"
    if isNull message then nullArg "message"
    Spec.unique selectKey.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for sequences with only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selectKey">The function to select the key that determines uniqueness.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Unique (specification : Spec<ICollection<'T>>, selectKey : Func<'T, 'TKey>, message) =
    if isNull selectKey then nullArg "selectKey"
    if isNull message then nullArg "message"
    Spec.unique selectKey.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for sequences with only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selectKey">The function to select the key that determines uniqueness.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Unique (specification : Spec<'T[]>, selectKey : Func<'T, 'TKey>, message) =
    if isNull selectKey then nullArg "selectKey"
    if isNull message then nullArg "message"
    Spec.unique selectKey.Invoke message specification

  /// <summary>
  /// Adds a requirement to check if the sequence contains only unique elements.
  /// </summary>
  /// <param name="specification">The specification to add the requirement.</param>
  /// <param name="message">The error message to show when this requirement fails.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Unique (specification : Spec<IEnumerable<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.uniqueItems message specification

  /// <summary>
  /// Adds a requirement to check if the sequence contains only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement fails.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Unique (specification : Spec<IList<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.uniqueItems message specification

  /// <summary>
  /// Adds a requirement to check if the sequence contains only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement fails.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Unique (specification : Spec<ICollection<'T>>, message) =
    if isNull message then nullArg "message"
    Spec.uniqueItems message specification

  /// <summary>
  /// Adds a requirement to check if the sequence contains only unique elements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement fails.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Unique (specification : Spec<'T[]>, message) =
    if isNull message then nullArg "message"
    Spec.uniqueItems message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that only a single element in the sequence should satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="predicate">The function that only a single item should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Single (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, predicate : Func<'TResult, bool>, message) =
    if isNull selector then nullArg "selector"
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.singleOf selector.Invoke predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that only a single item should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Single (specification : Spec<IEnumerable<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.single predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that only a single item should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Single (specification : Spec<IList<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.single predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>/>
  /// <param name="predicate">The function that only a single item should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Single (specification : Spec<ICollection<'T>>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.single predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if only a single element satisfy the specified predicate.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="predicate">The function that only a single item should satisfy.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Single (specification : Spec<'T[]>, predicate : Func<'T, bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Spec.single predicate.Invoke message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, plan : IEnumerable<Func<'TResult, bool>>, message) =
    if isNull selector then nullArg "selector"
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structureOf selector.Invoke plan message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<'T>, selector : Func<'T, IList<'TResult>>, plan : IEnumerable<Func<'TResult, bool>>, message) =
    if isNull selector then nullArg "selector"
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structureOf selector.Invoke plan message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Structure (specification : Spec<'T>, selector : Func<'T, ICollection<'TResult>>, plan : IEnumerable<Func<'TResult, bool>>, message) =
    if isNull selector then nullArg "selector"
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structureOf selector.Invoke plan message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping,
  /// which defines that the sequence should match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">
  ///   Thrown when the <paramref name="selector"/> or the <paramref name="plan"/> or the <paramref name="message"/> is <c>null</c>.
  /// </exception>
  [<Extension>]
  static member Structure (specification : Spec<'T>, selector : Func<'T, 'TResult[]>, plan : IEnumerable<Func<'TResult, bool>>, message) =
    if isNull selector then nullArg "selector"
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structureOf selector.Invoke plan message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<IEnumerable<'T>>, plan : IEnumerable<Func<'T, bool>>, message) =
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structure plan message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<IList<'T>>, plan : IEnumerable<Func<'T, bool>>, message) =
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"

    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structure plan message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<ICollection<'T>>, plan : IEnumerable<Func<'T, bool>>, message) =
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structure plan message specification

  /// <summary>
  /// Adds a requirement for the sequence to verify if the sequence match the specified structure; in the form of predicates for each seperate element.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param> 
  /// <param name="plan">The expected predicates for each item in the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Structure (specification : Spec<'T[]>, plan : IEnumerable<Func<'T, bool>>, message) =
    if isNull plan then nullArg "plan"
    if isNull message then nullArg "message"
    if Seq.exists isNull plan 
    then invalidArg "plan" "Structure plan should not include 'null' elements"
    
    let plan = plan |> Seq.map (fun f -> f.Invoke)
    Spec.structure plan message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length of the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Length (specification : Spec<'T>, (selector : Func<'T, IEnumerable<'TResult>>), length, message) = 
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.lengthOf selector.Invoke length message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<IEnumerable<'T>>, length, message) = 
    if isNull message then nullArg "message"
    Spec.length length message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<IList<'T>>, length, message) = 
    if isNull message then nullArg "message"
    Spec.length length message specification
  
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<ICollection<'T>>, length, message) = 
    if isNull message then nullArg "message"
    Spec.length length message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<'T[]>, length, message) = 
    if isNull message then nullArg "message"
    Spec.length length message specification
  
  /// <summary>
  /// Adds a requirement for the sequence to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<IDictionary<'TKey, 'TValue>>, length, message) = 
    if isNull message then nullArg "message"
    Spec.length length message specification

  /// <summary>
  /// Adds a requirement for the string to check if the length matches the specified length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="length">The expected length of the sequence.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Length (specification : Spec<string>, length, message) = 
    if isNull message then nullArg "message"
    Spec.stringLength length message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member LengthMin (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, min, message) =
    if isNull selector then nullArg "selector"
    Spec.lengthMinOf selector.Invoke min message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<IEnumerable<'T>>, min, message) =
    if isNull message then nullArg "message"
    Spec.lengthMin min message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<ICollection<'T>>, min, message) =
    if isNull message then nullArg "message"
    Spec.lengthMin min message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<IList<'T>>, min, message) =
    if isNull message then nullArg "message"
    Spec.lengthMin min message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<'T[]>, min, message) =
    if isNull message then nullArg "message"
    Spec.lengthMin min message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<IDictionary<'TKey, 'TValue>>, min, message) =
    if isNull message then nullArg "message"
    Spec.lengthMin min message specification

  /// <summary>
  /// Adds a requirement for the string to check if the sequence has a minimum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
   /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMin (specification : Spec<string>, min, message) =
    if isNull message then nullArg "message"
    Spec.stringLengthMin min message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, max, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.lengthMaxOf selector.Invoke max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<IEnumerable<'T>>, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthMax max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<IList<'T>>, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthMax max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<ICollection<'T>>, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthMax max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<'T[]>, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthMax max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax (specification : Spec<IDictionary<'TKey, 'TValue>>, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthMax max message specification

  /// <summary>
  /// Adds a requirement for the string to check if the sequence has a maximum length.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthMax ((specification : Spec<_>), max, message) =
    if isNull message then nullArg "message"
    Spec.stringLengthMax max message specification

  /// <summary>
  /// Adds a requirement for the resulting sequence of the specified mapping, 
  /// which defines that the sequence should have a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<'T>, selector : Func<'T, IEnumerable<'TResult>>, min, max, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.lengthBetweenOf selector.Invoke min max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<IEnumerable<'T>>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthBetween min max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<ICollection<'T>>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthBetween min max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<IList<'T>>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthBetween min max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<'T[]>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthBetween min max message specification

  /// <summary>
  /// Adds a requirement for the sequence to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween (specification : Spec<IDictionary<'TKey, 'TValue>>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.lengthBetween min max message specification

  /// <summary>
  /// Adds a requirement for the string to check if the sequence has a length within the specified range (min, max).
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The minimum length the sequence should have.</param>
  /// <param name="max">The maximum length the sequence should have.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LengthBetween ((specification : Spec<_>), min, max, message) =
    if isNull message then nullArg "message"
    Spec.stringLengthBetween min max message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than (limit gt; value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member LessThan (specification : Spec<'T>, selector : Func<'T, 'TResult>, limit, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.lessThanOf selector.Invoke limit message specification

  /// <summary>
  /// Adds a requirement to check if the value is less than (limit &gt; value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LessThan (specification : Spec<'T>, limit, message) =
    if isNull message then nullArg "message"
    Spec.lessThan limit message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than (limit &lt; value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member GreaterThan (specification : Spec<'T>, selector : Func<'T, 'TResult>, limit, message) =
    if isNull message then nullArg "message"
    Spec.greaterThanOf selector.Invoke limit message specification
  
  /// <summary>
  /// Adds a requirement to check if the value is greater than (limit &lt; value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member GreaterThan (specification : Spec<'T>, limit, message) =
    if isNull message then nullArg "message"
    Spec.greaterThan limit message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be less than or equal to (limit &gt;= value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member LessThanOrEqual (specification : Spec<'T>, selector : Func<'T, 'TResult>, limit, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.lessThanOrEqualOf selector.Invoke limit message specification

  /// <summary>
  /// Adds a requirement to check if the value is less than or equal to (limit &gt;= value) to the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member LessThanOrEqual (specification : Spec<'T>, limit, message) =
    if isNull message then nullArg "message"
    Spec.lessThanOrEqual limit message specification

  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be greater than or equal to (limit &lt;= value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member GreaterThanOrEqual (specification : Spec<'T>, selector : Func<'T, 'TResult>, limit, message) =
    if isNull message then nullArg "message"
    Spec.greaterThanOrEqualOf selector.Invoke limit message specification

  /// <summary>
  /// Adds a requirement to check if the value is greater than or equal to (limit &lt;= value) the specified limit.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="limit">The upper boundary limit where the value should stay under.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member GreaterThanOrEqual (specification : Spec<'T>, limit, message) =
    if isNull message then nullArg "message"
    Spec.greaterThanOrEqual limit message specification
  
  /// <summary>
  /// Adds a requirement to check if the value is inclusive between (min &lt;= value &amp;&amp; value &lt;= max) the specified range.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The lower boundary limit where the value should stay on or above.</param>
  /// <param name="max">The uppder boundary limit where the value should stay on or below.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member InclusiveBetween (specification : Spec<'T>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.inclusiveBetween min max message specification
  
  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be inclusive between (min &lt;= value &amp;&amp; value &lt;= max) the specified range.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="min">The lower boundary limit where the value should stay on or above.</param>
  /// <param name="max">The uppder boundary limit where the value should stay on or below.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member InclusiveBetween (specification : Spec<'T>, selector : Func<'T, 'TResult>, min, max, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.inclusiveBetweenOf selector.Invoke min max message specification

  /// <summary>
  /// Adds a requirement to check if the value is exclusive between (min &lt; value &amp;&amp; value &lt; max) the specified range.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="min">The lower boundary limit where the value should stay above.</param>
  /// <param name="max">The uppder boundary limit where the value should stay below.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member ExclusiveBetween (specification : Spec<'T>, min, max, message) =
    if isNull message then nullArg "message"
    Spec.exclusiveBetween min max message specification
  
  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be exclusive between (min &lt; value &amp;&amp; value &lt; max) the specified range.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="min">The lower boundary limit where the value should stay above.</param>
  /// <param name="max">The uppder boundary limit where the value should stay below.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member ExclusiveBetween (specification : Spec<'T>, selector : Func<'T, 'TResult>, min, max, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.exclusiveBetweenOf selector.Invoke min max message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="pattern">The regular expression pattern that the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension; Obsolete("Use 'Matches' instead")>]
  static member Regex (specification : Spec<'T>, selector : Func<'T, string>, pattern, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.matchesOf selector.Invoke pattern message specification

  // <summary>
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="pattern">The regular expression pattern the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension; Obsolete("Use 'Matches' instead")>]
  static member Regex ((specification : Spec<string>), pattern, message) =
    if isNull message then nullArg "message"
    Spec.matches pattern message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="expr">The regular expression the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Regex (specification : Spec<'T>, selector : Func<'T, string>, expr : Regex, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.matchesOf selector.Invoke (expr.ToString()) message specification

  /// <summary>
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="expr">The regular expression the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Regex (specification : Spec<string>, expr : Regex, message) =
    if isNull message then nullArg "message"
    Spec.matches (expr.ToString()) message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should be a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="pattern">The regular expression pattern the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Matches (specification : Spec<'T>, selector : Func<'T, string>, pattern, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.matchesOf selector.Invoke pattern message specification

  /// <summary>
  /// Adds a requirement to check if the value is a match to the specified regular expression pattern.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="pattern">The regular expression pattern the value should match against.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Matches ((specification : Spec<string>), pattern, message) =
    if isNull message then nullArg "message"
    Spec.matches pattern message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be a string containing only charaters in the alphabet.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member Alphabetical (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphabeticalOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for strings containing only charaters in the alphabet.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Alphabetical (specification, message) =
    if isNull message then nullArg "message"
    Spec.alphabetical message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Alphanum (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphanumOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for an alphanumerical value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Alphanum (specification, message) =
    if isNull message then nullArg "message"
    Spec.alphanum message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping,
  /// which defines that the result should be an alphanumerical value or special charaters.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The selection function that will select another value from the untrusted value.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> or the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member AlphanumSpecial (specification : Spec<'T>, selector : Func<'T, string>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.alphanumSpecialOf selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check for alphanumerical values.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member AlphanumSpecial (specification, message) =
    if isNull message then nullArg "message"
    Spec.alphanumSpecial message specification
  
  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to select the another value from the validated type.</param>
  /// <param name="expectedType">The expected type of the untrusted value.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member IsType (specification : Spec<'T>, selector : Func<'T, 'TResult>, expectedType, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.isTypeOf selector.Invoke expectedType message specification

  /// <summary>
  /// Adds a requirement to check if the value is an instance of the specified type.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="expectedType">The expected type of the untrusted value.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member IsType (specification : Spec<'T>, expectedType, message) =
    if isNull message then nullArg "message"
    Spec.isType expectedType message specification

  /// <summary>
  /// Adds a requirement for the result of the specified mapping, 
  /// which defines that the result should an instance of the specified type `T`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The selection function that will select another value from the untrusted value which should have the <typeparamref name="TExpected"/> type.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  [<Extension>]
  static member IsType<'T, 'TResult, 'TExpected> (specification : Spec<'T>, selector : Func<'T, 'TResult>, message) =
    if isNull selector then nullArg "selector"
    if isNull message then nullArg "message"
    Spec.isTypeOfT<'T, 'TResult, 'TExpected> selector.Invoke message specification

  /// <summary>
  /// Adds a requirement to check if the value is an instance of the specified type `T`.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="message">The error message to show when this requirement doesn't hold.</param>
  [<Extension>]
  static member IsType<'T, 'TExpected> (specification : Spec<'T>, message) =
    if isNull message then nullArg "message"
    Spec.isTypeT<'T, 'TExpected> message specification

  /// <summary>
  /// Change the way the validation of requirements should happen.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="mode">The mode in which the requirements should be validated.</param>
  [<Extension>]
  static member Cascade (specification : Spec<'T>, mode) = 
    Spec.cascade mode specification

  /// <summary>
  /// Maps the requirements of this specification to antother requirement function structure.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="selector">The function to project each requirement in the <paramref name="specification"/> to another requirement.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="selector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member Select (specification : Spec<'T>, selector : Func<Requirement<'T>, Requirement<'TResult>>) =
    if isNull selector then nullArg "selector"
    Spec.map (fun r -> let f = selector.Invoke(Requirement r) in if isNull f then nullArg "selector return value" else f.Invoke) specification

  /// <summary>
  /// Maps the to-be-validated value before the requirements are verified, therefore controlling the input value of this specification and further added requirements.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="preSelector">The selection function that will select another value from the untrusted value which results in a different specification.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="preSelector"/> is <c>null</c>.</exception>
  [<Extension>]
  static member PreSelect (specification : Spec<'TAfter>, preSelector : Func<'TBefore, 'TAfter>) =
    if isNull preSelector then nullArg "preSelector"
    Spec.comap preSelector.Invoke specification

  /// <summary>
  /// Adds a pre-valildation function to all the requirements of this specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="preValidator">The validation function that will pre-validate the untrusted value before the specification validaton.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="preValidator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member PreValidate (specification : Spec<'TAfter>, preValidator : Func<'TBefore, ValidationResult<'TAfter>>) =
    if isNull preValidator then nullArg "preValidator"
    let f x = preValidator.Invoke(x).Result |> Result.mapError (fun errs -> String.Join (Environment.NewLine, errs))
    Spec.preval f specification

  /// <summary>
  /// Transforms this specification to work with a array of values.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  [<Extension>]
  static member ForArray (specification : Spec<'T>) = Spec.array specification

  /// <summary>
  /// Maps this specification to work with sequences of values.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  [<Extension>]
  static member ForEnumerable (specification : Spec<'T>) : Spec<IEnumerable<'T>> = Spec.seq specification

  /// <summary>
  /// Determine whether the specified value satisfies the domain specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  [<Extension>]
  static member IsSatisfiedBy (specification : Spec<'T>, untrusted) =
    Spec.isSatisfiedBy untrusted specification

  /// <summary>
  /// Validate the specified value to the domain specification.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  [<Extension>]
  static member Validate (specification : Spec<'T>, untrusted) =
    ValidationResult<'T> (result=Spec.validate untrusted specification)

  /// <summary>
  /// Validate the specified value to the domain <paramref name="specification"/>,
  /// throw an <typeparamref name="TException"/> exception otherwise with the given <paramref name="message"/>.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="message">The error message that the thrown exception of type <typeparamref name="TException"/> should have.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="message"/> is <c>null</c>.</exception>
  /// <exception cref="TException">Thrown when the <paramref name="untrusted"/> is not considered valid according to the given <paramref name="specification"/></exception>
  [<Extension>]
  static member ValidateThrow<'T, 'TException when 'TException :> exn> (specification : Spec<'T>, untrusted, message : string) : 'T =
    if isNull message then nullArg "message" 
    Spec.validate untrusted specification
    |> Result.getOrElse (fun _ -> 
      raise (Activator.CreateInstance(typeof<'TException>, [| message :> obj |]) :?> 'TException))

  /// <summary>
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should look like.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">
  ///   The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/>.
  /// </param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member CreateModel (specification : Spec<'T>, untrusted, creator : Func<'T, 'TResult>) =
    if isNull creator then nullArg "creator"
    ValidationResult<'TResult> (result=Spec.createModel creator.Invoke untrusted specification)

  /// <summary>
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/></param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member CreateModel (specification : Spec<'T>, untrusted, creator : Func<'T, ValidationResult<'TResult>>) =
    if isNull creator then nullArg "creator"
    ValidationResult<'TResult> (result=Spec.createModelWith (creator.Invoke >> fun r -> r.Result) untrusted specification)

  /// <summary>
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">
  ///   The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/>
  /// </param>
  /// <param name="result">
  ///   The resulting model that will be created when the <paramref name="untrusted"/> value is considered valid according to the given <paramref name="specification"/>
  /// </param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member TryCreateModel (specification : Spec<'T>, untrusted, creator : Func<'T, 'TResult>, result : outref<'TResult>) =
    if isNull creator then nullArg "creator"
    match Spec.createModel creator.Invoke untrusted specification with
    | Ok x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'TResult>; false

  /// <summary>
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds,
  /// throw an <typeparamref name="TException"/> exception otherwise with the given <paramref name="message"/>.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/></param>
  /// <param name="message">The error message that the thrown exception should have.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> or <paramref name="message"/> is <c>null</c>.</exception>
  /// <exception cref="TException">Thrown when the <paramref name="untrusted"/> is not considered valid according to the given <paramref name="specification"/></exception>
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult, 'TException when 'TException :> exn> (specification : Spec<'T>, untrusted, creator : Func<'T, 'TResult>, message) =
    if isNull creator then nullArg "creator"
    if isNull message then nullArg "message"
    Spec.createModel creator.Invoke untrusted specification
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (Activator.CreateInstance (typeof<'TException> , [| message :> obj |]) :?> 'TException))
  
  /// <summary>
  /// Create a domain model after the validate of the specifed value to the domain specification succeeds,
  /// throw an <see cref="ValidationFailureException"/> exception otherwise with the given <paramref name="message"/>.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/></param>
  /// <param name="message">The error message that the thrown exception should have.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> or <paramref name="message"/> is <c>null</c>.</exception>
  /// <exception cref="ValidationFailureException">Thrown when the <paramref name="untrusted"/> is not valid according to the given <paramref name="specification"/></exception>
  [<Extension>]
  static member CreateModelOrThrow (specification : Spec<'T>, untrusted, creator : Func<'T, 'TResult>, message) =
    if isNull creator then nullArg "creator"
    if isNull message then nullArg "message"
    Spec.createModel creator.Invoke untrusted specification
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (ValidationFailureException message))

  /// <summary>
  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">
  ///   The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/>
  /// </param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member CreateModel (specification, untrusted, creator : Func<'T, 'TResult>) =
    if isNull creator then nullArg "creator"
    ValidationResult<'TResult> (
      result=Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted)

  /// <summary>
  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">
  ///   The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/>
  /// </param>
  /// <param name="result">
  ///   The resulting model that will be created when the <paramref name="untrusted"/> value is considered valid according to the given <paramref name="specification"/>.
  /// </param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> is <c>null</c>.</exception>
  [<Extension>]
  static member TryCreateModel (specification, untrusted, creator : Func<'T, 'TResult>, result : outref<'TResult>) =
    if isNull creator then nullArg "creator"
    match Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted with
    | Ok x -> result <- x; true
    | _ -> result <- Unchecked.defaultof<'TResult>; false

  /// <summary>
  /// Tries to get the wrapped value out of the untrusted boundary by validating the value.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created.</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">
  ///   The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/>
  /// </param>
  /// <param name="message">The error message that the thrown exception of type <typeparamref name="TException"/> should have.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> or <paramref name="message"/> is <c>null</c>.</exception>
  /// <exception cref="ValidationFailureException">Thrown when the <paramref name="untrusted"/> is not valid according to the given <paramref name="specification"/></exception>
  [<Extension>]
  static member CreateModelOrThrow (specification, untrusted, creator : Func<'T, 'TResult>, message) =
    if isNull creator then nullArg "creator"
    if isNull message then nullArg "message"
    Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (ValidationFailureException message))

  /// <summary>
  /// Tries to get the wrapped value out of the untrusted boundary by validating the value, 
  /// throw an <typeparamref name="TException"/> exception otherwise with the given <paramref name="message"/>.
  /// </summary>
  /// <param name="specification">The instance that specifies how the model should be created</param>
  /// <param name="untrusted">The untrusted value that should be tried to create into a model according to the given <paramref name="specification"/></param>
  /// <param name="creator">The function that will create model after the <paramref name="untrusted"/> value is valid according to the given <paramref name="specification"/></param>
  /// <param name="message">The error message that the thrown exception of type <typeparamref name="TException"/> should have.</param>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="creator"/> or <paramref name="message"/> is <c>null</c>.</exception>
  /// <exception cref="TException">Thrown when the <paramref name="untrusted"/> value isn't valid according to the given <paramref name="specification"/></exception>
  [<Extension>]
  static member CreateModelOrThrow<'T, 'TResult, 'TException when 'TException :> exn> (specification, untrusted, creator : Func<'T, 'TResult>, message) =
    if isNull creator then nullArg "creator"
    if isNull message then nullArg "message"
    Untrust.getWithResult (fun x -> Spec.createModel creator.Invoke x specification) untrusted
    |> Result.getOrElse (fun errs ->
      let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
      let message = sprintf "%s: %s" message errors
      raise (Activator.CreateInstance (typeof<'TException> , [| message :> obj |]) :?> 'TException))
