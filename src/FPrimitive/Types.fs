namespace FPrimitive

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices
open System.Threading

open Microsoft.FSharp.Core
open System.Threading.Tasks
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open System.Reflection
open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions

#nowarn "1001"
#nowarn "0044"

/// Type alias for a result type with an exception as error.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type Try<'T> = Result<'T, exn>

/// Operations on the result type with an exception as error.
[<Obsolete("Remove unnecessary types in future release")>]
[<ExcludeFromCodeCoverage>]
module Try = 
  /// Create a `Try` alias by creating an exception for the error of the result.
  let forEx createException (result : Result<'T, 'TError>) : Try<'T> =
    match result with
    | Error args -> Error (createException args :> exn)
    | Ok x -> Ok x

  let internal appendCore item (ex : exn) =
    match ex with
    | :? AggregateException as aex -> AggregateException (aex.InnerExceptions.Append item) :> exn
    | ex -> AggregateException (seq { yield ex; yield item }) :> exn

  /// Append an exception to the exception of the `Try`.
  let append item (result : Try<'T>) : Try<'T> =
    match result with
    | Error ex -> Error (appendCore item ex)
    | _ -> result

  /// Raise the exception in the `Try` when the type alias result is indeed an `Error`.
  let ifErrorRaise (result : Try<'T>) =
    match result with
    | Error ex -> raise ex
    | _ -> ()

  /// Get the value of the `Try` or raise the exception.
  let getOrRaise (r : Try<_>) = Result.either id raise r

  /// Create a `Try` alias when running a function for an exception that satisfied the given exception filter.
  let createWhen exFilter f x : Try<_> =
    try Ok (f x)
    with ex when exFilter ex -> Error ex

  /// Create a `Try` alias when running a function.
  let create f x = createWhen (fun _ -> true) f x

/// Model representation of a valididated 'T.
[<Struct>]
[<Obsolete("Remove unnecessary types in future release")>]
[<ExcludeFromCodeCoverage>]
type Valid<'T> =
  private Validated of 'T with
    /// Gets the inner validated value.
    member this.Value = match this with Validated x -> x
    /// Create a valid representation of 'T
    static member create (value : 'T) spec =
      Spec.createModel Validated value spec
    /// Create a valid representation of 'T
    static member Create (value : 'T, spec) =
      ValidationResult<Valid<'T>> (Valid<'T>.create value spec)
      |> ValidationResult.op_Implicit

    /// Create a valid representation of 'T
    static member createWith value predicate message =
      if predicate value then Ok <| Validated value
      else Error message
    /// Create a valid representation of 'T
    static member Create (value : 'T, predicate : Func<'T, _>, [<ParamArray>] messages : string array) =
      if isNull predicate then nullArg "predicate"
      if predicate.Invoke value then Outcome.Success (Validated value)
      else Outcome.Failure (messages)
    static member op_Implicit (valid : Valid<'T>) = valid.Value

/// Representation of a 'secure' structure of how an error can be reported.
/// Explicitly stating what information will be shared with the user.
[<Obsolete("Remove unnecessary types in future release")>]
[<ExcludeFromCodeCoverage>]
type ErrorMessage =
  private { /// Gets the type of information that the application require from the user.
            RequiredFromUser : string
            /// Gets information that should be shared with the user.
            SharedWithUser : IDictionary<string, string>
            /// Gets the explanation of how the user should solve the error.
            ErrorSolving : string } with
    /// <summary>
    /// Creates a new instance of the <see cref="ErrorMessage"/> class.
    /// </summary>
    /// <param name="requiredFromUser">The explanation of what's required from the user (ex: 'Requires a valid input for person's firstname').</param>
    /// <param name="sharedWithUser">The collection of values that will be shared with the user, be careful of what's in here.</param>
    /// <param name="errorSolving">The explanation of how the user can solve the error.</param>
    /// <exception cref="ArgumentException">Thrown when the <paramref name="requiredFromUser"/> or <paramref name="errorSolving"/> is blank, or the <paramref name="requiredFromUser"/> doesn't conform with the expected format.</exception>
    /// <exception cref="ArgumentNullException">Thrown when the <paramref name="sharedWithUser"/> is <c>null</c></exception>
    static member Create (requiredFromUser, sharedWithUser, errorSolving) =
      if String.IsNullOrWhiteSpace requiredFromUser 
      then invalidArg "requiredFromUser" "Requires a non-blank explanation of what's' required from the user"
      if isNull sharedWithUser then nullArg "sharedWithUser"
      if String.IsNullOrWhiteSpace errorSolving 
      then invalidArg "errorSolving" "Requires a non-blank explanation of how the user's supposed to sovle the error"
      if not <| Regex.IsMatch (requiredFromUser, "^[a-zA-Z,\.']+$") 
      then invalidArg "requiredFromUser" "Requires a 'simple' explenation of the error, containing only a-z & A-Z characthers"
      { RequiredFromUser = requiredFromUser
        SharedWithUser = sharedWithUser
        ErrorSolving = errorSolving }
    override this.ToString () =
      sprintf "Required from user: %s%sContext:%A%sHow to solve:%s" 
        this.RequiredFromUser Environment.NewLine
        this.SharedWithUser Environment.NewLine
        this.ErrorSolving

/// Represents a way of a 'secure' application exception 
/// that explicitly states what information should be shared with the user.
[<Serializable>]
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type SecureApplicationException =
  inherit ApplicationException
  /// Initializes a new instance of the <see cref="SecureApplicationException" /> class.
  new () = { inherit ApplicationException () }
  /// <summary>
  /// Initializes a new instance of the <see cref="SecureApplicationException" /> class.
  /// </summary>
  /// <param name="message">The message that describes the exception.</param>
  new (message) = { inherit ApplicationException (message) }
  /// <summary>
  /// Initializes a new instance of the <see cref="SecureApplicationException" /> class.
  /// </summary>
  /// <param name="message">The message that describes the exception.</param>
  /// <param name="innerException">The exception that is the cause of the current exception.</param>
  new (message, innerException : exn) = { inherit ApplicationException (message, innerException) }
  /// <summary>
  /// Initializes a new instance of the <see cref="SecureApplicationException" /> class.
  /// </summary>
  /// <param name="requiredFromUser">The explanation of what's required from the user (ex: 'Requires a valid input for person's firstname')</param>
  /// <param name="sharedWithUser">The collection of values that will be shared with the user, be careful of what's in here.</param>
  /// <param name="errorSolving">The explanation of how the user can solve the error.</param>
  /// <param name="innerException">The exception that is the cause of the current exception.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="requiredFromUser"/> or <paramref name="errorSolving"/> is blank, or the <paramref name="requiredFromUser"/> doesn't conform with the expected format.</exception>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="sharedWithUser"/> is <c>null</c></exception>
  new (requiredFromUser, sharedWithUser, errorSolving, innerException) =
    let msg = ErrorMessage.Create (requiredFromUser, sharedWithUser, errorSolving)
    { inherit ApplicationException (string msg, innerException) }
  /// <summary>
  /// Initializes a new instance of the <see cref="SecureApplicationException" /> class.
  /// </summary>
  /// <param name="requiredFromUser">The explanation of what's required from the user (ex: 'Requires a valid input for person's firstname')</param>
  /// <param name="sharedWithUser">The collection of values that will be shared with the user, be careful of what's in here.</param>
  /// <param name="errorSolving">The explanation of how the user can solve the error.</param>
  /// <exception cref="ArgumentException">Thrown when the <paramref name="requiredFromUser"/> or <paramref name="errorSolving"/> is blank, or the <paramref name="requiredFromUser"/> doesn't conform with the expected format.</exception>
  /// <exception cref="ArgumentNullException">Thrown when the <paramref name="sharedWithUser"/> is <c>null</c></exception>
  new (requiredFromUser, sharedWithUser, errorSolving) =
    let msg = ErrorMessage.Create (requiredFromUser, sharedWithUser, errorSolving)
    { inherit ApplicationException (string msg) }

/// Model with operations related to discriminated unions.
[<ExcludeFromCodeCoverage>]
[<Obsolete("Remove unnecessary types in future releases")>]
type Union =
  /// Determines if the given string is a union case entry of the given type.
  [<CompiledName("IsUnionCase")>]
  static member isUnionCase<'T> (s : string, [<Optional; DefaultParameterValue(BindingFlags.Default)>] bindingFlags : BindingFlags) =
    FSharpType.IsUnion (typeof<'T>, bindingFlags)
    && FSharpType.GetUnionCases (typeof<'T>, bindingFlags)
       |> Array.exists (fun c -> c.Name = s)
  /// Converts a string to a discriminated union entry with optional args during creation.
  static member ofString<'T, 'TArg> (s : string, [<Optional; DefaultParameterValue(BindingFlags.Default)>] bindingFlags : BindingFlags, [<ParamArray>] args) =
    try if FSharpType.IsUnion (typeof<'T>, bindingFlags)
        then let args = Array.map box<'TArg> args |> Array.ofSeq
             FSharpType.GetUnionCases (typeof<'T>, bindingFlags)
             |> Array.filter (fun case -> case.Name = s)
             |> function
                  | [|case|] -> Some (FSharpValue.MakeUnion(case, args, bindingFlags) :?> 'T)
                  |_ -> None
        else None
    with _ -> None
  /// Converts a string to a discriminated union entry with optional args during creation.
  static member OfString<'T, 'TArg> (s : string, [<Optional>] bindingFlags : BindingFlags option, [<ParamArray>] args) =
    Union.ofString<'T, 'TArg> (s, defaultArg bindingFlags BindingFlags.Default, args) |> Maybe.OfOption
  /// Validates a string to a discriminated union entry with optional args during creation.
  [<CompiledName("Create")>]
  static member create<'T, 'TArg> (s : string, [<Optional; DefaultParameterValue(BindingFlags.Default)>] bindingFlags : BindingFlags, [<ParamArray>] args) =
    let message = "cannot create union case because the input string doesn't represent a union case entry field"
    Spec.def<string>
    |> Spec.verify (fun s -> Union.isUnionCase<'T> (s, bindingFlags)) message
    |> Spec.createModelWith (fun x -> 
        Union.ofString<'T, 'TArg> (x, bindingFlags, args) 
        |> Option.toResult (Map.create "union" [message])) s
  /// Validates a string to a discriminated union entry with optional args during creation.
  static member create<'T> (s : string, [<Optional; DefaultParameterValue(BindingFlags.Default)>] bindingFlags : BindingFlags) =
    Union.create<'T, obj> (s, bindingFlags, Array.empty)
  /// Validates a string to a discriminated union entry with optional args during creation.
  static member create<'T, 'TArg> (s : string, [<ParamArray>] args) =
    Union.create<'T, 'TArg> (s, BindingFlags.Default, args)
  /// Validates a string to a discriminated union entry with optional args during creation.
  static member create<'T> (s : string) =
    Union.create<'T, obj> (s, BindingFlags.Default, Array.empty)

/// Model representing a sequence of elements with at least a single element.
[<CompiledName("NonEmptyEnumerable")>]
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type NonEmptySeq<'T> =
  internal NonEmptySeq of values:seq<'T> with
    member private this.Values = match this with NonEmptySeq xs -> xs
    interface IEnumerable<'T> with
      member this.GetEnumerator () = this.Values.GetEnumerator ()
      member this.GetEnumerator () = this.Values.GetEnumerator () :> IEnumerator
    /// Tries to create a non-empty sequence model of a given sequence.
    static member create (values : 'T seq) = specModel NonEmptySeq values {
      notNull "can't create non-empty sequence because 'null' was passed in"
      nonEmpty "can't create non-empty sequence model of given empty sequence"
      fornoneNull "can't create non-empty sequence because one or more entries was 'null'"
      cascade FirstFailure }
    /// Tries to create an non-empty sequence model of a given sequence.
    static member Create (values : 'T seq) = NonEmptySeq.create values |> ValidationResult<NonEmptySeq<'T>>

    /// Projects each element of the non-empty sequence to a new form.
    member this.Select<'TResult> (selector : Func<'T, 'TResult>) =
      if isNull selector then nullArg "selector"
      Enumerable.Select (this.Values, selector) |> NonEmptySeq
    /// Projects each element of the non-empty sequence to a new non-empty sequence and flattens the resulting non-empty sequences into one non-empty sequence
    member this.TrySelectMany<'TResult> (selector : Func<'T, NonEmptySeq<'TResult>>) =
      if isNull selector then nullArg "selector"
      Enumerable.SelectMany (this.Values, selector.Invoke >> fun xs -> xs.Values) |> NonEmptySeq.Create
    /// Filters the elements in the non-empty sequence and tries to create a non-empty sequence of the resulting elements.
    member this.TryWhere (predicate : Func<'T, bool>) =
      if isNull predicate then nullArg "predicate"
      Enumerable.Where (this.Values, predicate) |> NonEmptySeq.Create
    /// Concatenates to non-empty sequences.
    member this.Concat (other : NonEmptySeq<'T>) =
      Enumerable.Concat (this.Values, other.Values) |> NonEmptySeq
    /// Applies a specified function to the corresponding elements of two non-empty sequences, producing a sequences of the results.
    member this.Zip<'TOther, 'TResult> (other : NonEmptySeq<'TOther>, zipper : Func<'T, 'TOther, 'TResult>) =
      if isNull zipper then nullArg "zipper"
      Enumerable.Zip (this.Values, other.Values, zipper) |> NonEmptySeq

/// Model representing a sequence of elements with at least a single element.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type 'T nonEmptySeq = NonEmptySeq<'T>

/// Model representing a sequence where each element is considered to be unique.
[<CompiledName("UniqueEnumerable`2"); AllowNullLiteral(false)>]
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type UniqueSeq<'T, 'TKey when 'TKey : equality> internal (values : 'T seq, selectKey) =
  member internal __.values = values
  member internal __.selectKey = selectKey
  interface IEnumerable<'T> with
    member __.GetEnumerator () = values.GetEnumerator ()
    member __.GetEnumerator () = values.GetEnumerator () :> IEnumerator
  interface IEquatable<UniqueSeq<'T, 'TKey>> with
     member __.Equals (other : UniqueSeq<'T, 'TKey>) = other.values.SequenceEqual(values)
  override __.Equals (other : obj) =
    match other with
    | :? UniqueSeq<'T, 'TKey> as other -> other.values.SequenceEqual(values)
    | _ -> false
  override __.GetHashCode () = values.GetHashCode ()
    
  /// Tries to create a unique sequence model of the given sequence, 
  /// using the given selector to select the key to compare the elements in the sequence.
  static member createWith selectKey values = specModel (fun x -> UniqueSeq (x, selectKey)) values {
    nonEmpty "can't create unique sequence model of given empty sequence"
    fornoneNull "can't create unique sequence when one or more elements are 'null'"
    unique selectKey "can't create unique sequence model since the sequence contains duplicate elements"
    cascade FirstFailure }
  /// Tries to create a unique sequence model of the given sequence, 
  /// using the given selector to select the key to compare the elements in the sequence.
  static member Create (values : 'T seq, selectKey : Func<'T, 'TKey>) =
    if isNull values then nullArg "values"
    if isNull selectKey then nullArg "selectKey"
    ValidationResult<UniqueSeq<'T, 'TKey>> (result=UniqueSeq<'T, 'TKey>.createWith selectKey.Invoke values)
  /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
  static member convertWith selectKey (values : 'T seq) =
    Seq.distinctBy selectKey values
    |> UniqueSeq<'T, 'TKey>.createWith selectKey
  /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
  static member Convert (values : 'T seq, selectKey : Func<'T, 'TKey>) =
    if isNull selectKey then nullArg "selectKey"
    ValidationResult<UniqueSeq<'T, 'TKey>> (result=UniqueSeq<'T, 'TKey>.convertWith selectKey.Invoke values)

  /// Projects the elements in the unique sequence to a new form and tries to create a new unique sequence with the projected elements,
  /// using the given selector to select the key to compare the projected elements in the resulted sequence.
  member __.TrySelect (selector : Func<'T, 'TResult>, selectKey : Func<'TResult, 'TOtherKey>) =
    if isNull selector then nullArg "selector"
    if isNull selectKey then nullArg "selectKey"
    UniqueSeq<'TResult, 'TOtherKey>.Create (Enumerable.Select (values, selector), selectKey)
  /// Filters the elements in the unique sequence and creates a new unique sequence of the resulting elements.
  member __.Where (predicate : Func<'T, bool>) =
    if isNull predicate then nullArg "predicate"
    UniqueSeq<'T, 'TKey> (Enumerable.Where (values, predicate), selectKey)
  /// Transforms sequence with unique values to dictionary structure.
  member __.ToDictionary () = values.ToDictionary (Func<_, _> selectKey)

/// Model representing a sequence where each element is considered to be unique.
[<CompiledName("UniqueEnumerable`1"); AllowNullLiteral(false)>]
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type UniqueSeq<'T when 'T : equality> (values) = 
  inherit UniqueSeq<'T, 'T> (values, id)
  /// Tries to create a unique sequence model of the given sequence
  /// by comparing the elements itself.
  static member create values = 
    UniqueSeq<'T, 'T>.createWith id values
    |> Result.map (fun x -> UniqueSeq<'T> x.values)
  /// Tries to create a unique sequence model of the given sequence
  /// by comparing the elements itself.
  static member Create (values : 'T seq) =
    if isNull values then nullArg "values"
    ValidationResult<UniqueSeq<'T>> (result=UniqueSeq<'T>.create values)
  /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
  static member convert values =
    if isNull values then Seq.empty else values
    |> Seq.distinct
    |> UniqueSeq<'T>.create
  /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
  static member Convert (values : 'T seq) =
    ValidationResult<UniqueSeq<'T>> (result=UniqueSeq<'T>.convert values)

  /// Projects the elements in the unique sequence to a new form and tries to create a new unique sequence with the projected elements,
  /// using the given selector to select the key to compare the projected elements in the resulted sequence.
  member __.TrySelect (selector : Func<'T, 'TResult>) =
    if isNull selector then nullArg "selector"
    UniqueSeq<'TResult>.Create (Enumerable.Select (values, selector))
  /// Filters the elements in the unique sequence and creates a new unique sequence of the resulting elements.
  member __.Where (predicate : Func<'T, bool>) =
    if isNull predicate then nullArg "predicate"
    UniqueSeq<'T> (Enumerable.Where (values, predicate))

/// Model representing a sequence where each element is considered to be unique.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type 'T uniqueSeq when 'T : equality = UniqueSeq<'T>

[<Extension>]
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type SeqExtensions () =
  /// Tries to create a non-empty sequence model of a given sequence.
  [<Extension>]
  static member AsNonEmpty (values : 'T seq) = 
    if isNull values then nullArg "values"
    NonEmptySeq.Create values
  /// Tries to create a unique sequence model of the given sequence
  /// by comparing the elements itself.
  [<Extension>]
  static member AsUnique (values : 'T seq) =
    if isNull values then nullArg "values"
    UniqueSeq<'T>.Create values
  /// Tries to create a unique sequence model of the given sequence, 
  /// using the given selector to select the key to compare the elements in the sequence.
  [<Extension>]
  static member AsUnique (values : 'T seq, selectKey : Func<'T ,'TKey>) =
    if isNull values then nullArg "values"
    if isNull selectKey then nullArg "selectKey"
    UniqueSeq<'T, 'TKey>.Create (values, selectKey)

/// Operations on sequence domain types.
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
module Seq =
  /// Tries to create a non-empty sequence model of a given sequence.
  let toNonEmpty xs = NonEmptySeq.create xs
  /// Tries to create a non-empty sequence model of a given sequence.
  [<Obsolete("Use 'toNonEmpty' instead")>]
  let nonEmpty xs = toNonEmpty xs
  
  /// Tries to create a unique sequence model of the given sequence by comparing the elements itself.
  let toUnique xs = UniqueSeq<_>.create xs
  /// Tries to create a unique sequence model of the given sequence by comparing the elements itself.
  [<Obsolete("Use 'toUnique' instead")>]
  let unique xs = toUnique xs

  /// Tries to create a unique sequence model of the given sequence, 
  /// using the given selector to select the key to compare the elements in the sequence.
  let toUniqueBy selectKey xs = UniqueSeq<_, _>.createWith selectKey xs
  /// Tries to create a unique sequence model of the given sequence, 
  /// using the given selector to select the key to compare the elements in the sequence.
  [<Obsolete("Use 'toUniqueBy")>]
  let uniqueBy selectKey xs = toUniqueBy selectKey xs

  /// Projects each element of the non-empty sequence to a new form.
  let mapNonEmpty f (xs : NonEmptySeq<_>) = Seq.map f xs |> NonEmptySeq

  /// Projects each element of the unique sequence to a new form, 
  /// and tries to create a unique sequence of the projected elements using the given selector to get the key to compare the projected elements.
  let tryMapWith (selectKey : 'TResult -> 'TKey) (f : 'T -> 'TResult) (xs : UniqueSeq<'T, 'TOld>) = 
    Seq.map f xs |> UniqueSeq<'TResult, 'TKey>.createWith selectKey

  /// Projects each element of the unique sequence to a new form,
  /// ands tries to create a unique sequence of the projected elements.
  let tryMap (f : 'T -> 'TResult) (xs : UniqueSeq<'T>) =
    Seq.map f xs |> UniqueSeq<'TResult>.create

  /// Projects each element to a non-empty sequence and flattens the resulting non-empty sequences into one non-empty sequence.
  let tryCollect (f : _ -> NonEmptySeq<_>) (xs : NonEmptySeq<_>) = 
    Seq.collect f xs |> NonEmptySeq.create

  /// Filters the elements in the unique sequence and creates a new unique sequence with the resulting elements.
  let filterUnique f (xs : UniqueSeq<_, _>) = 
    UniqueSeq<_, _> (Seq.filter f xs, xs.selectKey)

  /// Filters the elements in the non-empty sequence and tries to create a non-empty sequence of the resulting elements.
  let tryFilter f (xs : NonEmptySeq<_>) =
    Seq.filter f xs |> NonEmptySeq.create

  /// Applies a specified function to the corresponding elements of two non-empty sequences, producing a sequences of results.
  let zipNonEmpty (xs : NonEmptySeq<_>) ys f = xs.Zip (ys, f)

/// Exception that gets thrown when a `ReadOnce<_>` instance was being read more than once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
exception AlreadyReadException of string

/// Representation of a value that can only be read once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type ReadOnce<'T> (value : 'T) =
  member val private Value = ref (value :> obj)
  /// Tries to read the read-once value.
  member this.tryGetValue () =
    let value = Interlocked.Exchange (this.Value, null)
    if value <> null then Some (value :?> 'T)
    else None
  /// Reads the read-once value and throws an `AlreadyReadException` when the value was already been read.
   member this.GetValue () =
     match this.tryGetValue () with
     | Some x -> x
     | None -> raise (AlreadyReadException (sprintf "The value of type %s has already been read" typeof<'T>.Name))
   /// Tries to reads the read-once value and throws an `AlreadyReadException` when the value was already been read.
   member this.TryGetValue (output : outref<'T>) =
     match this.tryGetValue () with
     | Some x -> output <- x; true
     | None -> output <- Unchecked.defaultof<'T>; false
   /// Tries to reads the read-once value or return `Maybe<>.Nothing`.
   member this.GetValueOrNothing () =
     this.tryGetValue () |> Maybe.OfOption

/// Representation of a value that can only be read once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type 'T readOnce = ReadOnce<'T>

/// Representation of a value that can only be read once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
module ReadOnce =
  /// Tries to read the read-once value.
  let tryGetValue (x : ReadOnce<_>) = x.tryGetValue ()
  /// Tries to read the read-once value.
  let tryGetValueResult x error = tryGetValue x |> Result.ofOption error
  /// Tries to read the read-once value.
  let tryEvalFunc (readOnce : ReadOnce<('a -> 'b)>) (x : 'a) = 
    readOnce.tryGetValue () |> Option.map (fun f -> f x)
  /// Tries to read the read-once value.
  let tryEvalFuncAsync (readOnce : ReadOnce<('a -> Async<'b>)>) (x : 'a) = async {
    match readOnce.tryGetValue () with
    | Some f -> let! result = f x
                return Some result
    | None -> return None }

[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type ReadOnceExtensions =
  ///Tries to reads the read-once function or return `Maybe<>.Nothing`.
  static member TryEvalFunction (readOnce : ReadOnce<Func<'T, 'TResult>>) (x : 'T) =
    match readOnce.tryGetValue () with
    | Some f -> Maybe.Just (f.Invoke x)
    | None -> Maybe.Nothing ()
  ///Tries to reads the read-once function or return `Maybe<>.Nothing`.
  static member TryEvalFunctionAsync (readOnce : ReadOnce<Func<'T, Task<'TResult>>>) (x : 'T) =
    async { match readOnce.tryGetValue () with
            | Some f -> let! result = Async.AwaitTask (f.Invoke x)
                        return Maybe.Just result
            | None -> return Maybe.Nothing () }
    |> Async.StartAsTask

/// Exception that gets thrown when a `Write<_>` instance was being written more than once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
exception AlreadyWrittenException of string

/// Representation of a value that can only be written once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type WriteOnce<'T, 'TResult> (write : 'T -> 'TResult) =
  /// Creates a representation of a value that can only be written once.
  new (writeFunc : Func<'T, 'TResult>) = WriteOnce<'T, 'TResult> (write=writeFunc.Invoke)

  member val internal Fun = ref (write :> obj)
  /// Tries to write the value to the write-once function.
  member this.trySetValue (value) =
    let f = Interlocked.Exchange (this.Fun, null)
    if f <> null then Some <| (f :?> 'T -> 'TResult) value
    else None
  /// Tries to write the value to the write-once function.
  member this.TrySetValue (value) = 
    this.trySetValue value |> Maybe.OfOption
  /// Tries to write the value to the write-once function.
  member this.TrySetValue (value, result : outref<'TResult>) =
    match this.trySetValue value with
    | Some x -> result <- x; true
    | None -> result <- Unchecked.defaultof<'TResult>; false
  /// Write the value to the write-once function and throws an 'AlreadyWrittenException` when the value was already written.
  member this.SetValue (value) =
    match this.trySetValue value with
    | Some result -> result
    | None -> raise (AlreadyWrittenException (sprintf "The value of type %s has already been written" typeof<'T>.Name))

/// Representation of a value that can only be written once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type WriteOnce<'T> (write : 'T -> unit) =
  inherit WriteOnce<'T, unit> (write=write)
  /// Creates a representation of a value that can only be written once.
  new (writeAction : Action<'T>) = WriteOnce<'T> (write=writeAction.Invoke)
  /// Tries to write the value to the write-once function.
  [<CompiledName("TrySetValue")>]
  member __.trySetValue (value) =
    let f = Interlocked.Exchange (base.Fun, null)
    if f <> null then (f :?> 'T -> unit) value; true
    else false

/// Representation of a value that can only be written once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type WriteOnceAsync<'T, 'TResult> (write : 'T -> Async<'TResult>) =
  /// Creates a representation of a value that can only be written once.
  new (writeFunc : Func<'T, Task<'TResult>>) = WriteOnceAsync<'T, 'TResult> (write=fun x -> writeFunc.Invoke x |> Async.AwaitTask)
  member val private Fun = ref (write :> obj)
  /// Tries to write the value to the write-once function.
  member this.trySetValueAsync (value) = async {
    let f = Interlocked.Exchange (this.Fun, null)
    if f <> null 
    then let! result = (f :?> 'T -> Async<'TResult>) value
         return Some result
    else return None }
  /// Tries to write the value to the write-once function.
  member this.TrySetValueAsync (value) = 
    async { let! result = this.trySetValueAsync value 
            return Maybe.OfOption result } 
    |> Async.StartAsTask
  /// Write the value to the write-once function and returns and error when the value was already written.
  member this.setValueAsync (value) = async {
    let! result = this.trySetValueAsync value
    match result with
    | Some x -> return Ok x
    | None -> return Error (sprintf "The value of type %s has already been written" typeof<'T>.Name) }
  /// Write the value to the write-once function and returns an error when the value was already written.
  member this.SetValueAsync (value) =
    async { let! result = this.setValueAsync value
            return Outcome.OfFSharpResult result } 
    |> Async.StartAsTask

/// Representation of a value that can only be written once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type WriteOnceAsync<'T> (write : 'T -> Async<unit>) =
  inherit WriteOnceAsync<'T, unit> (write=write)
  /// Creates a representation of a value that can only be written once.
  new (writeFunc : Func<'T, Task>) = WriteOnceAsync<'T> (write=fun x -> writeFunc.Invoke x |> Async.AwaitTask)
  member val private Fun = ref (write :> obj)
  /// Tries to write the value to the write-once function.
  member this.trySetValueAsync (value) = async {
    let f = Interlocked.Exchange (this.Fun, null)
    if f <> null 
    then do! (f :?> 'T -> Async<unit>) value
         return true
    else return false }
  /// Tries to write the value to the write-once function.
  member this.TrySetValueAsync (value) = 
    this.trySetValueAsync value |> Async.StartAsTask

/// Representation of a function that can be written once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
module WriteOnce =
  /// Creates a representation of a value that can only be written once.
  let create f = WriteOnce<_> (write=f)
  /// Creates a representation of a value that can only be written once.
  let createWithResult f = WriteOnce<_, _> (write=f)
  /// Creates a representation of a value that can only be written once.
  let createAsync f = WriteOnceAsync<_> (write=f)
  /// Creates a representation of a value that can only be written once.
  let createAsyncWithResult f = WriteOnceAsync<_, _> (write=f)
  /// Tries to write the value to the write-once function.
  let trySetValue value (x : WriteOnce<_>) = x.trySetValue value
  /// Tries to write the value to the write-once function.
  let trySetValueWithResult value (x : WriteOnce<_, _>) = x.trySetValue value
  /// Tries to write the value to the write-once function.
  let trySetValueAsync value (x : WriteOnceAsync<_>) = x.trySetValueAsync value
  /// Tries to write the value to the write-once function.
  let trySetValueAsyncWithResult value (x : WriteOnceAsync<_, _>) = x.trySetValueAsync value

/// Exception thrown when an instance of the `SetOnce<_>` type tries to set the value more than once.
exception AlreadySetException of string

/// Represents a model that can set a value once.
[<Obsolete("Remove unnecessary type in future rel")>]
[<ExcludeFromCodeCoverage>]
type SetOnce<'T when 'T : not struct> (?valueName, ?defaultValue) =
  let valueName = defaultArg valueName "set-once value"
  member val private value = defaultArg defaultValue Unchecked.defaultof<'T> with get, set
  member val private CanSet = ref (true :> obj)
  /// Tries to set the value on this set-once model.  
  member this.trySetValue (value) =
    if Interlocked.Exchange (this.CanSet, false) :?> bool
    then this.value <- value; Some ()
    else None
  /// Tries to set the value on this set-once model.
  member this.TrySetValue (value) =
    this.trySetValue value |> Maybe.OfOption
  /// Tries to set the value on this set-once model.
  member this.TrySetValue (value, result : outref<_>) =
    match this.trySetValue value with
    | Some _ -> result <- true
    | None -> result <- false
  /// Gets or sets the set-once value. Remark that the 'set' will throw an `AlreadySetException` when the value was already setted.
  member this.Value
    with get () = this.value
    and set v = 
      this.trySetValue v
      |> Option.iterNone (fun () -> raise (AlreadySetException (sprintf "The '%s' is already set and can't be setted more than once" valueName)))

/// Representation of a value that can be disposed/removed any time.
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
type Disposable<'a> (value : 'a) =
  member val private RefValue = ref (value :> obj)
  /// Tries to read the possible disposed value.
  member internal this.tryGetValue () =
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
    /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
    member this.Dispose () = 
      Interlocked.Exchange (this.RefValue, null) |> ignore

/// Operations on the `Disposable<_>` type.
[<Obsolete("Remove unnecessary type in future release")>]
[<ExcludeFromCodeCoverage>]
module Disposable =
  /// Creates a disposable resource from any value.
  let create (x : 'a) = new Disposable<'a> (x)
  /// Tries to read the possible disposed value.
  let tryGetValue (x : Disposable<_>) = x.tryGetValue ()
  /// Tries to read the possible disposed value.
  let tryGetValueResult x error = tryGetValue x |> Result.ofOption error


