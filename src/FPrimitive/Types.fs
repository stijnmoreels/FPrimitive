namespace FPrimitive

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices
open System.Threading
open System.Xml

open Microsoft.FSharp.Core

/// Model representing a string containing only charaters from the alphabet.
type Alphabetical = 
  private Alphabethical of string with
    /// Gets the original string representation.
    member this.Value = match this with Alphabethical x -> x
    /// Try to create a string model with only charaters from the alphabet.
    static member create (txt) = specModel Alphabethical txt {
        notNullOrWhiteSpace "alphabetical value cannot be 'null' or blank"
        alphabetical "alphabetical value should only contain charaters from the alphabet (any case)"
        cascade FirstFailure }
    /// Try to create a string model with only charaters from the alphabet.
    static member Create (txt) = ValidationResult (result=Alphabetical.create txt)
    static member op_Implicit (x : Alphabetical) = x.Value

/// Model representation of an alphanumerical value.
type Alphanum =
    private Alphanum of string with
    /// Gets the original string representation of the alphanumerical value.
    member this.Value = match this with Alphanum x -> x
    /// Try to create an alphanumerical value from a given raw text.
    static member create (txt) = specModel Alphanum txt {
        notNullOrWhiteSpace "alphanumerical value cannot be 'null' or blank"
        alphanum "alphanumerical value should only contain alphanumerical values"
        cascade FirstFailure }
    /// Try to create an alphanumerical value from a given raw text.
    static member Create (txt) = ValidationResult (result=Alphanum.create txt)
    static member op_Implicit (x : Alphanum) = x.Value

/// Model representation of an alphanumerical value or special charaters.
type AlphanumSpecial =
  internal AlphanumSpecial of value:string with
    /// Gets the original string representation of the alphanumerical value or special charaters.
    member this.Value = match this with AlphanumSpecial x -> x
    /// Try to create an alphanumerical value from a given raw text.
    static member create (txt) = specModel AlphanumSpecial txt {
        notNullOrWhiteSpace "alphanumerical value cannot be 'null' or blank"
        alphanumSpecial "alphanumerical value should only contain alphanumerical values or special charaters"
        cascade FirstFailure }
    /// Try to create an alphanumerical value or special charaters from a given raw text.
    static member Create (txt) = ValidationResult (result=AlphanumSpecial.create txt)
    static member op_Implicit (x : AlphanumSpecial) = x.Value

/// Model representation of an integer bounded by a minimum and maximum threshold.
type BoundedInt =
  private BoundedInt of min:int * max:int * value:int with 
  /// Gets the bounded integer value.
  member this.Value = match this with BoundedInt (_, _, x) -> x
  /// Try to create a bounded integer value within the given minimum and maximum boundaries.
  static member create (min, max) value = specModel BoundedInt (min, max, value) {
     add (fun (min, max, _) -> min < max, "minimum boundary should not be greater than maximum")
     add (fun (min, max, value) -> value > min && value < max, "value of bounded int should be between minimum and maximum boundary") }
  /// Try to create a bounded integer value within the given minimum and maximum boundaries.
  static member Create (min, max, value) =
      ValidationResult (result=BoundedInt.create (min, max) value)
  static member op_Implicit (x : BoundedInt) = x.Value

/// Model representing a sequence of elements with at least a single element.
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
        cascade FirstFailure }
    /// Tries to create an non-empty sequence model of a given sequence.
    static member Create (values : 'T seq) =
        ValidationResult (result=NonEmptySeq.create values)

    /// Projects each element of the non-empty sequence to a new form.
    member this.Select (selector : Func<_, _>) =
        if isNull selector then nullArg "selector"
        Enumerable.Select (this.Values, selector) |> NonEmptySeq
    /// Projects each element of the non-empty sequence to a new non-empty sequence and flattens the resulting non-empty sequences into one non-empty sequence
    member this.TrySelectMany (selector : Func<_, NonEmptySeq<_>>) =
        if isNull selector then nullArg "selector"
        Enumerable.SelectMany (this.Values, selector.Invoke >> fun xs -> xs.Values) |> NonEmptySeq.Create
    /// Filters the elements in the non-empty sequence and tries to create a non-empty sequence of the resulting elements.
    member this.TryWhere (selector : Func<_, _>) =
        Enumerable.Where (this.Values, selector) |> NonEmptySeq.Create

/// Model representing a sequence of elements with at least a single element.
type 'T nonEmptySeq = NonEmptySeq<'T>

/// Model representing a sequence where each element is considered to be unique.
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
        notNull "can't create unique sequence model because 'null' was passed in"
        nonEmpty "can't create unique sequence model of given empty sequence"
        unique selectKey "can't create unique sequence model since the sequence contains duplicate elements"
        cascade FirstFailure }
    /// Tries to create a unique sequence model of the given sequence, 
    /// using the given selector to select the key to compare the elements in the sequence.
    static member Create (values : 'T seq, selectKey : Func<'T, 'TKey>) =
        if isNull values then nullArg "values"
        if isNull selectKey then nullArg "selectKey"
        ValidationResult (result=UniqueSeq<'T, 'TKey>.createWith selectKey.Invoke values)
    /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
    static member convertWith selectKey (values : 'T seq) =
        Seq.distinctBy selectKey values
        |> UniqueSeq<'T, 'TKey>.createWith selectKey
    /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
    static member Convert (values : 'T seq, selectKey : Func<'T, 'TKey>) =
        if isNull selectKey then nullArg "selectKey"
        ValidationResult (result=UniqueSeq<'T, 'TKey>.convertWith selectKey.Invoke values)

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

/// Model representing a sequence where each element is considered to be unique.
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
        ValidationResult (result=UniqueSeq<'T>.create values)
    /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
    static member convert values =
        if isNull values then Seq.empty else values
        |> Seq.distinct
        |> UniqueSeq<'T>.create
    /// Tries to convert a sequence of elements to a unique sequence based on the key that gets selected from the given function.
    static member Convert (values : 'T seq) =
        ValidationResult (result=UniqueSeq<'T>.convert values)

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
type 'T uniqueSeq when 'T : equality = UniqueSeq<'T>

[<Extension>]
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
    let map f (xs : NonEmptySeq<_>) = Seq.map f xs |> NonEmptySeq

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
    let filter f (xs : UniqueSeq<_, _>) = 
        UniqueSeq<_, _> (Seq.filter f xs, xs.selectKey)

    /// Filters the elements in the non-empty sequence and tries to create a non-empty sequence of the resulting elements.
    let tryFilter f (xs : NonEmptySeq<_>) =
        Seq.filter f xs |> NonEmptySeq.create

/// Representation of a XML text string that can be serialized to XML.
/// Not necessary an XML document, but an input text that contains valid XML characters.
/// Can be used instead of string in a domain model type.
type XmlEncodedString private (value) =
    /// Gets the original value.
    member __.Value = value
    /// Tries to creates a XML encoded string from a raw string.
    static member create value = specModel XmlEncodedString value {
        notNull "can't create xml string because this type doesn't allow 'null' values"
        forallOf (fun (s : string) -> s.ToCharArray ()) XmlConvert.IsXmlChar "can't create xml string because not all characther are considered valid XML"
        cascade FirstFailure }
    /// Tries to creates a XML encoded string from a raw string.
    static member Create value =
        ValidationResult (result=XmlEncodedString.create value)

    /// Tries to create a XML encoded string from a raw string,
    /// after HTML encoding the string.
    static member convert value =
        System.Net.WebUtility.HtmlEncode value |> XmlEncodedString.create
    /// Tries to create a XML encoded string from a raw string,
    /// after HTML encoding the string.
    static member Convert value =
        ValidationResult (result=XmlEncodedString.convert value)

    interface IEquatable<XmlEncodedString> with
        member __.Equals (other : XmlEncodedString) = value.Equals(other.Value)
    override __.Equals (other) = 
        match other with 
        | :? XmlEncodedString as other -> value.Equals(other.Value)
        | _ -> false 
    override __.GetHashCode () = value.GetHashCode ()
    override __.ToString () = value
    static member op_Implicit (xml : XmlEncodedString) = xml.ToString ()

[<AutoOpen>]
module ActivePatterns =
    let (|Alphabetical|) (x : Alphabetical) = x.Value
    let (|Alphanum|) (x : Alphanum) = x.Value
    let (|AlphanumSpecial|) (x : AlphanumSpecial) = x.Value
    let (|BoundedInt|) (x : BoundedInt) = x.Value
    let (|XmlEncodedString|) (x : XmlEncodedString) = x.Value

[<AutoOpen>]
module StringExtensions =

    type String with
        static member toXmlEncoded x = XmlEncodedString.create x
        static member convertToXmlEncoded x = XmlEncodedString.convert x

/// Exception that gets thrown when a `ReadOnce<_>` instance was being read more than once.
exception AlreadyReadException of string

/// Representation of a value that can only be read once.
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

/// Representation of a value that can only be read once.
type 'T readOnce = ReadOnce<'T>

/// Representation of a value that can only be read once.
module ReadOnce =
    /// Tries to read the read-once value.
    let tryGetValue (x : ReadOnce<_>) = x.tryGetValue ()
    /// Tries to read the read-once value.
    let tryGetValueResult x error = tryGetValue x |> Result.ofOption error

/// Exception that gets thrown when a `Write<_>` instance was being written more than once.
exception AlreadyWrittenException of string

/// Representation of a value that can only be written once.
type WriteOnce<'T> (write : 'T -> unit) =
    /// Creates a representation of a value that can only be written once.
    new (writeAction : Action<'T>) = WriteOnce<'T> (write=writeAction.Invoke)

    member val private Fun = ref (write :> obj)
    /// Tries to write the value to the write-once function.
    [<CompiledName("TrySetValue")>]
    member this.trySetValue (value) =
      let f = Interlocked.Exchange (this.Fun, null)
      if f <> null then (f :?> 'T -> unit) value; true
      else false
    /// Write the value to the write-once function and throws an 'AlreadyWrittenException` when the value was already written.
    member this.SetValue (value) =
      if not <| this.trySetValue value
      then raise (AlreadyWrittenException (sprintf "The value of type %s has already been written" typeof<'T>.Name))

/// Representation of a function that can be written once.
module WriteOnce =
  /// Creates a representation of a value that can only be written once.
  let create f = WriteOnce (write=f)
  /// Tries to write the value to the write-once function.
  let trySetValue value (x : WriteOnce<_>) = x.trySetValue value

/// Representation of a value that can be disposed/removed any time.
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
module Disposable =
  /// Creates a disposable resource from any value.
  let create (x : 'a) = new Disposable<'a> (x)
  /// Tries to read the possible disposed value.
  let tryGetValue (x : Disposable<_>) = x.tryGetValue ()
  /// Tries to read the possible disposed value.
  let tryGetValueResult x error = tryGetValue x |> Result.ofOption error

