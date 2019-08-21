namespace FPrimitive

open System
open System.Threading
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices
open System.Xml

open Microsoft.FSharp.Core

/// Model representation of an integer bounded by a minimum and maximum threshold.
type BoundedInt (min : int, max : int, value : int) = 
    do if min > max then invalidArg "min" "Minimum boundary should not be greater than maximum"
       if value < min || value > max then invalidArg "value" "Value of bounded int should be between minimum and maximum boundary"
    /// Gets the bounded integer value.
    member __.Value = value
    /// Try to create a bounded integer value within the given minimum and maximum boundaries.
    static member create (min, max) value =
        Spec.def<(int * int * int)>
        |> Spec.add (fun (min, max, _) -> min < max, "minimum boundary should not be greater than maximum")
        |> Spec.add (fun (min, max, value) -> value > min && value < max, "value of bounded int should be between minimum and maximum boundary")
        |> Spec.createModel BoundedInt (min, max, value)
    /// Try to create a bounded integer value within the given minimum and maximum boundaries.
    static member Create (min, max, value) =
        ValidationResult (result=BoundedInt.create (min, max) value)
    static member op_Implicit (x : BoundedInt) = x.Value

/// Model representing a sequence of elements with at least a single element.
type NonEmptySeq<'T> internal (values : 'T seq) =
    member private __.values = values
    interface IEnumerable<'T> with
        member __.GetEnumerator () = values.GetEnumerator ()
        member __.GetEnumerator () = values.GetEnumerator () :> IEnumerator
    interface IEquatable<NonEmptySeq<'T>> with
        member __.Equals (other : NonEmptySeq<'T>) = other.values.SequenceEqual(values)
    override __.Equals (other : obj) =
        match other with
        | :? NonEmptySeq<'T> as other -> other.values.SequenceEqual(values)
        | _ -> false
    override __.GetHashCode () = values.GetHashCode ()
       
    /// Tries to create a non-empty sequence model of a given sequence.
    static member create (values : 'T seq) =
        Spec.def
        |> Spec.notNull "can't create non-empty sequence because 'null' was passed in"
        |> Spec.nonEmpty "can't create non-empty sequence model of given empty sequence"
        |> Spec.createModel NonEmptySeq values
    /// Tries to create an non-empty sequence model of a given sequence.
    static member Create (values : 'T seq) =
        ValidationResult (result=NonEmptySeq.create values)

    /// Projects each element of the non-empty sequence to a new form.
    member __.Select (selector : Func<_, _>) =
        if isNull selector then nullArg "selector"
        Enumerable.Select (values, selector) |> NonEmptySeq
    /// Projects each element of the non-empty sequence to a new non-empty sequence and flattens the resulting non-empty sequences into one non-empty sequence
    member __.TrySelectMany (selector : Func<_, NonEmptySeq<_>>) =
        if isNull selector then nullArg "selector"
        Enumerable.SelectMany (values, selector.Invoke >> fun xs -> xs.values) |> NonEmptySeq.Create
    /// Filters the elements in the non-empty sequence and tries to create a non-empty sequence of the resulting elements.
    member __.TryWhere (selector : Func<_, _>) =
        Enumerable.Where (values, selector) |> NonEmptySeq.Create

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
    static member createWith selectKey values =
        Spec.def
        |> Spec.nonEmpty "can't create unique sequence model of given empty sequence"
        |> Spec.unique selectKey "can't create unique sequence model since the sequence contains duplicate elements"
        |> Spec.createModel (fun x -> UniqueSeq (x, selectKey))  values
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
        UniqueSeq<'T, 'TKey>.createWith selectKey.Invoke values

/// Operations on sequence domain types.
module Seq =
    /// Tries to create a non-empty sequence model of a given sequence.
    let nonEmpty xs = NonEmptySeq.create xs

    /// Tries to create a unique sequence model of the given sequence by comparing the elements itself.
    let unique xs = UniqueSeq<_>.create xs

    /// Tries to create a unique sequence model of the given sequence, 
    /// using the given selector to select the key to compare the elements in the sequence.
    let uniqueBy selectKey xs = UniqueSeq<_, _>.createWith selectKey xs

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
    /// Tries to creates a XML encoded string from a raw string.
    static member create value =
        Spec.def<string>
        |> Spec.notNull "can't create xml string because this type doesn't allow 'null' values"
        |> Spec.forallOf (fun s -> s.ToCharArray ()) XmlConvert.IsXmlChar "can't create xml string because not all characther are considered valid XML"
        |> Spec.createModel XmlEncodedString value
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
        member __.Equals (other : XmlEncodedString) = other.ToString().Equals(value)
    override __.Equals (other) = 
        match other with 
        | :? XmlEncodedString as other -> other.ToString().Equals(value) 
        | _ -> false 
    override __.GetHashCode () = value.GetHashCode ()
    override __.ToString () = value
    static member op_Implicit (xml : XmlEncodedString) = xml.ToString ()

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
        | None -> raise (AlreadyReadException (sprintf "The value of type %s is already been read" typeof<'T>.Name))
     /// Tries to reads the read-once value and throws an `AlreadyReadException` when the value was already been read.
     member this.TryGetValue (output : outref<'T>) =
        match this.tryGetValue () with
        | Some x -> output <- x; true
        | None -> output <- Unchecked.defaultof<'T>; false

/// Representation of a value that can only be read once.
module ReadOnce =
    /// Tries to read the read-once value.
    let tryGetValue (x : ReadOnce<_>) = x.tryGetValue ()
    /// Tries to read the read-once value.
    let tryGetValueResult x error = tryGetValue x |> Result.ofOption error

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

