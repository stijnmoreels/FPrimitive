namespace FPrimitive.Tests

open System
open System.IO

open Microsoft.FSharp.Core

open Expecto
open FsCheck
open FPrimitive
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module Extensions =
  let (<=>) x y = x = y

  let fromString<'a> (s:string) =
      match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
      |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
      |_ -> None

type HttpUri =
  private HttpUri of Uri with
    static member create x = specModel (Uri >> HttpUri) x {
      notNull "cannot create HTTP Uri from 'null'"
      verify (fun x -> Uri.IsWellFormedUriString (x, UriKind.Absolute)) "URI is not in a well formed format"
      verify (fun x -> x.StartsWith Uri.UriSchemeHttp || x.StartsWith Uri.UriSchemeHttps) "URI should have a 'http' or 'https' scheme" }

type Author =
  private Author of string with
    static member create x = 
      let name = @"[A-Z]{1}[a-z]*\.?"
      let pattern = sprintf "^(%s)( %s)*$" name name
      specModel Author x {
        notNullOrWhiteSpace "author name should not be blank"
        greaterThanOrEqualOf String.length 1 "author name should at least be a single character"
        matches pattern (sprintf "author name should match regular expression: %s" pattern) }

type ISBN13 =
  private ISBN13 of string with
    static member create x = 
      let pattern = "^[0-9]+$"
      let checksum (code : string) =
        let digits = code.ToCharArray() |> Seq.map (string >> int)
        let sum = 
          Seq.take 12 digits 
          |> Seq.mapi (fun i n -> if i % 2 <> 0 then n * 3 else n)
          |> Seq.sum
        let rem = sum % 10
        let checksum = if rem <> 0 then 10 - rem else rem
        checksum = Seq.last digits
      specModel ISBN13 x {
        notNullOrWhiteSpace "ISBN13 number should not be blank"
        equalOf String.length 13 "ISBN13 number should be 13 characters"
        matches pattern (sprintf "ISBN13 number should match regular expression: %s" pattern)
        startsWith "987" "ISBN13 number should start with '987'"
        verify checksum "ISBN13 checksum was invalid" } 

type Int1000 =
  private Int1000 of int with
    static member create x = specModel Int1000 x {
      inclusiveBetween 1 1000 "integer number must be between 1-1000" }

type Rating =
  | Unforgettable
  | VeryGood 
  | Okay 
  | AlmostRight
  | Garbage with
    static member create s = 
      Union.create<Rating> s

type Book =
  { Author : Author
    ISBN13 : ISBN13
    Pages : Int1000
    Rating : Rating option } with
    static member create author isbn13 pages rating = result {
      let! author = Author.create author
      let! isbn13 = ISBN13.create isbn13
      let! pages = Int1000.create pages
      let! rating = Spec.optional Option.ofObj Rating.create rating
      return { Author = author; ISBN13 = isbn13; Pages = pages; Rating = rating } }

type Point =
  { Min : int
    Max : int } with
      static member create min max = result {
        let! min = specResult min { exclusiveBetween 0 10 "minimum should be between 0-10 exlusive" }
        let! max = specResult max { exclusiveBetween 11 20 "maximum should be between 11-20 exclusive" }
        return! if min < max then Ok { Min = min; Max = max }
                else Spec.error "min" "minimum should be less than maximum" }
      
      static member create2 min max =
        let minS = spec { exclusiveBetween 0 10 "minimum should be between 0-10 exlusive" }
        let maxS = spec { exclusiveBetween 11 20 "maximum should be between 11-20 exclusive" }
        specInvariant minS maxS {
          add (fun (min, max) -> min < max, "minimum should be less than maximum") }
        |> Spec.createModel (fun (min, max) -> { Min = min; Max = max }) (min, max)

type Text = 
  private Text of string with 
    static member create (s) =
      Spec.def
      |> Spec.notNull "should not be null"
      |> Spec.notEmpty "should not be empty"
      |> Spec.notWhiteSpace "should not be blank"
      |> Spec.createModel Text s

    static member create2 (s) = specModel Text s { 
      notNull "should not be null"
      notEmpty "should not be empty" }

type String100 =
  private String100 of string with
    static member create s = specModel String100 s {
      notNullOrWhiteSpace "string100 should not be empty"
      stringLengthBetween 1 100 "string100 should have a length of max 100 characters" }

type Sex =
  | Male
  | Female
  | Between
  | Both
  | Neither
  | ItsComplicated
  | MindYourOwnBusiness
  | DontKnow
  | Other with
  static member create str =
    FSharpType.GetUnionCases typeof<Sex>
    |> Array.filter (fun case -> case.Name = str)
    |> fun xs -> if xs.Length = 1 then Some xs.[0] else None
    |> Option.map (fun case -> FSharpValue.MakeUnion (case,[||]) :?> Sex)

type Person = 
  { FirstName : Text
    LastName : Text } with
      static member create first last = result {
        let! first = Text.create first
        let! last = Text.create last
        return { FirstName = first; LastName = last } }

type PartyId = 
  private PartyId of string with
    static member create x = specModel PartyId x {
      notNullOrWhiteSpace "party ID cannot be null or blank"
      matches "^#" "party ID should start with hashtag '#'"
      cascade FirstFailure }

type Agreement =
  private Agreement of string with
    static member create x = specModel Agreement x {
      notNullOrWhiteSpace "agreement cannot be blank" }
    static member createIf x =
      Spec.def<string>
      |> Spec.notNullOrWhiteSpace "agreement cannot be blank"
      |> Spec.createModelIf Option.ofObj Agreement x

open Microsoft.FSharp.Core.Result.Operators
open Fare

type Party =
  private { Ids : PartyId uniqueSeq } with
    static member create xs = result {
        let! uniqueIds = 
            NonEmptySeq.create xs
            >>= Result.traverseSeqMap PartyId.create
            >>= UniqueSeq<PartyId>.create
        return { Ids = uniqueIds } }

module Tests =
  let toProp = function
    | Ok _ -> true.ToProperty()
    | Error (errs : ErrorsByTag) -> 
      false |@ (String.Join (Environment.NewLine, Map.values errs))
  let testSpec desc testCase =
    testProperty desc (testCase >> toProp)

  [<Tests>]
  let specTests =
    testList "spec tests" [
      testSpec "add" <| fun (NegativeInt x) ->
        Spec.def |> Spec.add (fun x -> x < 0, "should be lesser than zero")
                 |> Spec.validate x
      
      testSpec "verify" <| fun (PositiveInt x) ->
        Spec.def |> Spec.verify (fun x -> x > 0) "should be greater than zero"
                 |> Spec.validate x
      
      testSpec "equal" <| fun x ->
        Spec.def |> Spec.equal x "should be equal"
                 |> Spec.validate x
      
      testSpec "equal of" <| fun (NegativeInt x) -> specResult x {
        equalOf abs (abs x) "negative + absolute value should be greater than zero" }
      
      testProperty "not equal" <| fun x y ->
        x <> y ==> lazy
        Spec.def
        |> Spec.notEqual y "should not be equal"
        |> Spec.validate x
        |> toProp
      
      testSpec "not equal of" <| fun (NegativeInt x) -> specResult x { 
        notEqualOf abs x "absolute of negative is not negative" }
      
      testSpec "not null" <| fun (NonNull x) ->
        Spec.def<string>
        |> Spec.notNull "should not be null"
        |> Spec.validate x
      
      testSpec "not null of" <| fun (NonNull x) -> specResult (Some x) { 
        notNullOf Option.get "should not be null" }

      testSpec "not empty string" <| fun (NonEmptyString x) ->
        Spec.def |> Spec.notNull "should not be null"
                 |> Spec.notEmpty "should not be empty"
                 |> Spec.validate x
      
      testSpec "not null or empty" <| fun (NonEmptyString x) -> specResult x { 
        notNullOrEmpty "should not be whitespace" }

      testProperty "not white space string" <| fun (NonEmptyString x) ->
        String.IsNullOrWhiteSpace x |> not ==> lazy
        specResult x { notWhiteSpace "should not be whitespace" } |> toProp

      testProperty "not null or white space string" <| fun (NonEmptyString x) ->
        String.IsNullOrWhiteSpace x |> not ==> lazy
        specResult x { notNullOrWhiteSpace "should not be whitespace" } |> toProp

      testSpec "starts with" <| fun (NonEmptyString prefix, txt) -> specResult (prefix + txt) {
        startsWith prefix "should start with prefix" }
      
      testSpec "starts with of" <| fun (NonEmptyString prefix, txt) -> specResult (Some <| prefix + txt) {
        startsWithOf Option.get prefix "should start with prefix" }
      
      testSpec "ends with" <| fun (txt, NonEmptyString trailer) -> specResult (txt + trailer) {
        endsWith trailer "should end with trailer" }
      
      testSpec "ends with of" <| fun (txt, NonEmptyString trailer) -> specResult (Some <| txt + trailer) {
        endsWithOf Option.get trailer "should end with trailer" }

      testSpec "seq equal" <| fun (xs : int list) -> specResult xs { 
          seqEqual xs "sequence should be equal to other sequence" }

      testSpec "non empty" <| fun x ->
        Spec.def |> Spec.nonEmpty "should not be empty"
                 |> Spec.validate [x]
      
      testSpec "non empty of" <| fun x ->
        Spec.def |> Spec.nonEmptyOf Option.get "should not be empty"
                 |> Spec.validate (Some [x])

      testSpec "forall" <| fun (xs : PositiveInt list) -> specResult xs {
        forall (fun (PositiveInt x) ->  x > 0) "positive integer should be greater than zero" }

      testSpec "fornone" <| fun (xs : NonZeroInt list) -> specResult xs {
        fornone (fun (NonZeroInt x) -> x = 0) "non-zero integer shouldn't be zero" }

      testSpec "fornone null" <| fun (xs : PositiveInt list) -> specResult xs { 
        fornoneNull "positive integer should not be 'null'" }

      testProperty "contains" <| fun x xs ->
        Gen.shuffle (x::xs)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs -> 
          specResult xs { contains x "should contain element" } |> toProp

      testProperty "contains all" <| fun xs ys ->
        Gen.shuffle (Array.append xs ys)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs ->
          specResult xs { containsAll ys "should contain all elements" } |> toProp

      testProperty "string contains" <| fun (NonEmptyString x) (NonEmptyString y) ->
        specResult (x + y) { stringContains x "should contain substring" } |> toProp

      testProperty "string not contains" <| fun (NonEmptyString x) (NonEmptyString y) ->
        (not <| x.Contains y) ==> lazy
        specResult x { stringNotContains y "should not contain substring" } |> toProp

      testProperty "string contains all" <| fun (NonEmptyString x) (xs : NonEmptyArray<NonEmptyString>) ->
        let xs = xs.Get |> Array.map (fun x -> x.Get) |> List.ofArray
        Gen.shuffle (x::xs)
        |> Gen.map (fun ys -> String.Join ("", ys))
        |> Arb.fromGen
        |> Prop.forAll <| fun str ->
          specResult str { stringContainsAll xs "should contain all substrings" } 
          |> toProp

      testProperty "exists" <| fun (PositiveInt x) (xs : NegativeInt list) ->
        (x :: List.map (fun (NegativeInt y) -> y) xs)
        |> Gen.shuffle
        |> Arb.fromGen
        |> Prop.forAll <| fun xs ->
          specResult xs { exists ((=) x) "should satisfy element" } |> toProp

      testProperty "not exists" <| fun (xs : PositiveInt list) ->
        Spec.def
        |> Spec.notExists ((=) 0) "should not contain zero"
        |> Spec.validate (xs |> List.map (fun x -> x.Get))
        |> toProp

      testProperty "single" <| fun (xs : PositiveInt list) ->
        Gen.shuffle (0 :: List.map (fun (PositiveInt x) -> x) xs)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs -> 
          specResult xs { single ((=) 0) "should only contain a single zero" } 
          |> toProp

      testProperty "unique" <| fun x y z ->
        (x <> y && x <> z && y <> z) ==> lazy
        Spec.def
        |> Spec.unique ((+) 1) "should have unique elements"
        |> Spec.validate [x; y; z]
        |> toProp

      testProperty "unique items" <| fun x y z ->
        (x <> y && x <> z && y <> z) ==> lazy
        Spec.def
        |> Spec.uniqueItems "should have unique elements"
        |> Spec.validate [x; y; z]
        |> toProp

      testProperty "structure" <| fun (PositiveInt x) (NegativeInt y) (NonZeroInt z) ->
          specResult [x; y; z] {
            structure [
              fun x -> x > 0
              fun y -> y < 0
              fun z -> z <> 0 ] "should match structure" }
          |> toProp

      testSpec "length" <| fun (xs : obj []) -> specResult xs { 
        length xs.Length "should have expected length" }
      
      testSpec "length between" <| fun (xs : obj []) -> specResult xs { 
        lengthBetween (xs.Length - 1) (xs.Length + 1) "length should in between expected length range" }
      
      testSpec "length max" <| fun (xs : obj []) -> specResult xs { 
        lengthMax (xs.Length + 1) "not higher than max length" }
      
      testSpec "length min" <| fun (xs : obj []) -> specResult xs { 
        lengthMin (xs.Length - 1) "not lower than min length" }

      testSpec "less than" <| fun (NegativeInt x) -> specResult x { 
        lessThan 0 "should be less than zero " }
      
      testSpec "greater than" <| fun (PositiveInt x) -> specResult x { 
        greaterThan 0 "should be greater than zero" }
      
      testSpec "greater than or equal" <| fun (NonNegativeInt x) -> specResult x { 
        greaterThanOrEqual 0 "should be greater than or equal to zero" }
      
      testSpec "less than or equal" <| fun (NegativeInt x) -> specResult x { 
        lessThanOrEqual -1 "should be less than or equal to -1" }

      testSpec "string length" <| fun (NonEmptyString txt) ->
        Spec.def<string>
        |> Spec.stringLength txt.Length "should have string length of %i"
        |> Spec.validate txt
      testProperty "string min length" <| fun (NonEmptyString txt) ->
        Gen.choose (1, txt.Length)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def<string>
          |> Spec.stringLengthMin x (sprintf "should have min length of %i" x)
          |> Spec.validate txt
          |> toProp
      testProperty "string max length" <| fun (NonEmptyString txt) ->
        Gen.choose (txt.Length, Int32.MaxValue)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def 
          |> Spec.stringLengthMax x (sprintf "should have max length of %i" x)
          |> Spec.validate txt 
          |> toProp
      testProperty "string between length" <| fun (NonEmptyString txt) ->
        gen {
          let! min = Gen.choose (1, txt.Length)
          let! max = Gen.choose (min, Int32.MaxValue)
          return min, max }
        |> Arb.fromGen
        |> Prop.forAll <| fun (min, max) ->
          Spec.def
          |> Spec.stringLengthBetween min max (sprintf "should be between %i and %i" min max)
          |> Spec.validate txt
          |> toProp

      testProperty "inclusive between" <| fun (PositiveInt min) (PositiveInt max) ->
        min < max ==> lazy
        Gen.choose (min, max)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def<int>
          |> Spec.inclusiveBetween min max (sprintf "should be between %i-%i" min max)
          |> Spec.validate x
          |> toProp

      testProperty "exclusive between" <| fun (PositiveInt min) (PositiveInt diff) ->
        diff > 1 ==> lazy
        let max = min + diff
        Gen.choose (min + 1, max - 1)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def<int>
          |> Spec.exclusiveBetween min max (sprintf "should be between %i-%i" min max)
          |> Spec.validate x
          |> toProp

      testSpec "matches" <| fun (PositiveInt x) ->
        specModel id (string x) { matches "^[0-9]+$" "should match regex of positive numbers" }

      testProperty "alphabetical" <| fun () ->
        ['a'..'z'] @ ['A'..'Z']
        |> Gen.elements
        |> Gen.nonEmptyListOf
        |> Arb.fromGen
        |> Prop.forAll <| fun str ->
            Spec.def
            |> Spec.alphabetical "should be in alphabet"
            |> Spec.validate (String.Join (String.Empty, str))
            |> toProp

      testProperty "non-alphabetical" <| fun (NonNull (str : string)) ->
        let alphabet = ['a'..'z'] @ ['A'..'Z']
        str.ToCharArray() 
        |> Array.exists (fun c -> List.contains c alphabet |> not) 
        ==> lazy
        Spec.def 
        |> Spec.alphabetical "should be in alphabet"
        |> (not << Spec.isSatisfiedBy str)
        
      testProperty "alphanum" <| fun (PositiveInt x) ->
        List.map string ['a'..'z'] 
        @ List.map string  ['A'..'Z'] 
        @ List.map string [ x ]
        |> Gen.shuffle
        |> Arb.fromGen
        |> Prop.forAll <| fun str -> 
            Spec.def 
            |> Spec.alphanum "should be alphanumerical"
            |> Spec.validate (String.Join (String.Empty, str))
            |> toProp

      testProperty "alphanumExtra" <| fun (PositiveInt x) ->
          List.map string ['a'..'z'] 
          @ List.map string  ['A'..'Z'] 
          @ List.map string [ x ]
          @ [ "@"; "%"; "(" ]
          |> Gen.shuffle
          |> Arb.fromGen
          |> Prop.forAll <| fun str -> 
              Spec.def 
              |> Spec.alphanumSpecial "should be alphanumerical"
              |> Spec.validate (String.Join (String.Empty, str))
              |> toProp

      testProperty "alphanumExtra sentence" <| fun () ->
        Spec.def 
        |> Spec.alphanumSpecial "sentence should work"
        |> Spec.validate "This is a s@mple of an alpha-numerical test string which can also contain 1235 and *//*^{}[/# "
        |> toProp

      testSpec "is type T" <| fun (NonNull x) ->
        Spec.def<obj>
        |> Spec.isTypeT "should be of type"
        |> Spec.validate x

      testSpec "is type" <| fun (x : NegativeInt) ->
        Spec.def<obj>
        |> Spec.isType typeof<NegativeInt> "should be of type 'negative int'"
        |> Spec.validate (x :> obj)

      testProperty "cascade" <| fun (NegativeInt x) ->
        Spec.tag "int"
        |> Spec.greaterThan 0 "should be greater than zero"
        |> Spec.equal 0 "should be equal to zero"
        |> Spec.cascade Continue
        |> Spec.validate x
        |> Result.mapError (fun err -> 
            Map.count err = 1 
            && Map.tryFind "int" err 
            |> Option.map (fun xs -> List.length xs = 2)
            |> Option.defaultValue false)
        |> Result.either (fun _ -> false) id

      testProperty "validate" <| fun x ->
        Spec.def
        |> Spec.lessThan 0 "int should be less than zero"
        |> Spec.validate x
        |> fun r -> Result.isOk r <=> (x < 0)

      testProperty "validate untrust" <| fun x ->
        Spec.def
        |> Spec.greaterThan 0 "int should be greater than zero"
        |> Spec.validateUntrust (Untrust x)
        |> fun r -> Result.isOk r <=> (x > 0)

      testProperty "validate option" <| fun x ->
        Spec.def<string>
        |> Spec.notNullOrWhiteSpace "string should not be blank"
        |> Spec.validateOption x
        |> fun r -> Option.isNone r <=> String.IsNullOrWhiteSpace x

      testProperty "create model" <| fun (NonNull x) ->
        Spec.def
        |> Spec.notNull "should not be null"
        |> Spec.createModel NonNull x
        |> (=) (Ok (NonNull x))
      
      testProperty "create model with" <| fun x ->
        Gen.elements [ Ok; fun _ -> Spec.error "tag" "error" ]
        |> Arb.fromGen
        |> Prop.forAll <| fun f ->
            Spec.def
            |> Spec.equal x "should be equal to instance"
            |> Spec.createModelWith f x
            |> fun r -> Result.isOk r <=> Result.isOk (f x)
      
      testProperty "optional" <| fun filter x ->
          let (Ok value) = Spec.optional filter Ok x
          value <=> filter x

      testProperty "create model if" <| fun (f : PositiveInt -> NegativeInt option) (x : PositiveInt) ->
          let r =
            Spec.def
            |> Spec.lessThanOf (fun (NegativeInt x) -> x) 0 "should be less than zero"
            |> Spec.createModelIf f id x 
          r <=> Ok (f x) |@ (sprintf "left: %A <=> right: %A" r (f x))

      testProperty "create model with if" <| fun f x ->
          Spec.def
          |> Spec.notNullOf (fun (NonNull y) -> y) "should not be null"
          |> Spec.createModelWithIf f Ok x <=> Ok (f x)

      testProperty "validate if" <| fun x -> 
          let result = specResultIf Option.ofObj x { equal x "should be equal to input" }
          (result = Ok None <=> (x = null) |@ (sprintf "left: %A <=> right: %A" result x))
          .|. (result = Ok (Some x) <=> (x <> null) |@ (sprintf "left: %A <=> right: %A" result x))

      testProperty "is satisfied by" <| fun f x ->
        Spec.def 
        |> Spec.verify f "can't predict outcome"
        |> Spec.isSatisfiedBy x <=> f x

      testSpec "depends on" <| fun (PositiveInt x) -> specResult x {
        greaterThan 0 "should be greater than zero"
        dependsOn (spec { 
          notEqual 0 "should not be equal to zero"
          dependsOn (spec {
            greaterThanOrEqual 1 "should be greater than or equal to 1" }) }) }

      testProperty "depends on comes first and fails first" <| fun (NegativeInt x) -> 
        let r = specResult x {
          tag "base"
          greaterThan 0 "should be greater than zero"
          dependsOn (spec { 
            tag "child"
            equal 0 "@int should be equal to zero" }) }
        r = Spec.error "child.int" "@int should be equal to zero"

      testProperty "valid type for quick validation" <| fun x isValid ->
        let r = Valid<_>.createWith x (fun _ -> isValid) "should be valid when predicate holds"
        Expect.isTrue (Result.isOk r <=> isValid) "should be valid when predicate holds" 

      testProperty "merge" <| fun (NegativeInt x) ->
        let lessThan1 = spec { lessThan 1 "should be less than 1" }
        let notEqual0 = spec { notEqual 0 "should not be equal to zero" }
        Spec.merge lessThan1 notEqual0
        |> Spec.validate x |> toProp

      testProperty "comap" <| fun x ->
        Spec.def
        |> Spec.verify Option.isSome "should be 'Some'"
        |> Spec.comap Some
        |> Spec.validate x |> toProp

      testProperty "map" <| fun (NonEmptyArray xs) ->
        let mutable logs = ""
        let r =
          Spec.def
          |> Spec.nonEmpty "non-empty array should not be empty"
          |> Spec.map (fun f -> fun x -> 
            logs <- logs + "start>"
            let r = f x
            logs <- logs + "<end"
            r)
          |> Spec.validate xs
        Expect.equal logs "start><end" "map function should add logs"
        Expect.isOk r "mapped req should still run"

      testProperty "preval-ok" <| fun (x : int) ->
        let r =
          Spec.def
          |> Spec.lessThan 0 "negative int should not be less than zero"
          |> Spec.preval Ok
          |> Spec.validate x
        Result.isOk r <=> (x < 0)

      testProperty "preval-error" <| fun (x : int) ->
        let r =
          Spec.def
          |> Spec.lessThan 0 "negative int should not be less than zero"
          |> Spec.preval (fun _ -> Error "sabotage error")
          |> Spec.validate x
        Result.isError r |@ (sprintf "%A" r)

      testProperty "list" <| fun (xs : NegativeInt list) ->
        Spec.def
        |> Spec.lessThanOf (fun (NegativeInt y) -> y) 0 "item should be less than zero"
        |> Spec.list
        |> Spec.validate xs |> toProp

      testProperty "array" <| fun (xs : NonEmptyString array) ->
        Spec.def
        |> Spec.notEmptyOf (fun (NonEmptyString y) -> y) "item should not be empty"
        |> Spec.array
        |> Spec.validate xs |> toProp

      testProperty "seq" <| fun (xs : NonNull<_> array) ->
        Spec.def 
        |> Spec.notNullOf (fun (NonNull y) -> y) "item should not be null"
        |> Spec.seq 
        |> Spec.validate (Seq.ofArray xs) |> toProp

      testProperty "invariant" <| fun (NonEmptyString name) ->
        Gen.choose (1, 10)
        |> Gen.map (fun i -> i, sprintf "#%i %s" i name)
        |> Arb.fromGen
        |> Prop.forAll <| fun t ->
             let specTop10 = spec { inclusiveBetween 1 10 "should be between 1-1(inclusive)" }
             let specFinalist = spec { notNullOrWhiteSpace "should not be blank" }
             let s = specInvariant specTop10 specFinalist { 
                 add (fun (top10, finalist) -> finalist.StartsWith (sprintf "#%i"top10), sprintf "should start with %i" top10) }
             Spec.validate t s |> toProp

      testProperty "invariant 3" <| fun (NegativeInt x) (PositiveInt y) (NonEmptyString z) ->
        let x_spec = spec { lessThan 0 "negative int should be greater than zero" }
        let y_spec = spec { greaterThan 0 "positive int should be greater than zero" }
        let z_spec = spec { notEmpty "non-empty string type should not be empty" }
        Spec.invariant3 x_spec y_spec z_spec
        |> Spec.add (fun (x, y, z) -> x < y && z <> null, "negative int should be less than positive int & non-empty string should not be 'null'")
        |> Spec.validate (x, y , z) |> toProp
    ]

  [<Tests>]
  let orderTests =
    testList "spec order" [
      testCase "order" <| fun () ->
          let xs = [ null; ""; " "; "\n"]
          let asserter x =
            Spec.def |> Spec.verify (fun (x : string) -> x.Length > 10) "should be greater than 10 characters" 
                     |> Spec.notNull "should not be null"
                     |> Spec.notEmpty "should not be empty"
                     |> Spec.notWhiteSpace "should not be whitespace"
                     |> Spec.isSatisfiedBy x
          Expect.all xs (not << asserter) "should validate in order" 
    ]

  [<Tests>]
  let accessTests =
    testList "access tests" [
      testCase "read once" <| fun _ ->
       let acc = Access.func (fun () -> 1) |> Access.once
       
       Expect.equal (Access.evalUnit acc) (Ok 1) "read once access should evaluate once"
       Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"
       Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"

      testCase "read twice" <| fun _ ->
        let acc = Access.func (fun () -> true) |> Access.twice

        Expect.equal (Access.evalUnit acc) (Ok true) "read twice should evaluate once"
        Expect.equal (Access.evalUnit acc) (Ok true) "read twice should evaluate twice"
        Expect.notEqual (Access.evalUnit acc) (Ok true) "read twice should dissallow after 2 reads"
        Expect.notEqual (Access.evalUnit acc) (Ok true) "read twice should dissallow after 2 reads"

      testCaseAsync "revoke after 1 second" <| async {
        let acc = access {
          funcResult (fun () -> Ok "get me")
          filter ((=) ()) "should be '()'"
          revokedAfter _1s }

        Expect.equal (Access.eval () acc) (Ok "get me") "revokable access should evaluate before revocation expiration"
        do! Async.Sleep 1200
        Expect.notEqual (Access.eval () acc) (Ok "get me") "revokable access should be revoked after revocation expiration" }
      
      testCase "revoke when observable emits" <| fun _ ->
        let mutable observer = null
        let obs = 
          { new IObservable<int> with
              member __.Subscribe (obs) =
                observer <- obs
                { new IDisposable with member __.Dispose () = () } }
        let acc = access {
          funcOption (fun x -> if x > 0 then Some x else None) "should not be 'None'"
          filter (fun x -> x < 10) "should be less than 10"
          revokedWhen obs }
        
        Expect.equal (Access.eval 9 acc) (Ok 9) "revokable access with observable should allow before revocation expiration"
        observer.OnNext (1)
        Expect.notEqual (Access.eval 4 acc) (Ok 9) "revokable access with observable should be revoked after observable emits"

      testCase "files in sub-directory" <| fun _ ->
        let acc = Access.onlyFilesFromDirs [ DirectoryInfo "C:/temp" ]
        let file = FileInfo "C:/temp/sub/test.txt"
        Expect.notEqual (Access.eval file acc) (Ok file) "only directory itself is allowed to access"

      testCase "relative files in relative directory" <| fun _ ->
        let acc = Access.onlyFilesFrom (DirectoryInfo "/temp")
        let file = FileInfo "/temp/test.jpg"
        Expect.equal (Access.eval file acc) (Ok file) "relative paths should result in same access evaluation"

      testCase "file extensions" <| fun _ ->
        let acc = access {
          onlyFilesFrom (DirectoryInfo "/bin")
          fileExtension ".txt" }
        let file = FileInfo "/bin/image.jpg"
        Expect.notEqual (Access.eval file acc) (Ok file) ".jpg files are disallowed"
        let file = FileInfo "/bin/doc.txt"
        Expect.equal (Access.eval file acc) (Ok file) ".txt files are allowed"

      testCase "uri is baseOf" <| fun _ ->
        let acc = Access.onlyUriFromBase (Uri "http://localhost:1234/path")
        let uri = Uri "http://localhost:1234/path/to/something"
        Expect.equal (Access.eval uri acc) (Ok uri) "URI with extra path on base is allowed"

      testCase "uri is not baseOf" <| fun _ ->
        let acc = Access.onlyUriFromBase (Uri "http://www.website.com/something")
        let uri = Uri "http://www.otherwebsite.com/other/"
        Expect.notEqual (Access.eval uri acc) (Ok uri) "diff URI with other base is not allowed"

      testCase "validate with specification" <| fun _ ->
        let spec = 
          Spec.def<string> 
          |> Spec.notEmpty "should not be empty string"
        
        let acc =
          Access.func (fun x -> Some x)
          |> Access.satisfy spec
          |> Access.revokable
          |> Access.times 3

        Expect.notEqual (Access.eval "" acc) (Ok (Some "")) "1st evaluation: revokable access with specification should not be satisfied"
        Expect.equal (Access.eval "not empty" acc) (Ok (Some "not empty")) "2nd evaluation: revokable access with specification should be satisfied"
        Access.revoke acc
        Expect.notEqual (Access.eval "also not empty" acc) (Ok (Some "also not empty")) "3nd evaluation: access with specification should be revoked"
      
      testCase "during correct hours" <| fun _ ->
        let now = DateTimeOffset.Now
        let acc = access {
          funcResult (fun x -> Ok x)
          duringHours (now.AddHours -2.).Hour (now.AddHours +2.).Hour }
        Expect.equal (Access.eval 0 acc) (Ok 0) "should evaluate during expected time range"

      testCase "during incorrect dates" <| fun _ ->
        let now = DateTimeOffset.Now
        let acc = access { 
          funcOption (fun _ -> None) "should not be Some"
          duringDates (now.AddYears -10) (now.AddYears -5) }
        Expect.notEqual (Access.evalUnit acc) (Ok None) "should block evaluation during expected time range"
    ]

  type Critical =
    { Config : string readOnce }

  type Temp =
    { Value : Disposable<string> }

  [<Tests>]
  let typeTests =
    testList "types" [
      testCase "read once" <| fun _ ->
        let cri = { Config = ReadOnce "secret" }
        let value = ReadOnce.tryGetValue cri.Config
        Expect.equal value (Some "secret") "read once type should evaluate once"
        let value = ReadOnce.tryGetValue cri.Config
        Expect.equal value None "read once type should not evaluate twice"
      testCase "read once throw" <| fun _ ->
        let cri = { Config = ReadOnce "connection string" }
        Expect.equal (cri.Config.GetValue ()) "connection string" "read once type should evaluate once"
        Expect.throwsT<AlreadyReadException> (cri.Config.GetValue >> ignore) "read once type should throw after single read"
      testCase "read once func" <| fun () ->
        let cri = ReadOnce (fun () -> "secret")
        let value = ReadOnce.tryEvalFunc cri ()
        Expect.equal value (Some "secret") "read once type should evaluate once"
        let value = ReadOnce.tryEvalFunc cri ()
        Expect.equal value None "read once type should not evaluate twice"
      testCaseAsync "read once func async" <| async {
        let cri = ReadOnce (fun () -> async.Return 1)
        let! value = ReadOnce.tryEvalFuncAsync cri ()
        Expect.equal value (Some 1) "read once type should evaluate once"
        let! value = ReadOnce.tryEvalFuncAsync cri ()
        Expect.equal value None "read once type should not evaluate twice" }
      testCase "write once" <| fun _ ->
        let printOnce = WriteOnce.create <| fun name -> printfn "hello %s!" name
        let value = WriteOnce.trySetValue "Elsa" printOnce
        Expect.isTrue value "write once should evaluate once"
        let value = WriteOnce.trySetValue "Hanna" printOnce
        Expect.isFalse value "write once should not evaluate twice"

      testCase "write once throw" <| fun _ ->
        let printOnce = WriteOnce.create <| fun name -> printfn "hello %s!" name
        let value = WriteOnce.trySetValue "Elsa" printOnce
        Expect.isTrue value "write once should evaluate once"
        Expect.throwsT<AlreadyWrittenException> (fun () -> printOnce.SetValue "Hanna") "write once type should throw after single write"

      testCase "disposable" <| fun _ ->
        let temp = { Value = Disposable.create "something to remove" }
        using temp.Value (fun v ->
          let value = Disposable.tryGetValue v
          Expect.equal value (Some "something to remove") "disposable type should evaluate when not disposed")
        
        let value = Disposable.tryGetValue temp.Value
        Expect.equal value None "disposable type should not evaluate after disposal"

      testCase "untrust validate bool" <| fun _ ->
        Expect.equal (Untrust.getWith (fun x -> x > 0) (Untrust 10)) (Some 10) "should get value when the predicate holds"
        Expect.equal (Untrust.getWith (fun x -> x <> "") (Untrust String.Empty)) None "should not get value when predicate fails"

      testCase "untrust validate option" <| fun _ ->
        let actual = Untrust.getWithOption (fun x -> if x > 0 then Some (PositiveInt x) else None) (Untrust 5)
        Expect.equal actual (Some (PositiveInt 5)) "should get value when the predicate holds"
        let actual = Untrust.getWithOption (fun x -> if x <> "" then Some (NonEmptyString x) else None) (Untrust String.Empty)
        Expect.equal actual None "should not get value when the predicate holds"

      testCase "untrust validate result" <| fun _ ->
        let actual = Untrust.getWithResult (fun x -> if x <> 0 then Ok (NonZeroInt x) else Spec.error "int" "should be not be zero") (Untrust 1)
        Expect.equal actual (Ok (NonZeroInt 1)) "should get value when predicate holds"
        let actual = Untrust.getWithResult (fun x -> if x <> null then Ok (NonNull x) else Spec.error "int" "should not be 'null'") (Untrust null)
        Expect.equal actual (Spec.error "int" "should not be 'null'") "should not get value when predicate fails"

      testProperty "non-empty sequences are the same" <| fun (objs : NonEmptyArray<obj>) ->
        let xs = Seq.toNonEmpty objs.Get
        let ys = Seq.toNonEmpty objs.Get
        Expect.equal xs ys "should be the same non-empty seq"

      testCase "non-emtpy sequence with `None` is still correct" <| fun () ->
        let xs = Seq.toNonEmpty [ None, Some 1, None, None ]
        Expect.isOk xs "should accept `None` as a valid entry"

      testProperty "unique sequences on elements are the same" <| fun (objs : NonEmptyArray<obj>) ->
        let objs = Seq.distinct objs.Get
        let xs = Seq.toUnique objs
        let ys = Seq.toUnique objs
        Expect.equal xs ys "should be the same unique seq"

      testProperty "unique sequences on element keys are the same" <| fun (objs : NonEmptyArray<obj>) ->
        let objs = Seq.distinctBy (fun x -> x.ToString ()) objs.Get
        let xs = Seq.toUniqueBy (fun x -> x.ToString ()) objs
        let ys = Seq.toUniqueBy (fun x -> x.ToString ()) objs
        Expect.equal xs ys "should be the same unique seq"

      testCase "unique sequence with `None` is still correct" <| fun () ->
        let xs = Seq.toUnique [ None, Some 1, Some 2 ]
        Expect.isOk xs "should accept `None` as a valid entry"


      testProperty "tags are made from strings starting with '@'" <| fun (PositiveInt x) ->
        Gen.subListOf [ yield! ['a'..'z']; yield! ['A'..'Z'] ]
        |> Gen.filter (fun xs -> xs.Length > 0)
        |> Gen.map (fun xs -> String.Join ("", xs))
        |> Gen.three
        |> Arb.fromGen
        |> Prop.forAll <| fun (name, tag1, tag2) ->
            let r = specResult x { 
              tag name
              cascade Continue
              startsWithOf string "-" (sprintf "the @%s should start with a '-'" tag1)
              endsWithOf string "." (sprintf "ths @%s should end with a '.'" tag1)
              lessThanOf (string >> String.length) 0 (sprintf "the length of the @%s should be -1" tag2)
              inclusiveBetween -1 -100 "should be between -1 <-> -100" }
            
            Expect.isError r "validation should fail"
            let errMap = Result.either (fun _ -> Map.empty) id r
            Expect.hasLength errMap 3 "valiation error map should have 3 entries"
            Expect.equal 
              (Map.tryFind (sprintf "%s.%s" name tag1) errMap) 
              (Some [ sprintf "the @%s should start with a '-'" tag1
                      sprintf "ths @%s should end with a '.'" tag1 ])
              "validation error map should have 2 entries for tag1 with the same 'name.tag' key"
            Expect.equal
              (Map.tryFind (sprintf "%s.%s" name tag2) errMap)
              (Some [ sprintf "the length of the @%s should be -1" tag2 ])
              "validation error map should have 1 entry with for tag2"
            Expect.equal 
              (Map.tryFind name errMap) 
              (Some [ "should be between -1 <-> -100" ]) 
              "validation error map should have 1 entry with the 'name' as key"
    ]

  [<Tests>]
  let sanitize_tests =
    testList "sanitize" [
      testProperty "empty when null" <| fun x ->
        let actual = Sanitize.empty_when_null x
        Expect.isNotNull actual "can never be 'null'"
      testProperty "white regex+match" <| fun (NonNull pre_noise) (PositiveInt x) (NonNull post_noise) ->
        (pre_noise <> string x && post_noise <> string x
        && not <| pre_noise.Contains (string x)
        && not <| post_noise.Contains (string x)) ==> lazy
        let input = sprintf "%s%i%s" pre_noise x post_noise
        let actual = Sanitize.whitematch (string x) input
        Expect.equal actual (string x) "should only allow matches"
      testProperty "white list" <| fun (NonNull pre_noise) x (NonNull post_noise) ->
        (pre_noise <> string x && post_noise <> string x
         && not <| pre_noise.Contains (string x)
         && not <| post_noise.Contains (string x)) ==> lazy
        let input = sprintf "%s%i%s" pre_noise x post_noise
        let actual = Sanitize.whitelist [string x] input
        Expect.equal actual (string x) "should only allow in list"
      testProperty "black regex+match" <| fun (PositiveInt pre_noise) (NonEmptyString x) (PositiveInt post_noise) ->
        (not <| x.Contains (string pre_noise) 
        && not <| x.Contains (string post_noise)
        && not <| (string pre_noise).Contains (string post_noise)
        && not <| (string post_noise).Contains (string pre_noise)) ==> lazy
        let input = sprintf "%i%s%i" pre_noise x post_noise
        let actual = Sanitize.blackmatch (sprintf "(%i|%i)" pre_noise post_noise) input
        Expect.equal actual x "should remove noise via match"
      testProperty "black list" <| fun (PositiveInt pre_noise) (NonEmptyString x) (PositiveInt post_noise) ->
        (not <| x.Contains (string pre_noise) 
         && not <| x.Contains (string post_noise)
         && not <| (string pre_noise).Contains (string post_noise)
         && not <| (string post_noise).Contains (string pre_noise)) ==> lazy
        let input = sprintf "%i_%s_%i" pre_noise x post_noise
        let actual = Sanitize.blacklist [string pre_noise; string post_noise] input
        Expect.equal actual (sprintf "_%s_" x) "should remove noise via list"
      testProperty "replace(s)" <| fun (NonEmptyString target) (NonEmptyString value) (NonEmptyString replacement) ->
        (target <> value && target <> replacement
         && not <| value.Contains target
         && not <| target.Contains value ) ==> lazy
        let input = sprintf "%s%s" target value
        let actual = Sanitize.replace value replacement input
        Expect.equal actual (sprintf "%s%s" target replacement) "should replace value with replacement"
      testProperty "max" <| fun (NonEmptyString x) (PositiveInt length) ->
          (length <= x.Length) ==> lazy
          let actual = Sanitize.max length x
          Expect.isLessThanOrEqual actual.Length length "stripping max length should not less or equal to original length"
      testProperty "ascii" <| fun (PositiveInt x) ->
        let input = sprintf "%i👍" x
        let actual = Sanitize.ascii input
        Expect.equal actual (string x) "should only get ASCII characters"
      testProperty "all ascii chars allowed" <| fun () ->
        let input = Xeger("[\x00-\x7F]+").Generate()
        let actual = Sanitize.ascii input
        Expect.equal actual input (sprintf "should allow all ASCII characters, left: %A, right: %A" actual input)
    ]
  