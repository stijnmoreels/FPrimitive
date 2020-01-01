namespace FPrimitive.Tests

open System
open System.IO

open Microsoft.FSharp.Core

open Expecto
open FsCheck
open FPrimitive

[<AutoOpen>]
module Extensions =
  let (<=>) x y = x = y

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

type Book =
  { Author : Author
    ISBN13 : ISBN13
    Pages : Int1000 } with
    static member create author isbn13 pages = result {
      let! author = Author.create author
      let! isbn13 = ISBN13.create isbn13
      let! pages = Int1000.create pages
      return { Author = author; ISBN13 = isbn13; Pages = pages } }

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

open Microsoft.FSharp.Core.Result.Operators

type Party =
  private { Ids : UniqueSeq<PartyId> } with
    static member create xs = result {
        let! uniqueIds = 
            NonEmptySeq.create xs
            >>= Result.traverseSeqMap PartyId.create
            >>= UniqueSeq<PartyId>.create
        return { Ids = uniqueIds } }

module Tests =
  let formatResult = function
    | Ok _ -> true.ToProperty()
    | Error (errs : ErrorsByTag) -> 
      false |@ (String.Join (Environment.NewLine, Map.values errs))
  let testSpec desc testCase =
    testProperty desc (testCase >> formatResult)

  [<Tests>]
  let specTests =
    testList "spec tests" [
      testSpec "add" <| fun (NegativeInt x) ->
        Spec.def<int> 
        |> Spec.add (fun x -> x < 0, "should be greater than zero")
        |> Spec.validate x

      testSpec "equal" <| fun x ->
        Spec.def 
        |> Spec.equal x "should be equal"
        |> Spec.validate x

      testProperty "not equal" <| fun x y ->
        x <> y ==> lazy
        Spec.def
        |> Spec.notEqual y "should not be equal"
        |> Spec.validate x
        |> formatResult

      testSpec "not null" <| fun (NonNull x) ->
        Spec.def<string>
        |> Spec.notNull "should not be null"
        |> Spec.validate x

      testSpec "not empty string" <| fun (NonEmptyString x) ->
        Spec.def<string>
        |> Spec.notNull "should not be null"
        |> Spec.notEmpty "should not be empty"
        |> Spec.validate x

      testSpec "not null or empty" <| fun (NonEmptyString x) ->
        specModel id x { notNullOrEmpty "should not be whitespace" }

      testProperty "not white space string" <| fun (NonEmptyString x) ->
        String.IsNullOrWhiteSpace x |> not ==> lazy
        specModel id x { notWhiteSpace "should not be whitespace" }
        |> formatResult

      testProperty "not null or white space string" <| fun (NonEmptyString x) ->
        String.IsNullOrWhiteSpace x |> not ==> lazy
        specModel id x { notNullOrWhiteSpace "should not be whitespace" }
        |> formatResult

      testSpec "length" <| fun (xs : obj []) ->
        specModel id xs { length xs.Length "should have expected length" }

      testSpec "length between" <| fun (xs : obj []) ->
        specModel id xs { lengthBetween (xs.Length - 1) (xs.Length + 1) "length should in between expected length range" }

      testSpec "length max" <| fun (xs : obj []) ->
        specModel id xs { lengthMax (xs.Length + 1) "not higher than max length" }

      testSpec "length min" <| fun (xs : obj []) ->
        specModel id xs { lengthMin (xs.Length - 1) "not lower than min length" }

      testSpec "less than" <| fun (NegativeInt x) ->
        specModel id x { lessThan 0 "should be less than zero " }
       
      testSpec "greater than" <| fun (PositiveInt x) ->
        specModel id x { greaterThan 0 "should be greater than zero" }

      testSpec "greater than or equal" <| fun (NonNegativeInt x) ->
        specModel id x { greaterThanOrEqual 0 "should be greater than or equal to zero" }

      testSpec "less than or equal" <| fun (NegativeInt x) ->
        specModel id x { lessThanOrEqual -1 "should be less than or equal to -1" }
       
      testProperty "inclusive between" <| fun (PositiveInt min) (PositiveInt max) ->
        min < max ==> lazy
        Gen.choose (min, max)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def<int>
          |> Spec.inclusiveBetween min max (sprintf "should be between %i-%i" min max)
          |> Spec.validate x
          |> formatResult

      testProperty "exclusive between" <| fun (PositiveInt min) (PositiveInt diff) ->
        diff > 1 ==> lazy
        let max = min + diff
        Gen.choose (min + 1, max - 1)
        |> Arb.fromGen
        |> Prop.forAll <| fun x ->
          Spec.def<int>
          |> Spec.exclusiveBetween min max (sprintf "should be between %i-%i" min max)
          |> Spec.validate x
          |> formatResult

      testSpec "string length" <| fun (NonEmptyString txt) ->
        Spec.def<string>
        |> Spec.lengthOf (fun str -> str.ToCharArray()) txt.Length "should check for string length"
        |> Spec.validate txt

      testSpec "regex" <| fun (PositiveInt x) ->
        specModel id (string x) { regex "^[0-9]+$" "should match regex of positive numbers" }

      testProperty "alphabetical" <| fun () ->
        ['a'..'z'] @ ['A'..'Z']
        |> Gen.elements
        |> Gen.nonEmptyListOf
        |> Arb.fromGen
        |> Prop.forAll <| fun str ->
            Spec.def
            |> Spec.alphabetical "should be in alphabet"
            |> Spec.validate (String.Join (String.Empty, str))
            |> formatResult

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
            |> formatResult

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
              |> formatResult

      testProperty "alphanumExtra sentence" <| fun () ->
        Spec.def 
        |> Spec.alphanumSpecial "sentece should work"
        |> Spec.validate "This is a s@mple of an alpha-numerical test string which can also contain 1235 and *//*^{}[/# "
        |> formatResult

      testSpec "isType" <| fun (NonNull x) ->
        Spec.def<obj>
        |> Spec.isType "should be of type"
        |> Spec.validate x

      testSpec "dependsOn" <| fun (PositiveInt x) -> specResult x {
        greaterThan 0 "should be greater than zero"
        dependsOn (spec { 
          notEqual 0 "should not be equal to zero"
          dependsOn (spec {
            greaterThanOrEqual 1 "should be greater than or equal to 1" }) }) }

      testProperty "dependsOn comes first and fails first" <| fun (NegativeInt x) -> 
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

      testSpec "seqEqual" <| fun (xs : int list) -> specResult xs { 
        seqEqual xs "sequence should be equal to other sequence" }

      testSpec "forall" <| fun (xs : PositiveInt list) -> specResult xs {
        forall (fun (PositiveInt x) ->  x > 0) "positive integer should be greater than zero" }

      testSpec "fornone" <| fun (xs : NonZeroInt list) -> specResult xs {
        fornone (fun (NonZeroInt x) -> x = 0) "non-zero integer shouldn't be zero" }

      testSpec "fornoneNull" <| fun (xs : PositiveInt list) -> specResult xs { 
        fornoneNull "positive integer should not be 'null'" }

      testProperty "structure" <| fun (PositiveInt x) (NegativeInt y) (NonZeroInt z) -> 
        specResult [x; y; z] {
          structure [
            fun x -> x > 0
            fun y -> y < 0
            fun z -> z <> 0 ] "should match structure" }
        |> formatResult

      testProperty "contains" <| fun x xs ->
        Gen.shuffle (x::xs)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs -> 
          specResult xs { contains x "should contain element" } |> formatResult

      testProperty "containsAll" <| fun xs ys ->
        Gen.shuffle (Array.append xs ys)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs ->
          specResult xs { containsAll ys "should contain all elements" } |> formatResult

      testProperty "exists" <| fun (PositiveInt x) (xs : NegativeInt list) ->
        (x :: List.map (fun (NegativeInt y) -> y) xs)
        |> Gen.shuffle
        |> Arb.fromGen
        |> Prop.forAll <| fun xs ->
          specResult xs { exists ((=) x) "should satisfy element" } |> formatResult

      testProperty "single" <| fun (xs : PositiveInt list) ->
        Gen.shuffle (0 :: List.map (fun (PositiveInt x) -> x) xs)
        |> Arb.fromGen
        |> Prop.forAll <| fun xs -> 
          specResult xs { single ((=) 0) "should only contain a single zero" } |> formatResult
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
    { Config : ReadOnce<string> }

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
    ]
  