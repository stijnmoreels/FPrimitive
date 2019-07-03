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

type Point =
  { Min : int
    Max : int } with
      static member create min max = result {
        let! min = specResult min { exclusiveBetween 0 10 "minimum should be between 0-10 exlusive" }
        let! max = specResult max { exclusiveBetween 11 20 "maximum should be between 11-20 exclusive" }
        return { Min = min; Max = max } }

type Text = 
  private Text of string with 
    static member create (s) =
      Spec.def<string>
      |> Spec.notNull "should not be null"
      |> Spec.notEmpty "should not be empty"
      |> Spec.notWhiteSpace "should not be blank"
      |> Spec.createModel Text s

    static member creaet (s) =
      specModel Text s { 
        notNull "should not be null"
        notEmpty "should not be empty" }

type Person = 
  { FirstName : Text
    LastName : Text } with
      static member create first last = result {
        let! first = Text.create first
        let! last = Text.create last
        return { FirstName = first; LastName = last } }

module Tests =
  let formatResult = function
    | Ok _ -> true.ToProperty()
    | Error (errs : string list) -> 
      false |@ (String.Join (Environment.NewLine, errs))
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

      testProperty "notEqual" <| fun x y ->
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

      testSpec "forall" <| fun (xs : NonEmptyArray<NegativeInt>) ->
        specModel id xs.Get { 
          forall (fun (NegativeInt x) -> x < 0) "each element should be greater than zero" }

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

      testSpec "isType" <| fun (NonNull x) ->
        Spec.def<obj>
        |> Spec.isType<obj> "should be of type"
        |> Spec.validate x
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
        let actual = Untrust.getWithResult (fun x -> if x <> 0 then Ok (NonZeroInt x) else Error ["should be not be zero"]) (Untrust 1)
        Expect.equal actual (Ok (NonZeroInt 1)) "should get value when predicate holds"
        let actual = Untrust.getWithResult (fun x -> if x <> null then Ok (NonNull x) else Error ["should not be 'null'"]) (Untrust null)
        Expect.equal actual (Error ["should not be 'null'"]) "should not get value when predicate fails"
    ]