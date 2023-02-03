module AccessTests

open System
open System.IO
open FsCheck
open Expecto
open FPrimitive
open FPrimitive.Tests

[<Tests>]
let accessTests =
  testList "access tests" [
    testCase "read once" <| fun _ ->
     let acc = access { func (fun () -> 1); once }
     
     Expect.equal (Access.evalUnit acc) (Ok 1) "read once access should evaluate once"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"

    testCase "read once Func 1" <| fun _ ->
     let acc = Access.Function1 (Func<_> (fun () -> 1)) |> Access.once
     
     Expect.equal (Access.evalUnit acc) (Ok 1) "read once access should evaluate once"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"

    testCase "read once Func 2" <| fun _ ->
     let acc = Access.Function2 (Func<_, _> (fun () -> 1)) |> Access.once
     
     Expect.equal (Access.evalUnit acc) (Ok 1) "read once access should evaluate once"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"
     Expect.notEqual (Access.evalUnit acc) (Ok 1) "read once should dissallow access after read"

    testCase "read once Action" <| fun _ ->
     let output = ref 0
     let acc = Access.Action (Action< _> (fun () -> output.Value <- 1)) |> Access.once
     
     Expect.equal (Access.evalUnit acc) (Ok ()) "read once access should evaluate once"
     Expect.equal 1 output.Value "read once access should evaluate once"
     Expect.notEqual (Access.evalUnit acc) (Ok ()) "read once should dissallow access after read"
     Expect.notEqual (Access.evalUnit acc) (Ok ()) "read once should dissallow access after read"

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

    testProperty "satisfy" <| fun f g msg x ->
      let a = access { func f; satisfy (spec { verify g msg}) }
      Access.eval x a |> Result.isOk = g x

    testAsync "eval async id rule" {
      let x = access { func (fun () -> async { return 0 }) }
      let! rUnit = Access.evalUnitAsync x
      let! rValue = Access.evalAsync () x
      Expect.equal rUnit (Ok 0) "accessing eval unit async ignore function should succeed"
      Expect.equal rValue (Ok 0) "accessing eval async ignore function should succeed" }

    testAsync "eval result async id rule" {
      let x = access { func (fun () -> async { return Ok 0 }) }
      let! rUnit = Access.evalUnitResultAsync id x
      let! rValue = Access.evalResultAsync () id x
      Expect.equal rUnit (Ok 0) "accessing eval unit async ignore function should succeed"
      Expect.equal rValue (Ok 0) "accessing eval async ignore function should succeed" }

    testTask "eval task id rule" {
      let x = access { func (fun y -> task { return 0 }) } 
      let rUnit = x.EvalAsync().GetAwaiter().GetResult()
      let rValue = x.EvalAsync(()).GetAwaiter().GetResult()
      Expect.equal rUnit.Value 0 "accessing eval unit task ignore function should succeed"
      Expect.equal rValue.Value 0 "accessing eval task ignore function should succeed" }

    testTask "eval outcome task id rule" {
      let x = Access.Function1 (Func<_> (fun () -> task { return Outcome.Success 0 }))
      let rUnit = x.EvalOutcomeAsync(Func<_, _> id).GetAwaiter().GetResult()
      let rValue = x.EvalOutcomeAsync((), Func<_, _> id).GetAwaiter().GetResult()
      Expect.equal rUnit.Value 0 "accessing eval outcome unit task ignore function should succeed"
      Expect.equal rValue.Value 0 "accessing eval outcome task ignore function should succeed" }

    testCase "func w/o value fails" <| fun () -> 
      Expect.throwsT<ArgumentNullException> (fun () -> Access.Function1 null |> ignore) "access Function1 without value should fail"
      Expect.throwsT<ArgumentNullException> (fun () -> Access.Function2 null |> ignore) "access Function2 without value should fail"
      Expect.throwsT<ArgumentNullException> (fun () -> Access.Action null |> ignore) "access Action without value should fail"
      
    testProperty "during hours fails" <| fun f ->
      let gmin_max_sep = 
        Gen.oneof [ 
          Gen.choose (Int32.MinValue, -1)
          Gen.choose (23, Int32.MaxValue) ]
        |> Gen.two

      let gmin_max_tog = gen {
        let! min = Gen.choose (1, 23)
        let! max = Gen.choose (0, min - 1)
        return min, max}

      withGen (Gen.oneof [ gmin_max_sep; gmin_max_tog ]) <| fun (min, max) ->
        Expect.throwsT<ArgumentException> (fun () -> 
          access { func f; duringHours min max } |> ignore )

    testCase "in role fails" <| fun () ->
      Expect.throwsT<ArgumentNullException> (fun () -> access { inRole null null } |> ignore) "access in role without role should fail"
  ]

[<Tests>]
let accessResultTests =
  testList "access result" [
    testProperty "success" <| fun (NonNull x) ->
      let r = AccessResult.Success x
      r.Successful && r.Value = x && r.ToMaybe() = Maybe<_>(x) && AccessResult.op_Implicit r = Outcome<_, _>(value=x)
      && r.Errors = Array.empty

    testProperty "failure" <| fun (NonNull (xs : NonNull<string> array)) ->
      let xs = Array.map (fun (NonNull x) -> x) xs
      let r = AccessResult<_>.Failure xs
      Expect.throwsT<AccessFailureException> (fun () -> r.Value) "access result value should not be available on failure result"
      not r.Successful && r.Errors = xs && r.ToMaybe() = Maybe<_>.Nothing && r.ToOutcome() = Outcome<_, _>(error=xs)

    testProperty "try get value" <| fun (NonNull x) ->
      let g = Gen.oneof [ 
        Gen.constant <| AccessResult.Success x
        Gen.constant <| AccessResult<_>.Failure [ string x ] ]
      withGen g <| fun r ->
        let output = ref null
        r.TryGetValue (output) = r.Successful

    testCase "failure creation fails" <| fun () ->
      Expect.throwsT<ArgumentNullException> (fun () -> AccessResult<_>.Failure null |> ignore) "access result failure without errors value should fail"
      Expect.throwsT<ArgumentException> (fun () -> AccessResult<_>.Failure [null] |> ignore) "access result failure without errors values should fail"

  ]