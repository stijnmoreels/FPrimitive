namespace FPrimitive.Tests

open System
open System.IO

open Microsoft.FSharp.Core

open Expecto
open FsCheck
open FPrimitive
open Microsoft.FSharp.Reflection
open Fare

#nowarn "0059"

[<AutoOpen>]
module Extensions =
  let (<=>) x y = x = y |@ (sprintf "%A = %A" x y)

  let fromString<'a> (s:string) =
      match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
      |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
      |_ -> None

[<AutoOpen>]
module Tests =
  let toProp = function
    | Ok _ -> true.ToProperty()
    | Error (errs : ErrorsByTag) -> 
      false |@ (String.Join (Environment.NewLine, Map.values errs))
  let testSpec desc testCase =
    testProperty desc (testCase >> toProp)

  let withGen g t = Arb.fromGen g |> Prop.forAll <| t

  [<Tests>]
  let untrust_tests =
    testList "untrust" [
      testProperty "try get value" <| fun (NonNull x) f ->
        let u = Untrust x
        let r = Untrust.getWith f u
        let output = ref null
        let getValue = u.TryGetValue (Func<_, _> (f), output)
        f x = (r = Some x) |@ "fsharp" .&. (getValue = (output.Value = x)) |@ "csharp"
      
      testProperty "try get value option" <| fun x f ->
        let u = Untrust x
        let r = Untrust.getWithOption f u
        r = f x .&. (u.TryGetValue (Func<_, Maybe<_>> (f >> Maybe.JustOrNothing)) = (Maybe.JustOrNothing (f x)))
      
      testProperty "try get value result" <| fun x f ->
        let u = Untrust x
        let r = Untrust.getWithResult f u
        r = f x .&. (u.TryGetValue (Func<_, Outcome<_, _>> (f >> Outcome.OfFSharpResult)) = (Outcome.OfFSharpResult (f x)))
      
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

      testProperty "try get value w/o validator fails" <| fun x ->
        let u = Untrust x
        let output = ref null
        Expect.throwsT<ArgumentNullException> (fun () -> u.TryGetValue (validator=null, output=output) |> ignore) "try get value without validator should throw"
        Expect.throwsT<ArgumentNullException> (fun () -> u.TryGetValue (validator=(null :> Func<_, Maybe<_>>)) |> ignore) "try get maybe value without validator should throw"
        Expect.throwsT<ArgumentNullException> (fun () -> u.TryGetValue (validator=(null :> Func<_, Outcome<_, _>>)) |> ignore) "try get outcome value without validator should throw"

      testProperty "to string contains value" <| fun x ->
        let u = Untrust x |> string
        Expect.stringContains u (sprintf "%A" x) "untrusted string should contain original value"
    ]