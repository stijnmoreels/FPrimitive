namespace FPrimitive.Tests

open System
open System.IO

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
  let testSpec desc testCase =
    testProperty desc (fun x -> 
      match testCase x with
      | Ok _ -> true.ToProperty()
      | Error (errs : string list) -> 
        false |@ (String.Join (Environment.NewLine, errs)))

  [<Tests>]
  let specTests =
    testList "spec tests" [
      testSpec "not null" <| fun (NonNull x) ->
        Spec.def<string>
        |> Spec.notNull "should not be null"
        |> Spec.validate x

      testSpec "not empty string" <| fun (NonEmptyString x) ->
        Spec.def<string>
        |> Spec.notNull "should not be null"
        |> Spec.notEmpty "should not be empty"
        |> Spec.validate x

      testSpec "greater than" <| fun (PositiveInt x) ->
        Spec.def<int>
        |> Spec.greaterThan 0 "should be greater than zero"
        |> Spec.notEqual 0 "should not be equal to zero"
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
          filter ((=) "get me") "should be 'Ok \"get me\""
          revokedAfter (TimeSpan.FromSeconds 1.) }

        Expect.equal (Access.eval () acc) (Ok "get me") "revokable access should evaluate before revocation expiration"
        do! Async.Sleep 1200
        Expect.notEqual (Access.eval () acc) (Ok "get me") "revokable access should be revoked after revocation expiration" }
      
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
          extension ".txt" }
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
    ]