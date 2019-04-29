namespace FPrimitive.Tests

open Expecto
open FsCheck

module Tests =
    [<Tests>]
    let testSimpleTests =

        testList "DomainTypes.Tag" [
            testCase "equality" <| fun () ->
                let result = 42
                Expect.isTrue (result = 42) "Expected True"

            testProperty "whitespace" <|
                fun  () ->
                    Prop.forAll Arb.from<int>
                        (fun (x) -> 
                            x = x)
        ]

