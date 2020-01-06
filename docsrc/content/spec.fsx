(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FPrimitive/bin/Release/netstandard2.0/FPrimitive.dll"

(**
Specifications
======================

The `FPrimitive` library contains a way to describe how domain models should look like by specifiying **specifications**.
These specifications are a set of requirements that sets how a set of values should look like.

> The return type is always a `Result<_, _>` where the `Error` is a `Map<_, _>` where the errors are grouped together (more on this later).

For example, a model that only can contain positie numbers could be described as:
*)

type PositiveInt = 
  private PositiveInt of int with
    static member create x =
      if x >= 0 then Ok (PositiveInt x)
      else Error "integer should be greater than zero"

(** 
When things start to get complicated, above approach can get really complicated and overly complex to maintain.
That's where this library can help with (following sample shows the same functionality implemented with the library).
*)

open FPrimitive

type PositiveInt' =
  private PositiveInt of int with
    static member create x =
      Spec.def 
      |> Spec.greaterThanOrEqual 0 "integer should be greater than zero"
      |> Spec.createModel PositiveInt x

(** 

Either with `Spec.def` or with the composition expression `spec { }` can specifications be made (some other CE exists to shortcut some functionality: `specResult`, `specModel`, `specOption`, ...).

Starting Your First Specification
---------------------------------

Specifications can be created with the `Spec.def<_>` identity value that will create an empty spec for your type:
*)

// F#
Spec.def<int>

// C#
Spec.Of<int>()

(** 
Then, additional requirements can be added for that type. See the API reference of the `Spec` module for all the available built-in requirements.
Each requirement will require an error message that all will be used when the validation fails.
*)

// F#
Spec.def<int> 
|> Spec.greaterThan 5 "should be greater than 5"
|> Spec.lessThanOrEqual 10 "should be <= 10"
|> Spec.inclusiveBetween 5 10 "should be between 5-10"
|> Spec.equal 5 "should be equal to 5"

// C#
Spec.Of<int>()
    .GreaterThan(5, "should be greater than 5")
    .LessThanOrEqual(10, "should be <= 10")
    .InclusiveBetween(5, 10, "should be between 5-10")
    .Equal(5, "should be equal to 5")

(** 
Validate Your Specification
---------------------------

When you want to validate an incoming value with your specification, there are several ways to do this:
*)

// F#

// Validate directly to have a result type with grouped errors.
let result : Result<int, _> = Spec.def<int> |> Spec.validate 0

// Creates the model directly after the validation succeeded.
let model : Result<int option, _> = Spec.def<int> |> Spec.createModel Some 0

// C#

ValidationResult<int> result = Spec.Of<int>().Validate(0);

ValidationResult<Nullable<int>> result = Spec.Of<int>().CreateModel(0, x => new Nullable<int>(x));

(** 
Structure Your Errors
---------------------

The `Error` type of the `Result<_, _>` type is a `Map<string, string list>` (`IDictionary` in C#).
This represents a grouping of errors by a tag.

When you create your specification, you can pass allong a tag name which makes sure that all errors will be grouped together under that tag.
*)

// F#
Spec.tag "name"

// C#
Spec.Of("name")

(** 
Sometimes, you want to add an lower-level/additional tag name to structure for example requirements related to the length of the value.
- Each built-in requirement also has a `...Of` variant that takes in an additional mapping function.
- Each error message you pass allong, can have a single additional tag name by specifying an `@...` in from of it.

With these two featuers, we can create a more detailed error structure.
*)

// F#
Spec.tag "name"
|> Spec.lengthMaxOf (fun (s : string) -> s.ToCharArray()) 10 "@length of the name should be max 10 char long"

// C#
Spec.Of("name").LengthMaxOf(s => s.ToCharArray(), 10, "@length of the name should be max 10 char long");

(** 
When this specification fails, the error result would be a map with a single entry:

- key:`name.length`: value:`@length of the name should be max 10 char long`.

Cascade: stop/continue on first failure
---------------------------------------

Each specification is by default configured to **Stop** on the first failure it comes across. This configuration can be alterd so the specification will run all configured requirements.
This can be useful if the client wants to know all the errors at once instead of a trial-error until each requirement is met.
*)

// F#
Spec.def |> Spec.cascade Continue

// C#
Spec.Of<int>().Cascade(CascadeMode.Continue);

(**

Advanced Scenario's
-------------------

The `Spec` module also contains some advanced combination and lower-level function:

- `Spec.dependsOn` will allow you to pre-validate another specification before you validate the requirements; it 'depends on' other requirements first.
- `Spec.merge` will create a new specification with two other specifications as 'dependents'.
- `Spec.map` will map all the requirements in the specification so you can pass allong another value.
- `Spec.comap` will map the value before it comes into the requirement
- `Spec.preval` adds a pre-validation function to all the requirements
- `Spec.list`, `.seq`, `.array` maps the input value so it can work with a series of values; ex. to validate a list of positive integers
- `Spec.invariant` allows to combine two specifications and add new requirements that will validate both values at the same time; good for relationships between values

*)

(**
Composition Expressions
-----------------------

The exact same functionality for the requirements can be expressed with composition expressions.

*)

Spec.def |> Spec.greaterThan 0 "should be greater than zero"

spec { greaterThan 0 "should be greater than zero" }

(** 
Some shortcut CE are also available if you want to create the model right away, have the validation run when the CE ends...
*)

// Validate directly.
let (x : Result<int, _>) =
  Spec.def 
  |> Spec.exclusiveBetween -1 11 "should be between 0-10" 
  |> Spec.validate 5

let (x : Result<int, _>) = specResult 5 { 
  exclusiveBetween -1 11 "should be between 0-10" }

// Create model
let (x : Result<int option, _>) =
  Spec.def
  |> Spec.greaterThanOrEqualOf id 0 "should be greater than zero"
  |> Spec.createModel Some 5

let (x : Result<int option, _>) = specModel Some 5 {
  greaterThanOrEqualOf id 0 "should be greater than zero" }

(**

Advanced Example
----------------

Following sample shows how a ISBN13 book number can be expressed with specifications:
*)

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
        tag "isbn13"
        notNullOrWhiteSpace "ISBN13 number should not be blank"
        equalOf String.length 13 "ISBN13 number should have a @length of 13 characters"
        startsWith "987" "ISBN13 number should start with '987'"
        matches pattern (sprintf "ISBN13 number should match regular expression: %s" pattern)
        verify checksum "ISBN13 @checksum was invalid" }

