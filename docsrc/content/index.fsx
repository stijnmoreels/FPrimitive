(*** hide ***)
#r "../../src/FPrimitive/bin/Release/netstandard2.0/FPrimitive.dll"
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.

(**
FPrimitive
======================

*)
open FPrimitive

/// Composible specifications for your domain types:
type NonEmptyString =
  private
  | NonEmptyString of string
  static member create x =
    Spec.def<string>
    |> Spec.notNull "should not be null"
    |> Spec.notEmpty "should not be empty"
    |> Spec.createModel NonEmptyString x

/// ...also available as computation expression.
type NonEmptyList<'a> =
  private
  | NonEmptyList of 'a list
  static member create xs =
    specModel NonEmptyList xs {
      nonEmpty "list should not be empty"
      lengthBetween 1 10 "list length should be between 1-10" }


/// Access controllers for your critical application resources:
let uncontrolled_critical = ignore

let critical =
  Access.func uncontrolled_critical
  |> Access.once
  |> Access.duringHours 9 17
  |> Access.revokable

/// Try to evaluate
Access.eval () critical
/// Revoke when neccessary
Access.revoke
(**

The library comes with comprehensible documentation about the major parts of the project and the complete API reference of the project:

 * [Specifications](spec.html) contains some in-depth documentation on the specifications and `Spec` module of the library.
 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork
the project and submit pull requests. If you're adding a new public API, please also
consider adding [samples][content] that can be turned into a documentation.

The library is available under Public Domain license, which allows modification and
redistribution for both commercial and non-commercial purposes. For more information see the
[License file][license] in the GitHub repository.

Icons made by [Vectors Market][vectorsmarket] from [www.flaticon.com][flaticon] is licensed by [CC 3.0][cc].

  [content]: https://github.com/stijnmoreels/FPrimitive/tree/master/docsrc/content
  [gh]: https://github.com/stijnmoreels/FPrimitive
  [issues]: https://github.com/stijnmoreels/FPrimitive/issues
  [license]: https://github.com/stijnmoreels/FPrimitive/blob/master/LICENSE.txt
  [vectorsmarket]: https://www.flaticon.com/authors/vectors-market
  [flaticon]: https://www.flaticon.com/
  [cc]: http://creativecommons.org/licenses/by/3.0/
*)