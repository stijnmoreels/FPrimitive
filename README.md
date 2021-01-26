# FPrimitive

FPrimitive is a .NET project to help developers build a correct and more secure domain model by providing building blocks, standard types and trust boundaries.

<img src="/docs/img/logo.png" width=100 height=100 alt="logo" />

## NuGet

[![NuGet Badge](https://buildstats.info/nuget/fprimitive)](https://www.nuget.org/packages/fprimitive)

## Build Status

| Mono                                                                                                                              | .NET                                                                                                                                                   |
| --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [![Build Status](https://travis-ci.org/stijnmoreels/FPrimitive.svg?branch=master)](https://travis-ci.org/stijnmoreels/FPrimitive) | [![.NET Build status](https://ci.appveyor.com/api/projects/status/2ijw1am46pyhqnur?svg=true)](https://ci.appveyor.com/project/stijnmoreels/fprimitive) |

## Documentation
The project has a wiki page with further information and some additional resources are also listed here:
* [Library Documentation Page](https://stijnmoreels.github.io/FPrimitive)
* [Domain-Driven Security w/ F# FPrimitive & C# Interop](https://www.codit.eu/blog/domain-driven-security-with-f-fprimitive-and-c-interop/)
* [From Untrusted Input to Strict Model w/ Layered JSON Parsing in F# FPrimitive](https://www.codit.eu/blog/from-untrusted-input-to-strict-model-with-layered-json-parsing-in-f-fprimitive-c/)

## Examples
The project contains several reusable building blocks to make your domain model more correct and therefore more secure.


```fsharp
open FPrimitive

/// Composible specifications for your domain types:
type NonEmptyString =
  private NonEmptyString of string with
    static member create x : Result<NonEmptyString, Map<string, string list>> =
      Spec.def<string>
      |> Spec.notNull "should not be null"
      |> Spec.notEmpty "should not be empty"
      |> Spec.createModel NonEmptyString x

/// ...also available as computation expression.
type NonEmptyList<'a> =
  private NonEmptyList of 'a list with
    static member create xs : Result<NonEmptyList<'a>, Map<string, string list>> =
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
Access.revoke critical
```

With full C# support!

```csharp
using FPrimitive;

public class PositiveInt
{
    private PrositiveInt(int value) { Value = int; }

	public int value { get; }

	public static ValidationResult<PositiveInt> Create(int value)
	{
		return Spec.Of<int>()
			   .GreaterThan(0, "Positive integer should be greater than zero")
			   .CreateModel(value, validated => new PositiveInt(validated));
	}
}
```
