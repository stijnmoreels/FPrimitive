(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FPrimitive/bin/Release/netstandard2.0/FPrimitive.dll"
open FPrimitive

(**
Accessibility
=============

The `FPrimitive` library contains a way to express access on certain parts of the domain/application.
A critical function that can be revoked from running? A password or access key that should only be read once? ...

Any part can be expressed as a `Access<_, _>` type which defines a set of rules before the resource can be accessed.

For example, the access key config value should only be accessed once:

*)

// F#
let (getAccessKeyOnce : Access<unit, string>) =
  Access.func (fun () -> "secret key")
  |> Access.once

// C#
Access.Function1(() => "secret key").Once()

(**
The `Access` function can then be evaluated, and will result in a `Result<_, _>` type; indicating either a successful or faulted access.
*)

// F#
let (result : Result<string, string list>) =
  getAccessKeyOnce |> Access.eval ()

// C#
AccessResult<string> result = getAccessKeyOnce.Eval()


(**
Revokability
------------

The library comes with several features, one of which is revokability. When the `Access.revokable`/`.Revokable()` is added to the pipeline; 
the resulting `Access` type can be revoked by calling `Access.revoke`/`.Revoke()` on the `Access` type. This makes the access part manually controlable.

The `Access` type can also be revoked automatically with these pipeline features:
* `Access.revokedAfter (TimeSpan)`/`.RevokedAfter(TimeSpan)`
* `Access.revokedWhen (IObservable)`/`.RevokedWhen(IObservable)`
*)
