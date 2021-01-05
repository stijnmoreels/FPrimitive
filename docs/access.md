# Accessibility

The `FPrimitive` library contains a way to express access on certain parts of the domain/application.
A critical function that can be revoked from running? A password or access key that should only be read once? ...

Any part can be expressed as a `Access<_, _>` type which defines a set of rules before the resource can be accessed.

For example, the access key config value should only be accessed once:

```fsharp
// F#
let (getAccessKeyOnce : Access<unit, string>) =
  Access.func (fun () -> "secret key")
  |> Access.once
```
```csharp
Access.Function1(() => "secret key").Once()
```

The `Access` function can then be evaluated, and will result in a `Result<_, _>` type; indicating either a successful or faulted access.

```fsharp
// F#
let (result : Result<string, string list>) =
  getAccessKeyOnce |> Access.eval ()
```
```csharp
// C#
AccessResult<string> result = getAccessKeyOnce.Eval()
```

## Revokability

The library comes with several features, one of which is revokability. When the `Access.revokable`/`.Revokable()` is added to the pipeline; 
the resulting `Access` type can be revoked by calling `Access.revoke`/`.Revoke()` on the `Access` type. This makes the access part manually controlable.

The `Access` type can also be revoked automatically with these pipeline features:
* `Access.revokedAfter (TimeSpan)`/`.RevokedAfter(TimeSpan)`
* `Access.revokedWhen (IObservable)`/`.RevokedWhen(IObservable)`