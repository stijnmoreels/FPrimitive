namespace FPrimitive

open System
open System.IO
open System.Runtime.CompilerServices
open System.Threading

open Microsoft.FSharp.Core

/// Represents an access-controlled function.
type Access<'a, 'b> = 
  internal
      /// Gets the function that is decorated with access-controlled functionality.
    { Capability : 'a -> Result<'b, string list>
      /// Gets the function that revokes the access-controlled function from being further used.
      Revokable : (unit -> unit) option }

/// Provides operations on the `Access<_, _>` type.
module Access =
    /// Creates an access-controlled function from a custom function.
    let func f = { Capability = f >> Ok; Revokable = None }

    /// Creates an access-controlled function from a function, directly using the returned `Result`.
    let funcResult f = { Capability = f; Revokable = None }

    /// Creates an access controlled function from a function, directly using the returned `Option`.
    let funcOption error f = { Capability = f >> Result.ofOption error; Revokable = None }

    [<CompiledName("Function")>]
    let Function2 (f : Func<_, _>) = 
        if f = null then nullArg "f"
        func f.Invoke

    [<CompiledName("Function")>]
    let Function1 (f : Func<_>) =
        if f = null then nullArg "f"
        func f.Invoke

    [<CompiledName("Function")>]
    let Action (f : Action<_>) =
        if f = null then nullArg "f"
        func f.Invoke

    let filter predicate error acc = 
        { acc with Capability = acc.Capability >> Result.filter predicate [ "access failure:" + error ] }
    
    let eval x { Capability = f } = f x
 
    let evalUnit acc = acc.Capability ()

    let revoke acc = Option.iter (fun f -> f ()) acc.Revokable

    let onlyFilesFromDirs (expected : DirectoryInfo seq) =
        let expected = List.ofSeq expected |> List.map (fun (x : DirectoryInfo) -> Path.GetFullPath x.FullName)
        { Revokable = None
          Capability = fun (actual : FileInfo) -> 
            let actualPath = Path.GetDirectoryName (Path.GetFullPath actual.FullName)
            if List.exists (fun expectedPath -> String.Compare (expectedPath, actualPath, ignoreCase=true) = 0) expected
            then Ok actual
            else let dirs = String.Join (", ", Array.ofList expected)
                 Error [ sprintf "access failure: only files from directories: '%s' are allowed, but came from '%s'" dirs actualPath ] }

    let OnlyFilesFromDirectories ([<ParamArray] expected : DirectoryInfo []) =
      onlyFilesFromDirs (Seq.ofArray expected)

    [<CompiledName("OnlyFilesFromDirectory")>]
    let onlyFilesFrom expected = onlyFilesFromDirs [expected]

    let extensions exts acc =
        { acc with
            Capability = fun (actual : FileInfo) ->
                if List.contains actual.Extension exts
                then Ok actual
                else let exts = String.Join (", ", Array.ofList exts)
                     Error [ sprintf "access failure: only files with extension: '%s' are allowed" exts ] }

    let extension ext acc = extensions [ext] acc

    [<CompiledName("OnlyUriFromBase")>]
    let onlyUriFromBase baseUri =
        { Revokable = None
          Capability = fun (actual : Uri) -> 
            if (baseUri : Uri).IsBaseOf actual
            then Ok actual
            else Error [ sprintf "access failure: only URI's with the base of '%s' are allowed, but got: '%s'" baseUri.OriginalString actual.OriginalString ] }

    let once acc =
        let func = ref (Func<_, _> acc.Capability)
        { acc with 
            Capability = fun x -> 
                let func = Interlocked.Exchange (func, null)
                if func = null then Error [ "access failure: function is already evaluated" ]
                else func.Invoke x }

    let times count acc =
        if count < 0 then invalidArg "count" "amount of times to access resource must be greater than zero"
        let count = ref count
        let func = ref (Func<_, _> acc.Capability)
        { acc with
            Capability = fun x ->
                let cnt = Interlocked.Exchange (count, !count - 1)
                if cnt <= 1 then 
                  let func = Interlocked.Exchange (func, null)
                  if func = null then Error [ "access failure: function is already evaluated twice" ]
                  else func.Invoke x
                else (!func).Invoke x }

    let twice acc = times 2 acc

    let revokable acc =
        let allow = ref (true :> obj)
        let capability x =
            if !allow :?> bool then (acc.Capability x)
            else Error [ "access failure: cannot access function because it was revoked" ]
        { Capability = capability
          Revokable = Some (fun () -> Interlocked.Exchange (allow, false) |> ignore) }

    let revokedAfter delay acc =
        let acc = revokable acc
        let timer = new Timers.Timer ()
        timer.Interval <- (delay : TimeSpan).TotalMilliseconds
        timer.Elapsed.Add (fun _ -> 
            revoke acc
            timer.Stop ()
            try timer.Dispose () 
            with _ -> ())
        timer.Start ()
        acc

    let revokedWhen observable acc =
      let acc = revokable acc
      Observable.add (fun _ -> revoke acc) observable
      acc

    let duringDates min max acc =
        { acc with
            Capability = fun x ->
                let now = DateTimeOffset.Now
                if min < now && now > max
                then acc.Capability x
                else Error [ sprintf "access failure: can only access resource between '%A' and '%A', but now it's '%A'" min max now ] }

    let duringHours min max acc =
        if min < 0 || min > 23 then invalidArg "min" "minimum hour should be in the range of 0-23"
        if max < 0 || max > 23 then invalidArg "max" "maximum hour should be in the range of 0-23"
        if min > max then invalidArg "min" "minimum hour cannot be greater than maximum hour" 
        { acc with
            Capability = fun x ->
                let now = DateTimeOffset.Now.Hour
                if min < now && now > max
                then acc.Capability x
                else Error [ sprintf "access failure: can only access resource between '%A' and '%A', but now it's '%A'" min max now ] }
    

    let matching spec acc =
      { acc with
          Capability = fun x -> 
            match Spec.validate x spec with
            | Ok x -> acc.Capability x
            | Error errs -> Error errs }

type AccessBuilder<'a, 'b> () =
    member __.Yield (_) = 
        { Revokable = None
          Capability = fun _ -> Error [ "access failure: no resource was configured" ] }
    [<CustomOperation("func")>]
    member __.Func (_, f) = Access.func f
    [<CustomOperation("funcResult")>]
    member __.FuncResult (_, f) = Access.funcResult f
    [<CustomOperation("funcOption")>]
    member __.FuncOption (_, f) = Access.funcOption f
    [<CustomOperation("onlyFilesFrom")>]
    member __.OnlyFilesFrom (_, dir) = Access.onlyFilesFrom dir
    [<CustomOperation("onlyFilesFromDirs")>]
    member __.OnlyFilesFromDirs (_, dirs) = Access.onlyFilesFromDirs dirs
    [<CustomOperation("extension")>]
    member __.Extension (state, ext) = Access.extension ext state
    [<CustomOperation("extensions")>]
    member __.Extensions (state, exts) = Access.extensions exts state
    [<CustomOperation("onlyUriFromBase")>]
    member __.OnlyUriFrombase (_, baseUri) = Access.onlyUriFromBase baseUri
    [<CustomOperation("filter")>]
    member __.Filter (state, predicate, error) = Access.filter predicate error state
    [<CustomOperation("once")>]
    member __.Once (state) = Access.once state
    [<CustomOperation("twice")>]
    member __.Twice (state) = Access.twice state
    [<CustomOperation("times")>]
    member __.Times (state, count) = Access.times count state
    [<CustomOperation("revokable")>]
    member __.Revokable (state) = Access.revokable state
    [<CustomOperation("revokedAfter")>]
    member __.RevokedAfter (state, delay) = Access.revokedAfter delay state
    [<CustomOperation("revokedWhen")>]
    member __.RevokedWhen (state, observable) = Access.revokedWhen observable state
    [<CustomOperation("duringDates")>]
    member __.DuringDates (state, min, max) = Access.duringDates min max state
    [<CustomOperation("duringHours")>]
    member __.DuringHours (state, min, max) = Access.duringHours min max state

[<AutoOpen>]
module AccessExposure =
    let access<'a, 'b> = AccessBuilder<'a, 'b> ()

exception AccessFailureException of string

type AccessResult<'a> internal (result) =
    member __.Successful = Result.isOk result 
    member __.Error =
        match result with
        | Ok _ -> null
        | Error err -> err
    member __.Value = 
      result
      |> Result.getOrElse (fun errs ->
        let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", errs)
        let message = sprintf "%s: %s" errors
        raise (ValidationFailureException message))
    member __.TryGetValue (output : outref<'a>) =
      match result with
      | Ok x -> output <- x; true
      | _ -> output <- Unchecked.defaultof<'a>; false

[<Extension>]
type AccessExtensions () =
    [<Extension>]
    static member Extension (access, fileExtension) =
        if fileExtension = null then nullArg "fileExtension"
        Access.extension fileExtension access
    [<Extension>]
    static member Extensions (access, [<ParamArray>] fileExtensions) =
        if fileExtensions = null then nullArg "fileExtensions"
        Access.extensions (List.ofArray fileExtensions) access

    [<Extension>]
    static member Once (acc) = Access.once acc

    [<Extension>]
    static member Twice (acc) = Access.twice acc

    [<Extension>]
    static member Times (acc, count) = Access.times count acc

    [<Extension>]
    static member Revokable (acc) = Access.revokable acc

    [<Extension>]
    static member RevokedAfter (acc, delay) = Access.revokedAfter delay acc

    [<Extension>]
    static member RevokedWhen (acc, observable) = Access.revokedWhen observable acc

    [<Extension>]
    static member During (acc, min, max) = Access.duringDates min max acc

    [<Extension>]
    static member DuringHours (acc, min, max) = Access.duringHours min max acc

    [<Extension>]
    static member Revoke (acc : Access<_, _>) = Option.iter (fun f -> f ()) acc.Revokable

    [<Extension>]
    static member Eval (acc : Access<unit, 'b>) =
      Access.eval () acc |> AccessResult<'b>

    [<Extension>]
    static member Eval ((acc : Access<'a, 'b>), (input : 'a)) : AccessResult<'b> = 
        Access.eval input acc |> AccessResult<'b>
        