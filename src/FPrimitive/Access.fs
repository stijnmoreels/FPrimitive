namespace FPrimitive

open System
open System.IO
open System.Runtime.CompilerServices
open System.Threading

open Microsoft.FSharp.Core

/// Represents an access-controlled function.
type Access<'T, 'TResult> = 
  internal
      /// Gets the function that is decorated with access-controlled functionality.
    { Capability : 'T -> Result<'TResult, string list>
      /// Gets the function that revokes the access-controlled function from being further used.
      Revokable : (unit -> unit) option }

/// Provides operations on the `Access<_, _>` type.
module Access =
  /// Creates an access-controlled function from a custom function.
  let func f = { Capability = f >> Ok; Revokable = None }

  /// Creates an access-controlled function from a function, directly using the returned `Result`.
  let funcResult f = { Capability = f; Revokable = None }

  /// Creates an access controlled function from a function, directly using the returned `Option`.
  let funcOption f error = { Capability = f >> Result.ofOption [error]; Revokable = None }

  /// Creates an access-controlled function from a custom function.
  [<CompiledName("Function")>]
  let Function2 (f : Func<'T, 'TResult>) = 
    if f = null then nullArg "f"
    func f.Invoke

  /// Creates an access-controlled function from a custom function.
  [<CompiledName("Function")>]
  let Function1 (f : Func<'TResult>) =
    if f = null then nullArg "f"
    func f.Invoke

  /// Creates an access controlled function from a custom function.
  [<CompiledName("Function")>]
  let Action (f : Action<'T>) =
    if f = null then nullArg "f"
    func f.Invoke

  /// Adds a custom requirement that the access-controlled function.
  let filter predicate error acc = 
    { acc with 
        Capability = fun x -> 
          if predicate x then acc.Capability x 
          else Error [ "access failure:" + error ] }
  
  /// Evaluate the access-controlled function.
  let eval x { Capability = f } = f x
 
  /// Evaluate the access-controlled function with unit `()`.
  let evalUnit acc = acc.Capability ()

  /// Revokes the access-controlled function.
  let revoke acc = Option.iter (fun f -> f ()) acc.Revokable

  /// Creates an access-controlled function that only retrieves files from certain directories.
  let onlyFilesFromDirs (expected : DirectoryInfo seq) =
    let expected = List.ofSeq expected |> List.map (fun (x : DirectoryInfo) -> Path.GetFullPath x.FullName)
    { Revokable = None
      Capability = fun (actual : FileInfo) -> 
        let actualPath = Path.GetDirectoryName (Path.GetFullPath actual.FullName)
        if List.exists (fun expectedPath -> String.Compare (expectedPath, actualPath, ignoreCase=true) = 0) expected
        then Ok actual
        else let dirs = String.Join (", ", Array.ofList expected)
             Error [ sprintf "access failure: only files from directories: '%s' are allowed, but came from '%s'" dirs actualPath ] }

  /// Creates an access-controlled function that only retrieves files from certain directories.
  let OnlyFilesFromDirectories ([<ParamArray>] expected : DirectoryInfo []) =
    onlyFilesFromDirs (Seq.ofArray expected)

  /// Creates an access-controlled function that only retrieves files from a certain directory.
  [<CompiledName("OnlyFilesFromDirectory")>]
  let onlyFilesFrom expected = onlyFilesFromDirs [expected]

  /// Adds a requirement to the access-controlled function to only allow files with certain extensions.
  let fileExtensions exts acc =
    { acc with
        Capability = fun (actual : FileInfo) ->
          if List.contains actual.Extension exts
          then Ok actual
          else let exts = String.Join (", ", Array.ofList exts)
               Error [ sprintf "access failure: only files with extension: '%s' are allowed" exts ] }

  /// Adds a requirement to the access-controlled function to only allow files with a certain extension.
  let fileExtension ext acc = fileExtensions [ext] acc

  /// Creates an access-controlled function to only allow `Uri` values with a certain base `Uri`.
  [<CompiledName("OnlyUriFromBase")>]
  let onlyUriFromBase baseUri =
    { Revokable = None
      Capability = fun (actual : Uri) -> 
        if (baseUri : Uri).IsBaseOf actual
        then Ok actual
        else Error [ sprintf "access failure: only URI's with the base of '%s' are allowed, but got: '%s'" baseUri.OriginalString actual.OriginalString ] }

  /// Let the access-controlled function only evaluate once.
  let once acc =
    let func = ref (Func<_, _> acc.Capability)
    { acc with 
        Capability = fun x -> 
          let func = Interlocked.Exchange (func, null)
          if func = null then Error [ "access failure: function is already evaluated" ]
          else func.Invoke x }

  /// Let the access-controlled function only evaluate a certain amount of times.
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

  /// Let the access-controlled function only evaluate two times.
  let twice acc = times 2 acc

  /// Make the access-controlled function revokable.
  let revokable acc =
      let allow = ref (true :> obj)
      let capability x =
          if !allow :?> bool then (acc.Capability x)
          else Error [ "access failure: cannot access function because it was revoked" ]
      { Capability = capability
        Revokable = Some (fun () -> Interlocked.Exchange (allow, false) |> ignore) }

  /// Automatically revoke the access-controlled after a specified delay.
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

  /// Automatically revoke the access-controlled function when the specified observable emits a value.
  let revokedWhen observable acc =
    let acc = revokable acc
    Observable.add (fun _ -> revoke acc) observable
    acc

  /// Make the access-controlled function only available during certain dates.
  let duringDates min max acc =
    { acc with
        Capability = fun x ->
          let now = DateTimeOffset.Now
          if min < now && now > max
          then acc.Capability x
          else Error [ sprintf "access failure: can only access resource between '%A' and '%A', but now it's '%A'" min max now ] }

  /// Make the access-controlled function only available during certain hours.
  let duringHours min max acc =
    if min < 0 || min > 23 then invalidArg "min" "minimum hour should be in the range of 0-23"
    if max < 0 || max > 23 then invalidArg "max" "maximum hour should be in the range of 0-23"
    if min > max then invalidArg "min" "minimum hour cannot be greater than maximum hour" 
    { acc with
        Capability = fun x ->
          let now = DateTimeOffset.Now
          if min < now.Hour && now.Hour < max
          then acc.Capability x
          else Error [ sprintf "access failure: can only access resource between '%i:00' and '%i:00', but now it's '%i:%i'" min max now.Hour now.Minute ] }
  
  /// Adds input-validation to the access-controlled function.
  let satisfy spec acc =
    { acc with
        Capability = fun x -> 
          match Spec.validate x spec with
          | Ok x -> acc.Capability x
          | Error errs -> Error errs }

/// Computation expression to control the access of functions.
type AccessBuilder<'a, 'b> () =
    member __.Yield (_) = 
        { Revokable = None
          Capability = fun _ -> Error [ "access failure: no resource was configured" ] }
    /// Creates an access controlled function from a custom function.
    [<CustomOperation("func")>]
    member __.Func (_, f) = Access.func f
    /// Creates an access-controlled function from a function, directly using the returned `Result`.
    [<CustomOperation("funcResult")>]
    member __.FuncResult (_, f) = Access.funcResult f
    /// Creates an access controlled function from a function, directly using the returned `Option`.
    [<CustomOperation("funcOption")>]
    member __.FuncOption (_, f, error) = Access.funcOption f error
    /// Creates an access-controlled function that only retrieves files from a certain directory.
    [<CustomOperation("onlyFilesFrom")>]
    member __.OnlyFilesFrom (_, dir) = Access.onlyFilesFrom dir
    /// Creates an access-controlled function that only retrieves files from certain directories.
    [<CustomOperation("onlyFilesFromDirs")>]
    member __.OnlyFilesFromDirs (_, dirs) = Access.onlyFilesFromDirs dirs
    /// Adds a requirement to the access-controlled function to only allow files with a certain extension.
    [<CustomOperation("fileExtension")>]
    member __.Extension (state, ext) = Access.fileExtension ext state
    /// Adds a requirement to the access-controlled function to only allow files with certain extensions.
    [<CustomOperation("filesExtensions")>]
    member __.Extensions (state, exts) = Access.fileExtensions exts state
    /// Creates an access-controlled function to only allow `Uri` values with a certain base `Uri`.
    [<CustomOperation("onlyUriFromBase")>]
    member __.OnlyUriFrombase (_, baseUri) = Access.onlyUriFromBase baseUri
    [<CustomOperation("filter")>]
    member __.Filter (state, predicate, error) = Access.filter predicate error state
    /// Let the access-controlled function only evaluate once.
    [<CustomOperation("once")>]
    member __.Once (state) = Access.once state
    /// Let the access-controlled function only evaluate two times.
    [<CustomOperation("twice")>]
    member __.Twice (state) = Access.twice state
    /// Let the access-controlled function only evaluate a certain amount of times.
    [<CustomOperation("times")>]
    member __.Times (state, count) = Access.times count state
    /// Make the access-controlled function revokable.
    [<CustomOperation("revokable")>]
    member __.Revokable (state) = Access.revokable state
    /// Automatically revoke the access-controlled after a specified delay.
    [<CustomOperation("revokedAfter")>]
    member __.RevokedAfter (state, delay) = Access.revokedAfter delay state
    /// Automatically revoke the access-controlled function when the specified observable emits a value.
    [<CustomOperation("revokedWhen")>]
    member __.RevokedWhen (state, observable) = Access.revokedWhen observable state
    /// Make the access-controlled function only available during certain dates.
    [<CustomOperation("duringDates")>]
    member __.DuringDates (state, min, max) = Access.duringDates min max state
    /// Make the access-controlled function only available during certain hours.
    [<CustomOperation("duringHours")>]
    member __.DuringHours (state, min, max) = Access.duringHours min max state
    /// Adds input-validation to the access-controlled function.
    [<CustomOperation("satisfy")>]
    member __.Satisfy (state, spec) = Access.satisfy spec state

/// Exposure values for the `Access<_, _>` type.
[<AutoOpen>]
module AccessExposure =
    let access<'a, 'b> = AccessBuilder<'a, 'b> ()

/// Thrown when the access to the result of an access-controlled function is not available.
exception AccessFailureException of string

/// Result type when the access-controlled function is evaluated.
[<Struct; NoEquality; NoComparison>]
type AccessResult<'a> internal (result : Result<'a, string list>) =
    /// Gets a value indicating whether or not the access-controlled function was evaluated successfully.
    member __.Successful = Result.isOk result 
    /// Gets the series of errors that occured during the evaluation of the access-controlled function.
    member __.Errors =
      match result with
      | Ok _ -> [||]
      | Error errs -> Array.ofList errs
    /// Gets the result of the access-controlled function if the function evaluated successfully, 
    /// otherwise an `AccessFailureException` is thrown.
    member __.Value = 
      result
      |> Result.getOrElse (fun errs ->
        let errors = Environment.NewLine + String.Join (Environment.NewLine + " -> ", Array.ofList errs)
        let message = sprintf "Access to resource failed: %s" errors
        raise (AccessFailureException message))
    /// Tries to get the result of the access-controlled function based on whether the function evaluated successfully.
    member __.TryGetValue (output : outref<'a>) =
      match result with
      | Ok x -> output <- x; true
      | _ -> output <- Unchecked.defaultof<'a>; false

/// Extensions on the `Access<_, _>` type to use in C# context.
[<Extension>]
type AccessExtensions () =
    /// Adds a requirement to the access-controlled function to only allow files with a certain extension.
    [<Extension>]
    static member FileExtension (access, fileExtension) =
      if fileExtension = null then nullArg "fileExtension"
      Access.fileExtension fileExtension access
    /// Adds a requirement to the access-controlled function to only allow files with certain extensions.
    [<Extension>]
    static member FileExtensions (access, [<ParamArray>] fileExtensions) =
      if fileExtensions = null then nullArg "fileExtensions"
      Access.fileExtensions (List.ofArray fileExtensions) access
    /// Let the access-controlled function only evaluate once.
    [<Extension>]
    static member Once (acc : Access<'T, 'TResult>) = Access.once acc
    /// Let the access-controlled function only evaluate two times.
    [<Extension>]
    static member Twice (acc : Access<'T, 'TResult>) = Access.twice acc
    /// Let the access-controlled function only evaluate a certain amount of times.
    [<Extension>]
    static member Times (acc : Access<'T, 'TResult>, count) = Access.times count acc
    /// Make the access-controlled function revokable.
    [<Extension>]
    static member Revokable (acc : Access<'T, 'TResult>) = Access.revokable acc
    /// Automatically revoke the access-controlled after a specified delay.
    [<Extension>]
    static member RevokedAfter (acc : Access<'T, 'TResult>, delay) = Access.revokedAfter delay acc
    /// Automatically revoke the access-controlled function when the specified observable emits a value.
    [<Extension>]
    static member RevokedWhen (acc : Access<'T, 'TResult>, observable) = Access.revokedWhen observable acc
    /// Make the access-controlled function only available during certain dates.
    [<Extension>]
    static member During (acc : Access<'T, 'TResult>, min, max) = Access.duringDates min max acc
    /// Make the access-controlled function only available during certain hours.
    [<Extension>]
    static member DuringHours (acc : Access<'T, 'TResult>, min, max) = Access.duringHours min max acc
    /// Adds input-validation to the access-controlled function.
    [<Extension>]
    static member Satisfy (acc : Access<'T, 'TResult>, spec) = Access.satisfy spec acc
    /// Revokes the access-controlled function.
    [<Extension>]
    static member Revoke (acc : Access<'T, 'TResult>) = Option.iter (fun f -> f ()) acc.Revokable
    /// Evaluate the access-controlled function.
    [<Extension>]
    static member Eval (acc : Access<unit, 'TResult>) = Access.eval () acc |> AccessResult<'TResult>
    /// Evaluate the access-controlled function.
    [<Extension>]
    static member Eval (acc : Access<'T, 'TResult>, input : 'T) : AccessResult<'TResult> = Access.eval input acc |> AccessResult<'TResult>