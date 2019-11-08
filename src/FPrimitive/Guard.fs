namespace FPrimitive

open System

/// Basic guard class to verify 'null' checks; but only that. Everything else should be written as specifications.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Guard =
  /// Custom guard precondition with an exception instance.
  let against predicate ex =
    if not <| predicate () 
    then raise ex
  /// Custom guard precondition with an exception type and message.
  let againstT<'T when 'T :> exn> predicate message =
    let ex = Activator.CreateInstance (typeof<'T>, (message : string)) :?> 'T
    against predicate ex
  /// Guard against a 'null' reference with a custom exception message. 
  let notNull param message =
    againstT<ArgumentNullException> (fun () -> obj.ReferenceEquals(param, null)) message
  /// Guard against any 'null' reference in the sequence with a custom exception message.
  let notAnyNull param message =
    let ex = ArgumentException message
    against (fun () -> Seq.exists (fun x -> obj.ReferenceEquals(x, null)) param) ex

/// Basic guard class to verify 'null' checks; but only that. Everything else should be written as specifications.
type Guard private () =
  /// Custom guard precondition with an exception instance.
  static member For (predicate : Func<bool>, ex) = 
    if isNull predicate then nullArg "predicate"
    if isNull ex then nullArg "ex"
    Guard.against predicate.Invoke ex
  /// Custom guard precondition with an exception type and message.
  static member For<'TException when 'TException :> exn> (predicate : Func<bool>, message) =
    if isNull predicate then nullArg "predicate"
    if isNull message then nullArg "message"
    Guard.againstT<'TException> predicate.Invoke message
  /// Guard against a 'null' reference with a custom exception message.
  static member NotNull<'T> (param : 'T, message) =
    if isNull message then nullArg "message"
    Guard.notNull param message
  /// Guard against any 'null' reference in the sequence with a custom exception message.
  static member NotAnyNull<'T> (param : 'T seq, message) =
    if isNull message then nullArg "message"
    Guard.notAnyNull param message
