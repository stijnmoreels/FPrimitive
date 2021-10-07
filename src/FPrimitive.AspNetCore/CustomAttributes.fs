namespace FPrimitive

open System
open System.ComponentModel.DataAnnotations
open System.Collections
open System.Linq
open System.Runtime.InteropServices
open System.Text.Json.Serialization

open Microsoft.FSharp.Core

[<AutoOpen; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validate =
  let model x =
    Spec.validate x
    >> Result.either 
         (fun _ -> ValidationResult.Success) 
         (fun errors -> ValidationResult (String.Join (",", Map.values errors)))

/// <summary>
/// Creates <see cref="ValidationResult"/> instances via validating values through specifications.
/// </summary>
type ValidationResultFactory private () =
  /// <summary>
  /// Validates a given value through a specification resulting in an <see cref="ValidationResult"/> instance.
  /// </summary>
  static member Validate (value : 'T, specification) =
    Validate.model value specification

/// <summary>
/// Validation attribute to verify if a sequence or string is not empty during deserialization.
/// </summary>
/// <param name="errorMessage">The message to provide in the model validation in case of an error</param>
type NotEmptyAttribute ([<Optional>] errorMessage) =
  inherit ValidationAttribute ()
  let getSeqMessage name = defaultArg errorMessage <| sprintf "Sequence '%s' is empty while the model validation requires a not empty sequence" name
  let getStringMessage name = defaultArg errorMessage <| sprintf "String '%s' is empty while the model validaiton requires a not empty string" name
  member private __.seqSpec name = spec { nonEmpty (getSeqMessage name) }
  member private __.stringSpec name = spec { notEmpty (getStringMessage name) }
  override this.IsValid (x, ctx) =
    match x with
    | :? string as str -> this.stringSpec ctx.DisplayName |> Validate.model str
    | :? IEnumerable as xs -> this.seqSpec ctx.DisplayName |> Validate.model (xs.Cast<obj>())
    | _ ->  ValidationResult.Success

/// <summary>
/// Validation attribute to verify if a sequence only contains unique elements during deserialization.
/// </summary>
/// <param name="errorMessage">The message to provide in the model validation in case of an error</param>
type UniqueAttribute ([<Optional>] errorMessage) =
  inherit ValidationAttribute ()
  let getMessage name = defaultArg errorMessage <| sprintf "Sequence '%s' has duplicate values while the model valdidation requires only unique values" name
  member private __.seqSpec name = spec { unique id (getMessage name) }
  override this.IsValid (x, ctx) =
    match x with
    | :? IEnumerable as xs -> this.seqSpec ctx.DisplayName |> Validate.model (xs.Cast<obj>())
    | _ -> ValidationResult.Success

/// <summary>
/// Validation attribute to verify if a string is not blank.
/// </summary>
/// <param name="errorMessage">The message to provide in the model validation in case of an error</param>
type NotBlankAttribute ([<Optional>] errorMessage) =
  inherit ValidationAttribute ()
  let getMessage name = defaultArg errorMessage <| sprintf "String '%s' is blank while the model validation requires a non-blank string" name
  member private __.stringSpec name = spec { notNullOrWhiteSpace (getMessage name) }
  override this.IsValid (x, ctx) =
    match x with
    | :? string as str -> this.stringSpec ctx.DisplayName |> Validate.model str
    | _ -> ValidationResult.Success

/// <summary>
/// Validation attribute to verify if a string or sequence doesn't contain a specified value.
/// </summary>
/// <param name="value">The value that the string or sequence should not contain</param>
/// <param name="errorMessage">The message to provide in the model validation in case of an error</param>
type NotContainsAttribute (value : obj, [<Optional>] errorMessage) =
  inherit ValidationAttribute ()
  let stringMessage name = defaultArg errorMessage <| sprintf "String '%A' contains value '%A' while the model validation states that the string should not contain the value" name value
  let seqMessage name = defaultArg errorMessage <| sprintf "Sequence '%A' contains '%A' while the model validation states that the sequence should not contain the value" name value
  member private __.stringSpec name = spec { stringNotContains (string value) (stringMessage name) }
  member private __.seqSpec name = spec { notContains value (seqMessage name) }
  override this.IsValid (x, ctx) =
    match x with
    | :? string as str -> this.stringSpec ctx.DisplayName |> Validate.model str
    | :? IEnumerable as xs -> this.seqSpec ctx.DisplayName |> Validate.model (xs.Cast<obj>())
    | _ -> ValidationResult.Success

type StringJsonConverter (sanitize) =
  inherit JsonConverter<string> ()
  override __.Read (reader, typeToConvert, options) =
    reader.GetString () |> sanitize
  override __.Write (writer, value, options) =
    sanitize value |> writer.WriteStringValue

type EscapeJsonConverter () = inherit StringJsonConverter (Sanitize.escape)
type EscapeAttribute () = 
  inherit JsonConverterAttribute (typeof<EscapeJsonConverter>)

type UpperJsonConverter () = inherit StringJsonConverter (Sanitize.upper)
type UpperAttribute () = inherit JsonConverterAttribute (typeof<UpperJsonConverter>)

type LowerJsonConverter () = inherit StringJsonConverter (Sanitize.lower)
type LowerAttribute () = inherit JsonConverterAttribute (typeof<LowerJsonConverter>)

type TruncateJsonConverter (length) = inherit StringJsonConverter (Sanitize.max length)
type TruncateAttribute (length) = 
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) = TruncateJsonConverter (length) :> JsonConverter
  
type TrimJsonConverter (values) = inherit StringJsonConverter (Sanitize.trim values)
type TrimAttribute (values) =
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) = TrimJsonConverter (values) :> JsonConverter

type AllowMatchJsonConverter (pattern) = 
  inherit StringJsonConverter (Sanitize.whitematch pattern)
type AllowMatchAttribute (pattern) =
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) = AllowMatchJsonConverter (pattern) :> JsonConverter

type DenyMatchJsonConverter (pattern) =
  inherit StringJsonConverter (Sanitize.blackmatch pattern)
type DenyMatchAttribute (pattern) =
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) = DenyMatchJsonConverter pattern :> JsonConverter

type ReplaceJsonConverter (oldString : string, newString : string) =
  inherit StringJsonConverter (fun str -> str.Replace (oldString, newString))
type ReplaceAttribute (oldString, newString) =
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) = 
    ReplaceJsonConverter (oldString, newString) :> JsonConverter

type AsciiJsonConverter () =
  inherit StringJsonConverter (fun str -> Sanitize.ascii str)
type AsciiAttribute () =
  inherit JsonConverterAttribute ()
  override __.CreateConverter (_) =
    AsciiJsonConverter () :> JsonConverter