﻿namespace FPrimitive

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Text
open System.Text.RegularExpressions
open System.Runtime.CompilerServices
open System.Net

/// Sanitization operations on a string, filtering the untrusted user-input before any parsing, syntax, deserialization, or validation.
module Sanitize =
  /// Switch to an empty string when the input is 'null'.
  [<CompiledName("OfNull")>]
  let ofNull x = if isNull x then "" else x
  /// Switch to an empty string when the input is 'null'.
  [<CompiledName("EmptyWhenNull")>]
  [<Obsolete("Use 'ofNull' instead")>]
  [<ExcludeFromCodeCoverage>]
  let empty_when_null input =
    if isNull input then String.Empty else input
  /// Only allow the matches in the given input for the given regular expression.
  [<CompiledName("AllowRegex")>]
  let allowregex (regex : Regex) input =
    let matches = regex.Matches input
    let values = seq { for m in matches do yield m.Value }
    String.Join (String.Empty, Seq.toArray values)
  /// Only allow the matches in the given input for the given regular expression.
  [<CompiledName("WhiteRegex")>]
  [<Obsolete("Use 'allowregex' instead for a more inclusive code base ♥")>]
  [<ExcludeFromCodeCoverage>]
  let whiteregex (regex : Regex) input =
    allowregex regex input
   /// Only allow the matches in the given input for the given regular expression pattern.
  [<CompiledName("AllowMatch")>]
  let allowmatch pattern input =
    allowregex (Regex pattern) input
  /// Only allow the matches in the given input for the given regular expression pattern.
  [<CompiledName("WhiteMatch")>]
  [<Obsolete("Use 'allowmatch' instead for a more inclusive code base ♥")>]
  [<ExcludeFromCodeCoverage>]
  let whitematch pattern input =
    allowmatch pattern input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `allowlist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("AllowList")>]
  let allowlist (valuePatterns : string seq) input =
    let pattern = sprintf "(%s)"  <| String.Join ("|", valuePatterns)
    allowregex (Regex pattern) input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `whitelist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("WhiteList")>]
  [<Obsolete("Use 'allowlist' instead for a more inclusive code base ♥")>]
  [<ExcludeFromCodeCoverage>]
  let whitelist (valuePatterns : string seq) input =
    allowlist valuePatterns input
  /// Removes all the matches in the given input for the given regular expression.
  [<CompiledName("DenyRegex")>]
  let denyregex (regex : Regex) input =
    regex.Replace (input, replacement=String.Empty)
  /// Removes all the matches in the given input for the given regular expression.
  [<CompiledName("BlackRegex")>]
  [<Obsolete("Use 'denyregex' instead for a more inclusive code base ♥")>]
  [<ExcludeFromCodeCoverage>]
  let blackregex (regex : Regex) input =
    denyregex regex input
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<CompiledName("DenyMatch")>]
  let denymatch regex_pattern input = 
    denyregex (Regex regex_pattern) input
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<CompiledName("BlackMatch")>]
  [<Obsolete("Use 'denymatch' instead for a more inclusive code base ♥")>]
  [<ExcludeFromCodeCoverage>]
  let blackmatch regex_pattern input = 
    denymatch regex_pattern input
  /// Replaces the given input with the given key/value sequence where key: value to be replaced, and value: is the replacement for that value.
  [<CompiledName("Replaces")>]
  let replaces replacementByValue input =
    Seq.fold (fun (acc : string) (v : string, rep : string) -> acc.Replace (v, rep)) input replacementByValue
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `denylist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("DenyList")>]
  let denylist (values : string seq) (input : string) =
    let pattern = sprintf "(%s)" <| String.Join ("|", values)
    denymatch pattern input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `blacklist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("BlackList")>]
  [<Obsolete("Use 'denylist' instead for a more inclusive code base ♥")>]
  let blacklist (values : string seq) (input : string) =
    denylist values input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// A.k.a. `denylist`
  [<CompiledName("Removes")>]
  let removes values input = denylist values input
  /// Replace the given value for a given replacement in the given input.
  [<CompiledName("Replace")>]
  let replace value (replacement : string) (input : string) =
    replaces [ value, replacement ] input
  /// Replace all matches in the given input for the regular expression pattern.
  [<CompiledName("RegexReplace")>]
  let regex_replace pattern (replacement : string) input = Regex.Replace (input, pattern, replacement)
  /// Removes all the spaces in the given input.
  [<CompiledName("RemoveSpaces")>]
  let remove_spaces input = replace " " String.Empty input
  /// Removes all the white space characters in the given input.
  [<CompiledName("RemoveWhitespace")>]
  let remove_ws input = regex_replace "\s+" String.Empty input

  let private entities =
    [ "\"", "&quot;"
      "\'", "&#x27;"
      "<", "&lt;"
      ">", "&gt;"
      "/", "&#x2F;"
      "\\", "&#x5C;"
      "`", "&#96;" ]

  /// Escape all the HTML entity characters to their encoded representation.
  /// For example '<' becomes '&lt;'.
  [<CompiledName("Escape")>]
  let escape input =
    replaces entities input
  /// Unescape all the HTML entity charaters to their decoded representation.
  /// For example '&lt;' becomes '<'.
  [<CompiledName("Unescape")>]
  let unescape input =
    let entities = List.map (fun (x, y) -> y, x) entities
    replaces entities input
  /// Removes all leading occurences of a set of specified charachters in the given input.
  [<CompiledName("TrimStart")>]
  let ltrim values (input : string) =
    input.TrimStart (Seq.toArray values)
  /// Removes all trailing occurences of a set of specified charachters in the given input.
  [<CompiledName("TrimEnd")>]
  let rtrim values (input : string) =
    input.TrimEnd (Seq.toArray values)
  /// Removes all leading and trailing occurences of a set of specified charachters in the given input.
  [<CompiledName("Trim")>]
  let trim values (input : string) =
    input.Trim (values : char array)
  /// Removes all leading and trailing white space characters in the given input.
  [<CompiledName("Trim")>]
  let trim_ws (input : string) =
    input.Trim ()
  /// Substring the given input to a maximum length.
  [<CompiledName("Max")>]
  let max length (input : string) =
    if length > input.Length then input
    else input.Substring (0, length)
  /// Adds header if the input doesn't start with one.
  [<CompiledName("Header")>]
  let header value (input : string) =
    if input.StartsWith (value  : string) then input
    else sprintf "%s%s" value input
  /// Adds trailer if the input doesn't end with one.
  [<CompiledName("Trailer")>]
  let trailer value (input : string) =
    if input.EndsWith (value : string) then input
    else sprintf "%s%s" input value
  /// Filters out only European characters in the given input.
  [<CompiledName("European")>]
  let european (input : string) =
    let input = ofNull input
    Regex.Replace (input, "[^A-Za-zÁáĂăÂâÅåÄäǞǟÃãĄąĀāÆæĆćĈĉĊċÇçĎďḐḑĐđÐðÉéÊêĚěËëĖėĘęĒēĞğĜĝĠġĢģĤĥĦħİıÍíÌìÎîÏïĨĩĮįĪīĲĳĴĵĶķĹĺĻļŁłĿŀŃńŇňÑñŅņŊŋÓóÒòÔôÖöȪȫŐőÕõȮȯØøǪǫŌōỌọOEoeĸŘřŔŕŖŗſŚśŜŝŠšŞşṢṣȘșẞßŤťŢţȚțŦŧÚúÙùŬŭÛûŮůÜüŰűŨũŲųŪūŴŵÝýŶŷŸÿȲȳŹźŽžŻżÞþªº]+", String.Empty)
  /// Filters out only ASCII characters in the given input.
  [<CompiledName("ASCII")>]
  let ascii (input : string) =
    Regex.Replace (input, @"[^\x00-\x7F]", String.Empty);
  /// Converts a string to a HTML-encoded string.
  let htmlEncode (input : string) =
    WebUtility.HtmlEncode input
  /// Transforms the input to a lower-case representation.
  [<CompiledName("ToLower")>]
  let lower (input : string) =
    input.ToLower ()
  /// Transforms the input to a upper-case representation.
  [<CompiledName("ToUpper")>]
  let upper (input : string) =
    input.ToUpper ()
  /// Left-padding the input to a specified length with spaces.
  [<CompiledName("PadLeft")>]
  let lpad l (input : string) =
    input.PadLeft l
  /// Left-padding the input to a specified length with a specified character.
  [<CompiledName("PadLeft")>]
  let lpad_char l ch (input : string) =
    input.PadLeft (l, ch)
  /// Right-padding the input to a specified length.
  [<CompiledName("PadRight")>]
  let rpad l (input : string) =
    input.PadRight l
  /// Right-padding the input to a specified length with a specified character.
  [<CompiledName("PadRight")>]
  let rpad_char l ch (input : string) =
    input.PadRight (l, ch)

/// Composition builder for the `Sanitize` module.
[<ExcludeFromCodeCoverage>]
type SanitizeBuilder (input : string option) =
  /// Only allow the matches in the given input for the given regular expression.
  [<CustomOperation("allowregex")>] 
  member __.AllowRegex (state, regex) = Option.map (Sanitize.allowregex regex) state
  /// Only allow the matches in the given input for the given regular expression pattern.
  [<CustomOperation("allowmatch")>]
  member __.AllowMatch (state, pattern) = Option.map (Sanitize.allowmatch pattern) state
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `allowlist ["foo"; "bar[0-9]+"] input`
  [<CustomOperation("allowlist")>] 
  member __.AllowList (state, [<ParamArray>] list : string array) = Option.map (Sanitize.allowlist list) state
  /// Removes all the matches in the given input for the given regular expression.
  [<CustomOperation("denyregex")>]
  member __.DenyRegex (state, regex) = Option.map (Sanitize.denyregex regex) state
  /// Removes all the matches in the given input for the given regular expression.
  [<CustomOperation("denymatch")>]
  member __.DenyMatch (state, pattern) = Option.map (Sanitize.denymatch pattern) state
  /// Replaces the given input with the given key/value sequence where key: value to be replaced, and value: is the replacement for that value.
  [<CustomOperation("replaces")>]
  member __.Replaces (state, replacementByValue) = Option.map (Sanitize.replaces replacementByValue) state
  /// Transforms the input to a lower-case representation.
  [<CustomOperation("lower")>]
  member __.Lower (state) = Option.map (Sanitize.lower) state
  /// Transforms the input to a upper-case representation.
  [<CustomOperation("upper")>]
  member __.Upper (state) = Option.map (Sanitize.upper) state
  /// Left-padding the input to a specified length with a specified character.
  [<CustomOperation("lpad_char")>] 
  member __.PadLeft (state, length, ch) = Option.map (Sanitize.lpad_char length ch) state
  /// Right-padding the input to a specified length.
  [<CustomOperation("rpad")>]
  member __.PadRight (state, length) = Option.map (Sanitize.rpad length) state
  /// Right-padding the input to a specified length with a specified character.
  [<CustomOperation("rpad_char")>]
  member __.PadRight (state, length, ch) = Option.map (Sanitize.rpad_char length ch) state
  [<CustomOperation("removes_ws")>]
  member __.RemovesWhiteSpace (state) = Option.map Sanitize.remove_ws state
  [<CustomOperation("max")>]
  member __.Truncate (state, length) = Option.map (Sanitize.max length) state
  [<CustomOperation("ascii")>]
  member __.Ascii (state) = Option.map Sanitize.ascii state
  member __.Yield (_) = input
  member __.Run (x) : string = Option.toObj x

[<AutoOpen>]
module SanitizeExporure =
  let sanitize x = SanitizeBuilder (Option.ofObj x)

/// Sanitization operations on a string, filtering the untrusted user-input before any parsing, syntax, deserialization, or validation.
[<Extension>]
[<ExcludeFromCodeCoverage>]
type SanitizeExtensions private () =
  /// Safe sanitization extension to only run sanitization on a non-null string input.
  [<Extension>] 
  static member Sanitize (input : string, sanitization : Func<string, string>) = 
    if isNull sanitization then nullArg "sanitization"
    if isNull input then input 
    else sanitization.Invoke input 
  /// Switch to an empty string when the input is 'null'.
  [<Extension>] static member OfNull (input) = Sanitize.ofNull input
  /// Substring the given input to a maximum length.
  [<Extension>] static member Max (input, length) = Sanitize.max length input
  /// Filters out only ASCII characters in the given input.
  [<Extension>] static member ASCII (input) = Sanitize.ascii input
  /// Only allow the matches in the given input for the given regular expression.
  [<Extension>]
  [<Obsolete("Use 'AllowRegex' instead for a more inclusive code base ♥")>]
  static member WhiteRegex (input, regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.allowregex regex input
  /// Only allow the matches in the given input for the given regular expression.
  [<Extension>]
  static member AllowRegex (input, regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.allowregex regex input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `whitelist ["foo"; "bar[0-9]+"] input`
  [<Extension>]
  [<Obsolete("Use 'AllowList' instead for a more inclusive code base ♥")>]
  static member WhiteList (input, [<ParamArray>] values : string array) = 
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in white list"
    Sanitize.allowlist values input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `denylist ["foo"; "bar[0-9]+"] input`
  [<Extension>]
  static member AllowList (input, [<ParamArray>] values : string array) = 
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in white list"
    Sanitize.allowlist values input
  /// Only allow the matches in the given input for the given regular expression pattern.
  [<Extension>]
  [<Obsolete("Use 'AllowMatch' instead for a more inclusive code base ♥")>]
  static member WhiteMatch (input, pattern) =
    if isNull pattern then nullArg "pattern"
    Sanitize.allowmatch pattern input
   /// Only allow the matches in the given input for the given regular expression pattern.
  [<Extension>]
  static member AllowMatch (input, pattern) =
    if isNull pattern then nullArg "pattern"
    Sanitize.allowmatch pattern input
  /// Removes all the matches in the given input for the given regular expression.
  [<Extension>]
  [<Obsolete("Use 'DenyRegex' instead for a more inclusive code base ♥")>]
  static member BlackRegex (input,  regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.denyregex regex input
  /// Removes all the matches in the given input for the given regular expression.
  [<Extension>]
  static member DenyRegex (input,  regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.denyregex regex input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `blacklist ["foo"; "bar[0-9]+"] input`
  [<Extension>]
  [<Obsolete("Use 'DenyList' instead for a more inclusive code base ♥")>]
  static member BlackList (input, [<ParamArray>] values : string array) = 
    if isNull input then nullArg "input"
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in black list"
    Sanitize.denylist values input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `denylist ["foo"; "bar[0-9]+"] input`
  [<Extension>]
  static member DenyList (input, [<ParamArray>] values : string array) = 
    if isNull input then nullArg "input"
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in black list"
    Sanitize.denylist values input
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<Extension>]
  [<Obsolete("Use 'DenyMatch' instead for a more inclusive code base ♥")>]
  static member BlackMatch (input, pattern) = 
    if isNull pattern then nullArg "pattern"
    Sanitize.denymatch pattern input
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<Extension>]
  static member DenyMatch (input, pattern) = 
    if isNull pattern then nullArg "pattern"
    Sanitize.denymatch pattern input
  /// Replaces the given input with the given key/value sequence where key: value to be replaced, and value: is the replacement for that value.
  [<Extension>] 
  static member Replaces (input, replacements : IDictionary<string, string>) = 
    if isNull replacements then nullArg "replacements"
    if Seq.exists (fun (KeyValue (k, v)) -> isNull k || isNull v) replacements
    then invalidArg "replacements" "requires non-null value/replacement pairs"
    let replacements = Seq.map (|KeyValue|) replacements
    Sanitize.replaces replacements input
  /// Replace the given value for a given replacement in the given input.
  [<Extension>]
  static member Replace (input, value, replacement) =
    if isNull value then nullArg "value"
    if isNull replacement then nullArg "replacement"
    Sanitize.replace value replacement input
  /// Replace all matches in the given input for the regular expression pattern.
  [<Extension>] static member RegexReplace (input, pattern, replacement : string) = Regex.Replace (input, pattern, replacement)
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// A.k.a. `denylist`
  [<Extension>] 
  static member Removes (input, [<ParamArray>] values : string array) = 
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in removal list"
    Sanitize.removes values input
  /// Removes all the spaces in the given input.
  [<Extension>] static member RemoveSpaces (input) = Sanitize.remove_spaces input
  /// Removes all the white space characters in the given input.
  [<Extension>] static member RemoveWhitespace (input) = Sanitize.remove_ws input
  /// Escape all the HTML entity characters to their encoded representation.
  /// For example '<' becomes '&lt;'.
  [<Extension>] static member Escape (input) = Sanitize.escape input
  /// Unescape all the HTML entity charaters to their decoded representation.
  /// For example '&lt;' becomes '<'.
  [<Extension>] static member Unescape (input) = Sanitize.unescape input
  /// Adds header if the input doesn't start with one.
  [<Extension>] static member Header (input, value) = Sanitize.header value input
  /// Adds trailer if the input doesn't end with one.
  [<Extension>] static member Trailer (input, value) = Sanitize.trailer value input
  /// Filters out only ASCII characters in the given input.
  [<Extension>] static member Ascii (input) = Sanitize.ascii input
