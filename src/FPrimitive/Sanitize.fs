namespace FPrimitive

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open System.Runtime.CompilerServices

/// Sanitization operations on a string, filtering the untrusted user-input before any parsing, syntax, deserialization, or validation.
module Sanitize =
  /// Switch to an empty string when the input is 'null'.
  [<CompiledName("EmptyWhenNull")>]
  let empty_when_null input =
    if isNull input then String.Empty else input
  /// Only allow the matches in the given input for the given regular expression.
  [<CompiledName("WhiteRegex")>]
  let whiteregex (regex : Regex) input =
    let input = empty_when_null input
    let matches = regex.Matches input
    let values = seq { for m in matches do yield m.Value }
    String.Join (String.Empty, Seq.toArray values)
  /// Only allow the matches in the given input for the given regular expression pattern.
  [<CompiledName("WhiteMatch")>]
  let whitematch pattern input =
    whiteregex (Regex pattern) input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `whitelist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("WhiteList")>]
  let whitelist (valuePatterns : string seq) input =
    let pattern = sprintf "(%s)"  <| String.Join ("|", valuePatterns)
    whiteregex (Regex pattern) input
  /// Removes all the matches in the given input for the given regular expression.
  [<CompiledName("BlackRegex")>]
  let blackregex (regex : Regex) input =
    let input = empty_when_null input
    regex.Replace (input, replacement=String.Empty)
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<CompiledName("BlackMatch")>]
  let blackmatch regex_pattern input = 
    blackregex (Regex regex_pattern) input
  /// Replaces the given input with the given key/value sequence where key: value to be replaced, and value: is the replacement for that value.
  [<CompiledName("Replaces")>]
  let replaces replacementByValue input =
    let input = empty_when_null input
    Seq.fold (fun (acc : string) (v : string, rep : string) -> acc.Replace (v, rep)) input replacementByValue
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `blacklist ["foo"; "bar[0-9]+"] input`
  [<CompiledName("BlackList")>]
  let blacklist (values : string seq) (input : string) =
    let pattern = sprintf "(%s)" <| String.Join ("|", values)
    blackmatch pattern input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// A.k.a. `blacklist`
  [<CompiledName("Removes")>]
  let removes values input = blacklist values input
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
    let input = empty_when_null input
    input.TrimStart (Seq.toArray values)
  /// Removes all trailing occurences of a set of specified charachters in the given input.
  [<CompiledName("TrimEnd")>]
  let rtrim values (input : string) =
    let input = empty_when_null input
    input.TrimEnd (Seq.toArray values)
  /// Removes all leading and trailing occurences of a set of specified charachters in the given input.
  [<CompiledName("Trim")>]
  let trim values (input : string) =
    let input = empty_when_null input
    input.Trim (values)
  /// Removes all leading and trailing white space characters in the given input.
  [<CompiledName("Trim")>]
  let trim_ws (input : string) =
    let input = empty_when_null input
    input.Trim ()
  /// Substring the given input to a maximum length.
  [<CompiledName("Max")>]
  let max length (input : string) =
    let input = empty_when_null input
    if length > input.Length then input
    else input.Substring (0, length)
  /// Adds header if the input doesn't start with one.
  [<CompiledName("Header")>]
  let header value (input : string) =
    let input = empty_when_null input
    if input.StartsWith value then input
    else sprintf "%s%s" value input
  /// Adds trailer if the input doesn't end with one.
  [<CompiledName("Trailer")>]
  let trailer value (input : string) =
    let input = empty_when_null input
    if input.EndsWith value then input
    else sprintf "%s%s" input value
  /// Filters out only ASCII characters in the given input.
  [<CompiledName("ASCII")>]
  let ascii (input : string) =
    let input = empty_when_null input
    Regex.Replace (input, @"[^\u0020-\u007E]", String.Empty);
  /// Transforms the input to a lower-case representation.
  [<CompiledName("ToLower")>]
  let lower (input : string) =
    let input = empty_when_null input
    input.ToLower ()
  /// Transforms the input to a upper-case representation.
  [<CompiledName("ToUpper")>]
  let upper (input : string) =
    let input = empty_when_null input
    input.ToUpper ()
  /// Left-padding the input to a specified length with spaces.
  [<CompiledName("PadLeft")>]
  let lpad l input =
    let input = empty_when_null input
    input.PadLeft l
  /// Left-padding the input to a specified length with a specified character.
  [<CompiledName("PadLeft")>]
  let lpad_char l ch input =
    let input = empty_when_null input
    input.PadLeft (l, ch)
  /// Right-padding the input to a specified length.
  [<CompiledName("PadRight")>]
  let rpad l input =
    let input = empty_when_null input
    input.PadRight l
  /// Right-padding the input to a specified length with a specified character.
  [<CompiledName("PadRight")>]
  let rpad_char l ch input =
    let input = empty_when_null input
    input.PadRight (l, ch)

/// Sanitization operations on a string, filtering the untrusted user-input before any parsing, syntax, deserialization, or validation.
[<Extension>]
type SanitizeExtensions private () =
  /// Switch to an empty string when the input is 'null'.
  [<Extension>] static member EmptyWhenNull (input) = Sanitize.empty_when_null input
  /// Substring the given input to a maximum length.
  [<Extension>] static member Max (input, length) = Sanitize.max length input
  /// Filters out only ASCII characters in the given input.
  [<Extension>] static member ASCII (input) = Sanitize.ascii input
  /// Only allow the matches in the given input for the given regular expression.
  [<Extension>] 
  static member WhiteRegex (input, regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.whiteregex regex input
  /// Only allow the matches in the input for the given text regular expression patterns.
  /// For example: `whitelist ["foo"; "bar[0-9]+"] input`
  [<Extension>] 
  static member WhiteList (input, [<ParamArray>] values : string array) = 
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in white list"
    Sanitize.whitelist values input
  /// Only allow the matches in the given input for the given regular expression pattern.
  [<Extension>]
  static member WhiteMatch (input, pattern) =
    if isNull pattern then nullArg "pattern"
    Sanitize.whitematch pattern input
  /// Removes all the matches in the given input for the given regular expression.
  [<Extension>] 
  static member BlackRegex (input,  regex) = 
    if isNull regex then nullArg "regex"
    Sanitize.blackregex regex input
  /// Removes all the matches in the given input for gien the text regular expression patterns.
  /// For example: `blacklist ["foo"; "bar[0-9]+"] input`
  [<Extension>] 
  static member BlackList (input, [<ParamArray>] values : string array) = 
    if isNull input then nullArg "input"
    if isNull values then nullArg "values"
    if Seq.exists isNull values then invalidArg "values" "cannot have a 'null' value in black list"
    Sanitize.blacklist values input
  /// Removes all the matches in the given input for the given regular expression pattern.
  [<Extension>] 
  static member BlackMatch (input, pattern) = 
    if isNull pattern then nullArg "pattern"
    Sanitize.blackmatch pattern input
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
  /// A.k.a. `blacklist`
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