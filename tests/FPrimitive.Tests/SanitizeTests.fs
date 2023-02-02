module SanitizeTests

open System.Text.RegularExpressions
open FsCheck
open Expecto
open FPrimitive
open FPrimitive.Tests
open Fare
open System

module List =
  let union xs ys = List.filter (fun x -> List.contains x ys) xs

module String =
  let alphabet = (['a'..'z'] @ ['A'..'Z']) |> List.map string
  let digit = [0..9]
  let alphanumeric = alphabet @ List.map string digit
  let blanks = 
    [9; 10; 11; 12; 13; 32; 133; 160; 5760; 8192; 8193; 8194; 8195; 8196; 8197; 8198; 8199; 8200; 8201; 8202; 8232; 8233; 8239; 8287; 12288]
    |> List.map Char.ConvertFromUtf32
  let reduce xs = String.Join ("", (xs : string seq))
  let chars xs = (xs : string).ToCharArray() |> List.ofArray |> List.map string
  let containsNone xs str = List.union xs (chars str) = []

module Gen =
  let alphanumeric = 
    String.alphanumeric
    |> Gen.subListOf
    |> Gen.map String.reduce
  let alphabet =
    String.alphabet
    |> Gen.subListOf
    |> Gen.map (List.map string >> String.reduce)

  let sequenceShuffle gs = gen {
    let! xs = Gen.collect id gs
    return! Gen.shuffle xs |> Gen.map List.ofArray }

[<Tests>]
 let sanitize_tests =
   testList "sanitize" [
     testProperty "empty when null" <| fun x ->
       let actual = Sanitize.ofNull x
       Expect.isNotNull actual "can never be 'null'"
     
     testProperty "allow regex+match" <| fun (NonNull pre_noise) (PositiveInt x) (NonNull post_noise) ->
       (pre_noise <> string x && post_noise <> string x
       && not <| pre_noise.Contains (string x)
       && not <| post_noise.Contains (string x)) ==> lazy
       let input = sprintf "%s%i%s" pre_noise x post_noise
       let actualMatch = sanitize input { allowmatch (string x) }
       Expect.equal actualMatch (string x) "should only allow matches"
       let actualRegex = sanitize input  { allowregex (Regex <| string x) }
       Expect.equal actualRegex (string x) "should only allow matches"
     
     testProperty "allow list" <| fun (NonNull pre_noise) x (NonNull post_noise) ->
       (pre_noise <> string x && post_noise <> string x
        && not <| pre_noise.Contains (string x)
        && not <| post_noise.Contains (string x)) ==> lazy
       let input = sprintf "%s%i%s" pre_noise x post_noise
       let actualParams = sanitize input { allowparams (string x) }
       Expect.equal actualParams (string x) "should only allow in list via params"
       let actualList = sanitize input { allowlist [ string x ] }
       Expect.equal actualList (string x) "should only allow in list via list"

     testProperty "deny regex+match" <| fun (PositiveInt pre_noise) (PositiveInt post_noise) ->
       withGen (Gen.elements ['a'..'z'] |> Gen.map string) <| fun x ->
         let input = sprintf "%i%s%i" pre_noise x post_noise
         let actualMatch = sanitize input { denymatch "[0-9]+" }
         Expect.equal actualMatch x "should remove noise via match"
         let actualRegex = sanitize input { denyregex (Regex "[0-9]+") }
         Expect.equal actualRegex x "should remove noise via match"
     
     testProperty "deny list" <| fun (PositiveInt pre_noise) (NonEmptyString x) (PositiveInt post_noise) ->
       (not <| x.Contains (string pre_noise) 
        && not <| x.Contains (string post_noise)
        && not <| (string pre_noise).Contains (string post_noise)
        && not <| (string post_noise).Contains (string pre_noise)) ==> lazy
       let input = sprintf "%i_%s_%i" pre_noise x post_noise
       let actualParams = sanitize input { denyparams (string pre_noise) (string post_noise) }
       Expect.equal actualParams (sprintf "_%s_" x) "should remove noise via params"
       let actualList = sanitize input { denylist [string pre_noise; string post_noise] }
       Expect.equal actualList (sprintf "_%s_" x) "should remove noise via list"
     
     testProperty "replace" <| fun (NonEmptyString target) (NonEmptyString value) (NonEmptyString replacement) ->
       (target <> value && target <> replacement
        && not <| value.Contains target
        && not <| target.Contains value ) ==> lazy
       let input = sprintf "%s%s" target value
       let actual = sanitize input { replace value replacement }
       Expect.equal actual (sprintf "%s%s" target replacement) "should replace value with replacement"
     
     testProperty "replaces" <| fun (NonEmptyString str) (NonEmptyString key) (NonEmptyString value) ->
       let expected = str.Replace (key, value)
       let actualFromComp = sanitize str { replaces [ key, value ] }
       Expect.equal actualFromComp expected "sanitize replaces should be same as string.replace via composition"
       let actualFromExtension = str.Replaces (dict [ key, value ])
       Expect.equal actualFromExtension expected "sanitize replaces should be same as string.replace via extension"

     testProperty "ascii" <| fun (PositiveInt x) ->
       let input = sprintf "%i👍" x
       let actual = sanitize input { ascii }
       Expect.equal actual (string x) "should only get ASCII characters"
     
     testProperty "all ascii chars allowed" <| fun () ->
       let input = Xeger("[\x00-\x7F]+").Generate()
       let actual = sanitize input { ascii }
       Expect.equal actual input (sprintf "should allow all ASCII characters, left: %A, right: %A" actual input)
     
     testProperty "left padding" <| fun (NonNull str) (PositiveInt l) ->
       Sanitize.lpad l str = str.PadLeft l
     
     testProperty "right padding" <| fun (NonNull str) (PositiveInt l) ->
       sanitize str { rpad l } = str.PadRight l
     
     testProperty "left padding char" <| fun (NonNull str) (PositiveInt l) ch ->
       sanitize str { lpad_char l ch } = str.PadLeft (l, ch)
     
     testProperty "right padding char" <| fun (NonNull str) (PositiveInt l) ch ->
       sanitize str { rpad_char l ch } = str.PadRight (l, ch)
     
     testProperty "upper" <| fun (NonNull str) ->
       sanitize str { upper } = str.ToUpper ()
     
     testProperty "lower" <| fun (NonNull str) ->
       sanitize str { lower } = str.ToLower ()
     
     testProperty "html encode" <| fun (NonNull str) ->
       sanitize str { htmlEncode } = System.Net.WebUtility.HtmlEncode str

     testProperty "header" <| fun (NonNull str) (NonEmptyString value) ->
       let result = sanitize str { header value }
       Expect.isTrue (result.StartsWith value) "sanitize header with value should always start with value"

     testProperty "trailer" <| fun (NonNull str) (NonEmptyString value) ->
       let result = sanitize str { trailer value }
       Expect.stringEnds result value "sanitize trailer with value should always ends with value"

     testProperty "european" <| fun (NonNull str) ->
       let allowed = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzÁáĂăÂâÅåÄäǞǟÃãĄąĀāÆæĆćĈĉĊċÇçĎďḐḑĐđÐðÉéÊêĚěËëĖėĘęĒēĞğĜĝĠġĢģĤĥĦħİıÍíÌìÎîÏïĨĩĮįĪīĲĳĴĵĶķĹĺĻļŁłĿŀŃńŇňÑñŅņŊŋÓóÒòÔôÖöȪȫŐőÕõȮȯØøǪǫŌōỌọOEoeĸŘřŔŕŖŗſŚśŜŝŠšŞşṢṣȘșẞßŤťŢţȚțŦŧÚúÙùŬŭÛûŮůÜüŰűŨũŲųŪūŴŵÝýŶŷŸÿȲȳŹźŽžŻżÞþªº"
       let result = sanitize str { european }
       Expect.all result allowed.Contains "european sanitize should only contain european characters"
   
     testProperty "removes = denylist" <| fun (NonNull str) (values : string array) ->
       withGen (Gen.subListOf ['a'..'z'] |> Gen.map (List.map string)) <| fun values ->
         sanitize str { removes values } = sanitize str { denylist values}
   
     testProperty "regex replace" <| fun x (replacement : int) ->
       let g = 
         Gen.sequenceShuffle [ Gen.alphabet; Gen.constant x; Gen.constant (string replacement) ]
         |> Gen.map String.reduce
       withGen g <| fun str ->
         sanitize str { regex_replace "[a-zA-Z]+" (string replacement) }
         |> String.containsNone String.alphabet

     testProperty "remove spaces" <| fun (NonNull str) ->
       sanitize str { remove_spaces } |> String.forall ((<>) ' ')
   
     testProperty "remove ws" <| fun (NonNull str) ->
       sanitize str { remove_ws } |> String.containsNone String.blanks

     testProperty "escape" <| fun (NonNull str) ->
       sanitize str { escape } |> String.containsNone [ "\""; "\'"; "<"; ">"; "/"; "\\"; "`" ]
   
     testProperty "unescape" <| fun (NonNull str) ->
       sanitize str { unescape } |> String.containsNone [ "&quot;"; "&#x27;"; "&lt;"; "&gt;"; "&#x2F;"; "&#x5C;"; "&#96;" ]

     testProperty "ltrim" <| fun (NonNull str) ch ->
       let result = sanitize str { ltrim [ ch ] }
       not <| result.StartsWith ch

     testProperty "rtrim" <| fun (NonNull str) ch ->
       let result = sanitize str { rtrim [ ch ] }
       not <| result.EndsWith ch

     testProperty "trim" <| fun (NonNull str) ch ->
       let result = sanitize str { trim [ ch ] }
       not <| result.StartsWith ch |@ "starts with"
       .&. (not <| result.EndsWith ch |@ "ends with")

     testProperty "trim ws" <| fun (NonEmptyString str) ->
       let result = sanitize str { trim_ws }
       Seq.tryHead result |> Option.map (string >> String.containsNone String.blanks) |> Option.defaultValue true |@ "head"
       .&. (Seq.tryLast result |> Option.map (string >> String.containsNone String.blanks) |> Option.defaultValue true |@ "last")
   
     testProperty "max" <| fun (NonNull str) ->
       let genLength = Gen.choose (String.length str, Int32.MaxValue)
       withGen genLength <| fun l ->
         sanitize str { max l } |> String.length <= l
   ]