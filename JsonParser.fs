module JsonParser

// based on bellow documents:
// https://www.json.org/json-en.html
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/

open Parser

[<Literal>]
let private LOG_ENVIRONMENT_VAR_NAME = "JSON_PARSER_LOG"
let private LOG_ENVIRONMENT_VAR_FLAG = "true"

let private ( <??> ) parser label =
    parser
    <?> label
    |> loggingByEnvVar LOG_ENVIRONMENT_VAR_NAME LOG_ENVIRONMENT_VAR_FLAG
type JValue =
  | JString of string
  | JNumber of float
  | JBool   of bool
  | JNull
  | JObject of Map<string, JValue>
  | JArray  of JValue list

let jTrue = pstring "true" >>% JBool true
let jFalse = pstring "false"  >>% JBool false
let jBool = (jTrue <|> jFalse ) <??> "JBool"

let jNull = pstring "null" >>% JNull <??> "JNull"

let jNormalChar = safisfy (fun c -> c <> '\\' && c <> '\"') <??> "normal char"

let jEscapedChar =
    [
        (@"\""", '\"') // QuotationMark
        (@"\\", '\\') // ReverseSolidus
        (@"\/", '/') // Solidus
        (@"\b", '\b') // Backspace
        (@"\f", '\f') // Formfeed
        (@"\n", '\n') // Linefeed
        (@"\r", '\r') // CarriageReturn
    ]
    |> List.map(fun (input, returnChar) ->
        pstring input >>% returnChar
    )
    |> choice
    <??> "excaped char"

let jUnicodeChar =
    let convertHexDigitsToChar str =
        System.Int32.Parse(str,System.Globalization.NumberStyles.HexNumber) |> char
    let fourHexDigits =
        let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])
        manyN 4 hexdigit |>> charListToStr
        
    (pstring @"\u" >>. fourHexDigits)
    |>> convertHexDigitsToChar
    <??> "unicode char"

let jStringParser =
    let doubleQuote = pchar '\"'
    let jsonChar = jNormalChar <|> jEscapedChar <|>jUnicodeChar
    between doubleQuote (many jsonChar) doubleQuote
    |>> charListToStr

let jString = jStringParser |>> JString

let sign =  pchar '-' <??> "sign"
let integerPart =
    let zero = pchar '0' |>> string <??> "zero"
    let nonZeroInt=
        anyOf ['1'..'9'] .>>. many pdigitChar
        |>> fun (d, ds) -> d::ds |> charListToStr
        <??> "nonZeroInt"
    zero <|> nonZeroInt
    <??> "integerPart"

let fractionPart =
    pchar '.' >>. many1 pdigitChar
    |>> fun chars -> '.'::chars |> charListToStr
    <??> "fractionPart"
let exponentPart =
    let e = anyOf ['e';'E']
    let plusMinus = anyOf ['+';'-']
    e >>. plusMinus .>>. many pdigitChar
    |>> (fun (sign, num) ->
        'e'::(sign::num) |> charListToStr
    )
    <??> "exponentPart"

let jNumber =
    let jNumberParser =
        ((opt (sign |>> string)) >>? "")
        .>>. integerPart
        .>>. (opt fractionPart >>? "")
        .>>. (opt exponentPart >>? "")
        |>> flattenTuple4
        |>> (fun (sign, integer, fraction, exponent) ->
            [|sign;integer;fraction;exponent|]
            |> System.String.Concat
            |> float
            |> JNumber
        )
    jNumberParser <??> "JNumber"

let jValue,jValueRef = createParserForwardedToRef<JValue>()

let pComma = pchar ','

let jArray =
    let jvalueArray =
        (sepBy1 (pWhiteSpaces >>. jValue .>> pWhiteSpaces) pComma)
        <??> "JValueArray"

    let emptyArray =
        pWhiteSpaces >>% []
        <??> "emptyArray"

    between (pchar '[') (jvalueArray <|> emptyArray) (pchar ']')
    |>> JArray
    <??> "JArray"

let keyValuePair =
    let pColon = (pchar ':')
    ((pWhiteSpaces >>. jStringParser .>> pWhiteSpaces)
    .>>. ((pWhiteSpaces >>. pColon .>> pWhiteSpaces) >>. jValue .>> pWhiteSpaces))
    |>> fun (jstring, jvalue) -> (jstring, jvalue)
    <??> "keyValuePair"
let keyValueMap =
    (sepBy1 keyValuePair pComma) |>> Map.ofList
    <??> "keyValueMap"

let jObject =
    let emptyMap = pWhiteSpaces >>% Map.empty <?> "emptyMap"
    between (pchar '{') (keyValueMap <|> emptyMap) (pchar '}')
    |>> JObject
    <??> "JObject"

jValueRef :=
    (between pWhiteSpaces (choice [
        jNull
        jString
        jNumber
        jObject
        jArray
        jBool
    ]) pWhiteSpaces)
    <??> "JValue"


let tryParseJson str = tryRunFromStr jObject str 
