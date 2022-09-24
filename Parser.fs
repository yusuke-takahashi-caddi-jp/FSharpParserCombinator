module Parser

open System


// this is necessary for using shift_jis encoding in .Net Core
System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
let private charToBytes = Array.singleton >> System.Text.Encoding.GetEncoding("shift_jis").GetByteCount

type Position = {
    Line: int
    Column: int
}

let makePositionDescription line column = $"at Line {line + 1} Column {column + 1}"
let visualizedPosition (inputLine: string) column =
    let indent =
        let indentCount =
            inputLine
            |> Seq.take column
            |> Seq.sumBy charToBytes
        Array.replicate indentCount ' '
        |> System.String.Concat
    let waveLine =
        if column < inputLine.Length then
            let waveCount = charToBytes inputLine[column]
            Array.replicate waveCount '^'
            |> System.String.Concat
        else
            ""
    $"{inputLine}\n{indent}{waveLine}"

[<RequireQualifiedAccess>]
module Position =
    let initialPos = {Line=0; Column=0}
    let incrLine pos = {Line=pos.Line + 1; Column=0}
    let incrCol pos = {Line=pos.Line; Column=pos.Column + 1}


type InputState = {
    Position: Position
    Lines: string list
}

[<RequireQualifiedAccess>]
module InputState =
    let ofString (str: string) =
        if String.IsNullOrEmpty(str) then
            {Lines=[]; Position=Position.initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None) |> List.ofArray
            {Lines=lines; Position=Position.initialPos}


    // get currentChar and move position to next
    let nextChar state =
        let {Position=position; Lines=lines} = state
        if position.Line = lines.Length then
            (state, None)
        elif position.Column = lines[position.Line].Length then
            ({state with Position=state.Position |> Position.incrLine}, Some('\n'))
        else
            ({state with Position=state.Position |> Position.incrCol}, Some(lines[position.Line][position.Column]))

    let getRemainingString state =
        let remainingLines =
            state.Lines
            |> List.skip state.Position.Line
        match remainingLines with
        | [] -> ""
        | head::tail ->
            let remainingCurrent =
                head
                |> Seq.skip state.Position.Column
                |> System.String.Concat
            
            System.String.Join('\n', remainingCurrent::tail |> Array.ofList)

    let toPrretyCurrentPosition state =
        let {
            Lines=inputLines
            Position={
                Line=line
                Column=column
            }
        } = state
        if line < inputLines.Length then
            let header = makePositionDescription line column
            let visualizedPos = visualizedPosition inputLines[line] column
            $"{header}\n{visualizedPos}"
        else
            "EOF"


type ParserErrorPosition = {
    Line: int
    Column: int
    CurrentInputLine: string
}

[<RequireQualifiedAccess>]
module ParserErrorPosition =
    let ofState (state: InputState) =
        state.Lines
        |> List.tryItem state.Position.Line
        |> Option.map(fun currentInputLine ->
            {
                Line=state.Position.Line
                Column=state.Position.Column
                CurrentInputLine=currentInputLine
            }
        )

    let toPrettyStr position =
        let {
            Line=line
            Column=column
            CurrentInputLine=inputLine
        } = position
        $"{makePositionDescription line column}\n{visualizedPosition inputLine column}"



type ParserErorr = {
    Label: string
    ErrorMessage: string
    ErrorPosition: ParserErrorPosition option
}

[<RequireQualifiedAccess>]
module ParserError =
    let toPrettyStr error =
        sprintf "Error when try parsing %s: %s\n %s"
            error.Label
            error.ErrorMessage
            (error.ErrorPosition
                |> Option.map ParserErrorPosition.toPrettyStr
                |> Option.defaultValue ""
            )


type ParseResult<'a> =
    | Success of 'a * InputState
    | Failure of ParserErorr

let isSuccess = function
    | Success _ -> true
    | Failure _ -> false

let isFailure r = (isSuccess >> not) r

type Parser<'T> = {
    ParseFn: InputState -> ParseResult<'T>
    Label: string
}

let flattenTuple3 ((v1,v2), v3) = (v1,v2,v3)
let flattenTuple4 (((v1,v2), v3), v4) = (v1,v2,v3,v4)
let flattenTuple5 ((((v1,v2), v3), v4), v5) = (v1,v2,v3,v4,v5)
let flattenTuple6 (((((v1,v2), v3), v4), v5), v6) = (v1,v2,v3,v4,v5,v6)
let flattenTuple7 ((((((v1,v2), v3), v4), v5), v6), v7) = (v1,v2,v3,v4,v5,v6,v7)

let run parser input =
    parser.ParseFn input

let runFinally parser input =
    match run parser input with
    | Success (value, remaining) ->
        value, remaining
    | Failure error ->
        failwith (error |> ParserError.toPrettyStr)

let runFromStr parser input =
    run parser (InputState.ofString input)

let tryRunFromStr parser input =
    match runFromStr parser input with
    | Success (value, _) ->
        Ok value
    | Failure error ->
        Error (error |> ParserError.toPrettyStr)


let printResult result =
    match result with
    | Success (value, _input) ->
        printfn "%A" value
    | Failure error ->
        printfn "%s" (error |> ParserError.toPrettyStr)

let logging parser =
    let parseFn input =
        match run parser input with
        | Success (value, remaining) ->
            printfn $"--- Parser {parser.Label} Success---"
            printfn $"Parsed value: '{value.ToString()}'"
            printfn "Next  position:"
            printfn $"{remaining |> InputState.toPrretyCurrentPosition}"
            Success (value, remaining)
        | Failure error ->
            printfn $"--- Parser {parser.Label} Failed ---"
            printfn $"error: {error |> ParserError.toPrettyStr}"
            Failure error
    {parser with ParseFn = parseFn}

let loggingByPred pred parser  =
    if pred() then
        parser |> logging
    else
        parser

let loggingByEnvVar envKey envVar parser  =
    let specifiedEnvVar = System.Environment.GetEnvironmentVariable(envKey)
    loggingByPred (fun _ -> specifiedEnvVar = envVar) parser

let setLabel parser newLabel =
    let parseFn input =
        match run parser input with
        | Success (value, remaining) ->
            Success (value, remaining)
        | Failure error ->
            Failure {error with Label=newLabel}
    {
        ParseFn=parseFn
        Label=newLabel
    }

let ( <?> ) = setLabel

let charListToStr = Array.ofList >> String

let safisfy pred =
    let label = "safisfy"
    let parseFn input =
        let (remainingInput, currentChar) = input |> InputState.nextChar
        match currentChar with
        | Some c ->
            if pred c then
                (c, remainingInput) |> Success
            else
                {
                    Label=label
                    ErrorMessage=sprintf "Got char '%c'" c
                    ErrorPosition=ParserErrorPosition.ofState input
                }|> Failure
        | None ->
                {
                    Label=label
                    ErrorMessage="No more input."
                    ErrorPosition=None
                }|> Failure
    {
        ParseFn=parseFn
        Label=label
    }

let pchar c = safisfy ((=) c) <?> $"'{c}'"

let bindP f parser =
    let label = $"bindP({parser.Label})"
    let parseFn input =
        match run parser input with
        | Failure e -> Failure e
        | Success (value, remaining) ->
            let newParser = f value
            run newParser remaining
    {
        ParseFn=parseFn
        Label=label
    }

let ( >>= ) p f = bindP f p

let returnP value =
    let label = "returnP"
    let parseFn input = Success(value, input)
    {
        ParseFn=parseFn
        Label=label
    }

let andThen parser1 parser2 =
    parser1 >>= (fun p1result->
        parser2 >>= (fun p2result ->
            returnP (p1result, p2result)
        )
    )

let (.>>.) = andThen

let orElse parser1 parser2 =
    let label = $"orElse({parser1.Label},{parser2.Label})"
    let parseFn input =
        match run parser1 input with
        | Success (result1, remaining1) -> Success (result1, remaining1)
        | Failure _ ->
            match run parser2 input with
                | Success (result2, remaining2) -> Success (result2, remaining2)
                | Failure e -> Failure e
    {
        ParseFn=parseFn
        Label=label
    }

let (<|>) = orElse

let concatStrs f = List.map f >> System.String.Concat

let choice parsers =
    {
        (parsers |> List.reduce orElse)
        with Label = $"""choice of ({parsers |> concatStrs (fun p -> p.Label + ",")})"""
    }

    

let anyOf chars =
    {
        (chars |> List.map pchar |> choice)
        with Label = $"""any of ({chars |> concatStrs(fun c ->  "'" + c.ToString() + "',")})"""
    }

// let mapP f parser =
//     fun input ->
//         match run parser input with
//         | Success (value, remaining) -> Success (value |> f, remaining)
//         | Failure e -> Failure e
//     |> Parser
let mapP f parser =
    {
        (parser >>= (f >> returnP))
        with Label = parser.Label
    }

let (<!>) = mapP

let ( |>> ) x f = mapP f x

// applies the parser p, ignores the result,
// and returns x.
let (>>%) p x = p |>> (fun _ -> x)


let applyP fP xP =
    // create a Parser containing a pair (f,x)
    (fP .>>. xP)
    // map the pair by applying f to x
    |> mapP (fun (f,x) -> f x)

let (<*>) = applyP

let lift2 f p1 p2 = returnP f <*> p1 <*> p2

let startsWith (str:string) (prefix:string) =
    str.StartsWith(prefix)

let startsWithP = lift2 startsWith

let cons head tail = head::tail
let consP head tail = lift2 cons head tail

// // impl by rec
// let rec sequence parserList =
//     match parserList with
//     | [] -> returnP []
//     | head::tail -> consP head (sequence tail)
// impl by list.foldBack
let sequence parserList =
    List.foldBack (fun v acc -> consP v acc) parserList (returnP [])

let pstring (str: string) =
    str
    |> Seq.map pchar
    |> Seq.toList
    |> sequence
    |>> charListToStr
    <?> "string"

let pDefault defaultValue parser =
    parser |>> Option.defaultValue defaultValue

let (>>?) parser defaultValue = pDefault defaultValue parser

let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = run parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure _ ->
        // if parse fails, return empty list
        ([],input)
    | Success (firstValue,inputAfterFirstParse) ->
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)

let many parser =
    let label = $"many({parser.Label})"
    let parseFn input = (parseZeroOrMore parser input) |> Success
    {
        ParseFn=parseFn
        Label=label
    }

let many1 parser =
    let label = $"many1({parser.Label})"
    let parseFn input =
        match run parser input with
        | Success (value, remaining) ->
            let (additionalValue, remaining) = (parseZeroOrMore parser remaining)
            (value::additionalValue, remaining) |> Success
        | Failure e -> Failure e
    {
        ParseFn=parseFn
        Label=label
    }

let manyN c parser =
    List.replicate c parser |> sequence

let pdigitChar = anyOf ['0'..'9'] <?> "pDigitChar"
let pdigit = pdigitChar |>> (string >> Int32.Parse) <?> "pDigit"


let pUint = many1 pdigitChar |>> (Array.ofList >> String >> int) <?> "uint"

let opt parser =  (parser |>> Some) <|> (returnP None)

let pint =
    opt (pchar '-') .>>. pUint
    |>> (fun (minusSign, i) ->
        match minusSign with
        | Some _ -> -i
        | None -> i
    )
    <?> "pint"

let (.>>) p1 p2 = p1 .>>. p2 |>> fst
let (>>.) p1 p2 = p1 .>>. p2 |>> snd

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let sepBy1 p sep =
    p .>>. many (sep >>. p) |>> (fun (head, tail) -> head::tail)

let sepBy p sep =
    sepBy1 p sep <|> returnP []


// match space, tab, caridge return, and linefeed
let pWhiteSpace = safisfy Char.IsWhiteSpace <?> "pWhiteSpace"
let pWhiteSpaces = many pWhiteSpace <?> "pWhiteSpaces"
let pWhiteSpaces1 = many1 pWhiteSpace <?> "pWhiteSpaces1"

// utility for make forward parser ref
let createParserForwardedToRef<'a>() =
    let dummyParser : Parser<'a>=
        let innerFn _ = failwith "unfixed forwarded parser"
        {ParseFn=innerFn; Label="unknown"}

    // mutable pointer to placeholder Parser
    let parserRef = ref dummyParser

    // wrapper Parser
    let innerFn input =
        // forward input to the placeholder
        // (Note: "!" is the deferencing operator)
        run parserRef.Value input
    let wrapperParser = {ParseFn=innerFn; Label="unknown"}

    wrapperParser, parserRef
