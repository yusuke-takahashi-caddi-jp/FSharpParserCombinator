open Expecto
open ParserTest
open JsonParserTest
open JsonParser

[<EntryPoint>]
let main args =
    // runTestsWithCLIArgs [] args parserCombinatorsTest
    // |> ignore
    // runTestsWithCLIArgs [] args jsonParsersTest
    // |> ignore

    let sampleJson = System.IO.File.ReadAllText("sample.json")
    printfn "%A" (tryParseJson sampleJson)

    0