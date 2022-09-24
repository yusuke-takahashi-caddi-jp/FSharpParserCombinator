module ParserTestUtils

open Expecto
open Parser

module internal Expect =
    let parseAssert parser input expectedParsed  =
        let (parsed, _) = runFinally parser (InputState.ofString input)
        Expect.equal parsed expectedParsed "parsed value should be same"

    let parseFails parser input =
        Expect.throws (fun () -> runFinally parser (InputState.ofString input) |> ignore)  "this parse should fail"