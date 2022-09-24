module JsonParserTest

open JsonParser
open ParserTestUtils

open Expecto

let jsonParsersTest =
    testList "json parser tests" [
        test "jBool" {
            let parser = jBool
            Expect.parseAssert parser "true" (JBool true)
            Expect.parseAssert parser "false" (JBool false)
            Expect.parseFails parser "True"
            Expect.parseFails parser "False"
        }
        test "jNull" {
            let parser = jNull
            Expect.parseAssert parser "null" JNull
            Expect.parseFails parser "nul"
            Expect.parseFails parser "Null"
        }
        test "jUnicodeChar" {
            let parser = jUnicodeChar
            Expect.parseAssert parser @"\u0041" 'A'
            Expect.parseFails parser @"\u041"
            Expect.parseFails parser @"\U0041"
        }
        test "jString" {
            let parser = jString
            Expect.parseAssert parser @"""\u0041""" (JString "A")
            Expect.parseAssert parser @"""\u0041\rB""" (JString "A\rB")
            Expect.parseFails parser @"""\X"""
        }
        test "jNumber" {
            let parser = jNumber
            Expect.parseAssert parser "1" (JNumber 1)
            Expect.parseAssert parser "1.0" (JNumber 1.0)
            Expect.parseAssert parser "-1.0" (JNumber -1.0)
            Expect.parseAssert parser "0.1" (JNumber 0.1)
            Expect.parseAssert parser "1.0e-3" (JNumber 1.0e-3)
            Expect.parseFails parser "--1"
            // Expect.parseFails parser "001"
            // Expect.parseFails parser "123."
            // Expect.parseFails parser "1e1"
        }
        test "jArray" {
            let parser = jArray
            Expect.parseAssert
                parser
                "[ 1 ,2  ,  3 ]"
                ([1.0; 2.0;3.0] |> List.map JNumber |> JArray)
        }
        test "jValue" {
            let parser = jValue
            Expect.parseAssert
                parser
                "1"
                (1.0 |> JNumber)
        }
        test  "keyValuePair" {
            let parser = keyValuePair
            Expect.parseAssert
                parser
                //"{ \"key\" : \"value\" }"
                """ "a":1 """
                ("a",(1.0 |> JNumber))
        }
        test  "keyValueMap" {
            let parser = keyValueMap
            Expect.parseAssert
                parser
                //"{ \"key\" : \"value\" }"
                """ "a":1, "b"  :  2 """
                ([("a",1.0 |> JNumber);("b",2.0 |> JNumber)] |> Map.ofList)
        }
        test  "jObject" {
            let parser = jObject
            Expect.parseAssert
                parser
                """{ "a":1, "b"  :  "TEXT" }"""
                ([("a",1.0 |> JNumber);("b","TEXT" |> JString)] |> Map.ofList |> JObject)
            Expect.parseAssert
                parser
                """{ "a":1, "b"  :  {"innerKey": ["innerArray"]} }"""
                (
                    [
                        ("a", 1.0 |> JNumber)
                        (
                            "b",
                            [
                                (
                                    "innerKey",
                                    ["innerArray"] |> List.map JString |> JArray
                                )
                            ] |> Map.ofList |> JObject
                        )
                    ] |> Map.ofList |> JObject
                )
        }
    ]