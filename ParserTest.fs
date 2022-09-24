module ParserTest

open Expecto
open Parser
open ParserTestUtils

let parserCombinatorsTest =
    testList "parser tests" [
        test "pchar" {
            let parser = pchar 'A'
            Expect.parseAssert parser "ABC" 'A'
            Expect.parseFails parser "ZBC"
            Expect.parseFails parser ""
        }
        test "andThen" {
            let parser =  pchar 'A' .>>. pchar 'B'
            Expect.parseAssert
                parser "ABC"
                ('A', 'B')
            Expect.parseFails parser "ZBC"
            Expect.parseFails parser "AZC"
        }
        test "orElse" {
            let parser =  pchar 'A' <|> pchar 'B'
            Expect.parseAssert
                parser "ABC"
                'A'
            Expect.parseAssert
                parser "BAC"
                'B'
            Expect.parseFails parser "ZBC"
        }
        test "andTHen, orElse" {
            let parser =
                pchar 'A'
                .>>. (pchar 'B'<|> pchar 'C')
            Expect.parseAssert
                parser "ABC"
                ('A','B')
            Expect.parseAssert
                parser "ACB"
                ('A','C')
            Expect.parseFails parser "BAC"
        }
        test "choice" {
            let parser =
                choice [
                    pchar 'A'
                    pchar 'B'
                    pchar 'C'
                ]
            Expect.parseAssert
                parser "ABC"
                'A'
            Expect.parseAssert
                parser "BCA"
                'B'
            Expect.parseAssert
                parser "CAB"
                'C'
            Expect.parseFails parser "DABC"
        }
        test "anyOf" {
            let parser = anyOf ['A';'B';'C']
            Expect.parseAssert
                parser "AZ"
                'A'
            Expect.parseAssert
                parser "BZ"
                'B'
            Expect.parseFails parser "ZZ"
        }
        test "map" {
            let parser =
                (pchar 'A' .>>. pchar 'B')
                |>> fun (a,b) -> [a;b]
            Expect.parseAssert
                parser "ABC"
                ['A';'B']
        }
        test "lift2" {
            let parser =
                lift2 (fun c1 c2 -> [c1 ; c2] |> charListToStr) (pchar 'A') (pchar 'B')
            Expect.parseAssert
                parser "ABC"
                ("AB")
        }
        test "sequence" {
            let parser =
                "ABC"
                |> Seq.map pchar
                |> Seq.toList
                |> sequence
            Expect.parseAssert
                parser "ABCD"
                (['A';'B';'C'])
        }
        test "pstring" {
            let parser = pstring "ABC"
            Expect.parseAssert
                parser "ABCD"
                ("ABC")
        }
        test "many0" {
            let parser = many (pchar 'A')
            Expect.parseAssert
                parser "AAC"
                ['A';'A']
            Expect.parseAssert
                parser "ZZZ"
                []
        }
        test "many1" {
            let parser = many1 (pchar 'A')
            Expect.parseAssert
                parser "AAC"
                ['A';'A']
            Expect.parseFails parser "ZZ"
        }
        test "pUint" {
            let parser = pUint
            Expect.parseAssert
                parser "1234AA"
                1234
            Expect.parseFails parser "ZZ"
        }
        test "opt" {
            let parser = opt (pchar 'A')
            Expect.parseAssert
                parser "AA"
                (Some 'A')
            Expect.parseAssert
                parser "ZZ"
                None
        }
        test "opt2" {
            let parser = opt (pchar '-')
            Expect.parseAssert
                parser "-1"
                (Some '-')
            Expect.parseAssert
                parser "123"
                None
        }
        test "pdigit" {
            let parser = pdigit
            Expect.parseAssert
                parser "123"
                1 
            Expect.parseFails parser "-1"
        }
        test "pint" {
            let parser = pint
            Expect.parseAssert
                parser "1234A"
                1234
            Expect.parseAssert
                parser "-1234A"
                -1234
            Expect.parseFails parser "A"
        }
        test ".>>" {
            let parser = pchar 'A' .>> pchar 'B'
            Expect.parseAssert
                parser "ABC"
                'A'
        }
        test ">>." {
            let parser = pchar 'A' >>. pchar 'B'
            Expect.parseAssert
                parser "ABC"
                'B'
        }
        test "between" {
            let parser = between (pchar '[') pint (pchar ']')
            Expect.parseAssert
                parser "[1]"
                1
        }
        test "sepBy1" {
            let pcomma = pchar ','
            let parser = sepBy1 pint pcomma
            Expect.parseAssert
                parser "1,2,3,4,5"
                [1;2;3;4;5]
            Expect.parseAssert
                (between (pchar '[') parser (pchar ']')) "[1,2,3,4,5]"
                [1;2;3;4;5]
        }
        test "sepBy" {
            let pcomma = pchar ','
            let parser = sepBy pint pcomma
            Expect.parseAssert
                parser "1,2,3,4,5"
                [1;2;3;4;5]
            Expect.parseAssert
                parser ""
                []
        }
        test "whiteSpace" {
            let parser = pWhiteSpace
            Expect.parseAssert
                parser " "
                ' '
            Expect.parseAssert
                parser "\r"
                '\r'
            Expect.parseAssert
                parser "\n"
                '\n'
            Expect.parseAssert
                parser "\t"
                '\t'
            Expect.parseFails parser "A"
        }
        test "manyN" {
            let parser = (manyN 3 (anyOf ['A'..'C']))
            Expect.parseAssert
                parser "ABCD"
                ['A';'B';'C']
        }
        test "pDefault" {
            let parser = (opt (pchar 'A') >>? 'X')
            Expect.parseAssert
                parser "A"
                'A'
            Expect.parseAssert
                parser "B"
                'X'
        }
        test "pWhiteSpaces" {
            let parser =
                pdigit .>>. pWhiteSpaces .>>. pchar ','
                |>> flattenTuple3
            Expect.parseAssert
                parser "1,"
                (1, [], ',')
        }
    ]