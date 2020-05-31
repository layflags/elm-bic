module BICTest exposing (suite)

import BIC exposing (BIC)
import Expect exposing (Expectation)
import Iso3166
import Result.Extra as Result
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BIC"
        [ describe "BIC.fromString"
            [ -- Result.Ok
              test "parses valid BICs from a String" <|
                \_ ->
                    "FDDODEMMXXX"
                        |> BIC.fromString
                        |> Expect.ok
            , test "parses valid short BICs (w/o branch code) from a string" <|
                \_ ->
                    "FDDODEMM"
                        |> BIC.fromString
                        |> Expect.ok
            , test "parses valid BICs from a string with white space" <|
                \_ ->
                    "  FDDO DE MM XXX  "
                        |> BIC.fromString
                        |> Expect.ok
            , test "parses valid BICs from a string with lower case chars" <|
                \_ ->
                    "fddodemmxxx"
                        |> BIC.fromString
                        |> Expect.ok
            , Test.concat <|
                List.map
                    (\bic ->
                        test ("parses " ++ bic ++ " successfully") <|
                            \_ -> Expect.ok (BIC.fromString bic)
                    )
                    [ "CHASUS33" -- JPMORGAN CHASE BANK, N.A.
                    , "BOFAUS3N" -- BANK OF AMERICA, N.A.
                    , "MIDLGB22" -- HSBC BANK PLC
                    , "BARCGB22" -- BARCLAYS BANK PLC
                    , "ABNANL2A" -- ABN AMRO BANK N.V.
                    , "CITIUS33" -- CITIBANK N.A.
                    , "WFBIUS6S" -- WELLS FARGO BANK, N.A.
                    , "NWBKGB2L" -- NATIONAL WESTMINSTER BANK PLC
                    , "COBADEFF" -- COMMERZBANK AG
                    , "BNPAFRPP" -- BNP PARIBAS SA
                    , "POALILIT" -- BANK HAPOALIM B.M.
                    , "LOYDGB2L" -- LLOYDS BANK PLC
                    , "NTSBDEB1" -- N26 BANK GMBH
                    , "DEUTDEDBPAL" -- DEUTSCHE BANK PRIVAT-&GESCHAEFTSKUNDEN AG
                    , "AXISINBB002" -- AXIS BANK LIMITED
                    ]

            -- Result.Err
            , test "fails if string is too short" <|
                \_ ->
                    "FDDODEM"
                        |> BIC.fromString
                        |> expectError BIC.LengthError
            , test "fails if string is longer than 8 but shorter than 11 chars" <|
                \_ ->
                    "FDDODEMMX"
                        |> BIC.fromString
                        |> expectError BIC.LengthError
            , test "fails if string is too long" <|
                \_ ->
                    "FDDODEMMXXX W"
                        |> BIC.fromString
                        |> expectError BIC.LengthError
            , test "fails if string is not alpha-numeric" <|
                \_ ->
                    "FD @! DEMMXXX"
                        |> BIC.fromString
                        |> expectError BIC.NotAlphaNumeric
            , test "fails if string contains an unknown location code" <|
                \_ ->
                    "FDDO QQ MMXXX"
                        |> BIC.fromString
                        |> expectError BIC.UnknownCountryCode
            ]
        , describe "BIC.toString"
            [ test "converts a BIC w/o branch id to an 8 char string" <|
                \_ ->
                    "FDDODEMM"
                        |> BIC.fromString
                        |> Result.map BIC.toString
                        |> expectOkWith "FDDODEMM"
            , test "converts a BIC w/ branch id to an 11 char string" <|
                \_ ->
                    "FDDODEMM123"
                        |> BIC.fromString
                        |> Result.map BIC.toString
                        |> expectOkWith "FDDODEMM123"
            , test "converts a BIC w/ branch id XXX to an 8 char string" <|
                \_ ->
                    "FDDODEMMXXX"
                        |> BIC.fromString
                        |> Result.map BIC.toString
                        |> expectOkWith "FDDODEMM"
            ]
        , describe "BIC.toString11"
            [ test "converts a BIC w/o branch id to an 11 char string" <|
                \_ ->
                    "FDDODEMM"
                        |> BIC.fromString
                        |> Result.map BIC.toString11
                        |> expectOkWith "FDDODEMMXXX"
            , test "converts a BIC w/ branch id to an 11 char string" <|
                \_ ->
                    "FDDODEMM123"
                        |> BIC.fromString
                        |> Result.map BIC.toString11
                        |> expectOkWith "FDDODEMM123"
            , test "converts a BIC w/ branch id XXX to an 11 char string" <|
                \_ ->
                    "FDDODEMMXXX"
                        |> BIC.fromString
                        |> Result.map BIC.toString11
                        |> expectOkWith "FDDODEMMXXX"
            ]
        , describe "BIC.toPartyPrefix"
            [ test "extracts the party prefix from a BIC" <|
                \_ ->
                    "FDDO DEMMXXX"
                        |> BIC.fromString
                        |> Result.map BIC.toPartyPrefix
                        |> expectOkWith "FDDO"
            ]
        , describe "BIC.toCountryCode"
            [ test "extracts the country code from a BIC" <|
                \_ ->
                    "FDDO DE MMXXX"
                        |> BIC.fromString
                        |> Result.map BIC.toCountryCode
                        |> expectOkWith Iso3166.DE
            ]
        , describe "BIC.toPartySuffix"
            [ test "extracts the party suffix from a BIC" <|
                \_ ->
                    "FDDODE MM XXX"
                        |> BIC.fromString
                        |> Result.map BIC.toPartySuffix
                        |> expectOkWith "MM"
            ]
        , describe "BIC.toBranchId"
            [ test "extracts the branch id from a BIC" <|
                \_ ->
                    "FDDODEMM 123"
                        |> BIC.fromString
                        |> Result.map BIC.toBranchId
                        |> expectOkWith "123"
            , test "returns XXX for optional branch ids" <|
                \_ ->
                    "FDDODEMM"
                        |> BIC.fromString
                        |> Result.map BIC.toBranchId
                        |> expectOkWith "XXX"
            , test "returns XXX for optional branch ids - when using XXX" <|
                \_ ->
                    "FDDODEMM XXX"
                        |> BIC.fromString
                        |> Result.map BIC.toBranchId
                        |> expectOkWith "XXX"
            ]
        , describe "BIC.toOptionalBranchId"
            [ test "extracts the branch id from a BIC, if it exists" <|
                \_ ->
                    "FDDODEMM 123"
                        |> BIC.fromString
                        |> Result.map BIC.toOptionalBranchId
                        |> expectOkWith (Just "123")
            , test "returns Nothing for optional branch ids" <|
                \_ ->
                    "FDDODEMM"
                        |> BIC.fromString
                        |> Result.map BIC.toOptionalBranchId
                        |> expectOkWith Nothing
            , test "returns Nothing for optional branch ids - when using XXX" <|
                \_ ->
                    "FDDODEMM XXX"
                        |> BIC.fromString
                        |> Result.map BIC.toOptionalBranchId
                        |> expectOkWith Nothing
            ]
        ]


expectOkWith : a -> Result e a -> Expectation
expectOkWith x r =
    case r of
        Ok y ->
            Expect.equal x y

        Err _ ->
            Expect.fail "expected Result.Err"


expectError : BIC.Error -> Result BIC.Error BIC -> Expectation
expectError error result =
    case result of
        Err err ->
            if error == err then
                Expect.pass

            else
                Expect.fail <|
                    "expected "
                        ++ errorToString error
                        ++ " but got "
                        ++ errorToString err

        Ok _ ->
            Expect.fail "unexpected success"


errorToString : BIC.Error -> String
errorToString error =
    case error of
        BIC.LengthError ->
            "LengthError"

        BIC.NotAlphaNumeric ->
            "NotAlphaNumeric"

        BIC.UnknownCountryCode ->
            "UnknownCountryCode"
