module BIC exposing
    ( BIC, PartyPrefix, CountryCode, PartySuffix, BranchId
    , fromString, Error(..)
    , toString, toString11, toPartyPrefix, toCountyCode, toPartySuffix, toBranchId, toOptionalBranchId
    )

{-| This library is for parsing Business Identifier Codes (BIC) used e.g. in
banking. The implementation is based on [ISO 9362 Fourth edition 2014-12-01](https://www.iso.org/standard/60390.html).


# Definition

@docs BIC, PartyPrefix, CountryCode, PartySuffix, BranchId


# Parsing

@docs fromString, Error


# Common helpers

@docs toString, toString11, toPartyPrefix, toCountyCode, toPartySuffix, toBranchId, toOptionalBranchId

-}

import Iso3166


{-| Represents a Business Identifier Code (BIC) as an opaque type.
-}
type BIC
    = BIC PartyPrefix CountryCode PartySuffix (Maybe BranchId)


{-| Business party prefix (4 alpha-numeric).
-}
type alias PartyPrefix =
    String


{-| CountryCode (ISO 3166). Just an alias to [Iso3166.CountryCode](https://package.elm-lang.org/packages/rl-king/elm-iso3166-country-codes/2.0.0/Iso3166#CountryCode) from `rl-king/elm-iso3166-country-codes`
-}
type alias CountryCode =
    Iso3166.CountryCode


{-| Business party suffix (2 alpha-numeric).
-}
type alias PartySuffix =
    String


{-| Branch identifier (3 alpha-numeric).

The branch code is optional ("XXX" for primary office). Where an eight digit
BIC code is given, it may be assumed that it refers to the primary office.

-}
type alias BranchId =
    String


{-| Convert `BIC` to an either 8 or 11 character `String`, depending on the
availability of the branch code.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toString bic -- "FDDODEMM"

    -- bic == BIC "AXIS" Iso3166.IN "BB" (Just "002")
    toString bic -- "AXISINBB002"

-}
toString : BIC -> String
toString bic =
    let
        ( businessPartyId, optionalBranchId ) =
            convert bic
    in
    businessPartyId ++ Maybe.withDefault "" optionalBranchId


{-| Convert `BIC` to an 11 character `String`, where the branch code will be set
to `"XXX"` if it doesn't exist.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toString11 bic -- "FDDODEMMXXX"

    -- bic == BIC "AXIS" Iso3166.IN "BB" (Just "002")
    toString11 bic -- "AXISINBB002"

-}
toString11 : BIC -> String
toString11 bic =
    let
        ( businessPartyId, optionalBranchId ) =
            convert bic
    in
    businessPartyId ++ Maybe.withDefault "XXX" optionalBranchId


{-| Extract business party prefix from `BIC`.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toPartyPrefix bic -- "FDDO"

-}
toPartyPrefix : BIC -> PartyPrefix
toPartyPrefix (BIC code _ _ _) =
    code


{-| Extract country code from `BIC`.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toCountyCode bic -- Iso3166.DE

-}
toCountyCode : BIC -> CountryCode
toCountyCode (BIC _ code _ _) =
    code


{-| Extract business party suffix code from `BIC`.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toPartySuffix bic -- "MM"

-}
toPartySuffix : BIC -> PartySuffix
toPartySuffix (BIC _ _ code _) =
    code


{-| Extract branch code from `BIC`, using `"XXX"` if not available.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toBranchId bic -- "XXX"

    -- bic == BIC "AXIS" Iso3166.IN "BB" (Just "002")
    toBranchId bic -- "002"

-}
toBranchId : BIC -> BranchId
toBranchId =
    toOptionalBranchId >> Maybe.withDefault "XXX"


{-| Extract branch code from `BIC` if available.

    -- bic == BIC "FDDO" Iso3166.DE "MM" Nothing
    toOptionalBranchId bic -- Nothing

    -- bic == BIC "AXIS" Iso3166.IN "BB" (Just "002")
    toOptionalBranchId bic -- Just "002"

-}
toOptionalBranchId : BIC -> Maybe BranchId
toOptionalBranchId (BIC _ _ _ maybeCode) =
    maybeCode



-- PARSER


{-| Parser errors potentially returned by [`fromString`](#fromString).

    fromString "FDDO" -- Err LengthError

    fromString "FDDODEMMXXX Q" -- Err LengthError

    fromString "FD @! DEMMXXX" -- Err NotAlphaNumeric

    fromString "FDDO QQ MMXXX" -- Err UnknownCountryCode

-}
type Error
    = LengthError
    | NotAlphaNumeric
    | UnknownCountryCode


{-| Try to parse a `String` into a [`BIC`](#BIC). Spaces in that `String` will
be removed and all letters will be upcased.

    fromString "FDDO DE MM" -- Ok (BIC "FDDO" Iso3166.DE "MM" Nothing)

    fromString "FDDO DE MM XXX" -- Ok (BIC "FDDO" Iso3166.DE "MM" Nothing)

    fromString "axisinbb002" -- Ok (BIC "AXIS" Iso3166.IN "BB" (Just "002"))

-}
fromString : String -> Result Error BIC
fromString value =
    let
        cleanValue =
            value
                |> removeSpaces
                |> String.toUpper

        len =
            String.length cleanValue
    in
    if len /= 8 && len /= 11 then
        Err LengthError

    else if not (isAlphaNum cleanValue) then
        Err NotAlphaNumeric

    else
        let
            maybeCountryCode =
                cleanValue
                    |> String.slice 4 6
                    |> String.toLower
                    |> Iso3166.fromAlpha2

            maybeBranchId =
                let
                    branchId =
                        String.slice 8 11 cleanValue
                in
                if branchId == "" || branchId == "XXX" then
                    Nothing

                else
                    Just branchId
        in
        case maybeCountryCode of
            Just countryCode ->
                Ok <|
                    BIC
                        (String.slice 0 4 cleanValue)
                        countryCode
                        (String.slice 6 8 cleanValue)
                        maybeBranchId

            Nothing ->
                Err UnknownCountryCode



-- PARSER HELPER


isAlphaNum : String -> Bool
isAlphaNum =
    String.all Char.isAlphaNum


removeSpaces : String -> String
removeSpaces =
    String.filter <| (/=) ' '



-- MISC HELPER


type alias BusinessPartyId =
    String


convert : BIC -> ( BusinessPartyId, Maybe BranchId )
convert (BIC partyPrefix country partySuffix maybeBranch) =
    let
        businessPartyId =
            String.concat
                [ partyPrefix
                , (Iso3166.toAlpha2 >> String.toUpper) country
                , partySuffix
                ]
    in
    Tuple.pair businessPartyId maybeBranch
