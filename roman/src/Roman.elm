module Roman exposing (toArabic, toRoman)

import Regex


validRoman =
    Maybe.withDefault Regex.never <|
        Regex.fromString
            "^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$"


romanDivs : List ( Int, String )
romanDivs =
    [ ( 1000, "M" )
    , ( 900, "CM" )
    , ( 500, "D" )
    , ( 400, "CD" )
    , ( 100, "C" )
    , ( 90, "XC" )
    , ( 50, "L" )
    , ( 40, "XL" )
    , ( 10, "X" )
    , ( 9, "IX" )
    , ( 5, "V" )
    , ( 4, "IV" )
    , ( 1, "I" )
    ]


toRoman : String -> Result String String
toRoman arabic =
    case String.toInt arabic of
        Just num ->
            if num > 0 && num < 4000 then
                case toRomanHelper ( num, "" ) of
                    Just ( _, roman ) ->
                        Ok roman

                    _ ->
                        Err <|
                            "Error converting value ("
                                ++ String.fromInt num
                                ++ ")"

            else
                Err <|
                    "Number ("
                        ++ String.fromInt num
                        ++ ") must be > 1 and < 4000"

        _ ->
            Err <| "\"" ++ arabic ++ "\" is not an integer"


toRomanHelper : ( Int, String ) -> Maybe ( Int, String )
toRomanHelper ( val, roman ) =
    case List.head <| List.filter (\( num, _ ) -> val >= num) romanDivs of
        Just ( num, char ) ->
            toRomanHelper ( val - num, roman ++ char )

        _ ->
            Just ( val, roman )


toArabic : String -> Result String Int
toArabic roman =
    let
        matches =
            Regex.find validRoman roman
    in
    if String.isEmpty roman then
        Err "Empty string"

    else if List.length matches /= 1 then
        Err <| "Invalid Roman numeral (" ++ roman ++ ")"

    else
        case toArabicHelper ( 0, roman ) of
            Just ( arabic, _ ) ->
                Ok arabic

            _ ->
                Err <| "Error parsing '" ++ roman ++ "'"


toArabicHelper : ( Int, String ) -> Maybe ( Int, String )
toArabicHelper ( val, rom ) =
    case
        List.head <|
            List.filter (\( _, s ) -> String.startsWith s rom) romanDivs
    of
        Just ( num, char ) ->
            toArabicHelper
                ( val + num, String.dropLeft (String.length char) rom )

        _ ->
            Just ( val, rom )



-- toRoman : ( Int, String ) -> ( Int, String )
-- toRoman ( val, roman ) =
--     if val >= 1000 then
--         toRoman ( val - 1000, roman ++ "M" )
--
--     else if val >= 900 then
--         toRoman ( val - 900, roman ++ "CM" )
--
--     else if val >= 500 then
--         toRoman ( val - 500, roman ++ "D" )
--
--     else if val >= 400 then
--         toRoman ( val - 400, roman ++ "CD" )
--
--     else if val >= 100 then
--         toRoman ( val - 100, roman ++ "C" )
--
--     else if val >= 90 then
--         toRoman ( val - 90, roman ++ "XC" )
--
--     else if val >= 50 then
--         toRoman ( val - 50, roman ++ "L" )
--
--     else if val >= 40 then
--         toRoman ( val - 40, roman ++ "XL" )
--
--     else if val >= 10 then
--         toRoman ( val - 10, roman ++ "X" )
--
--     else if val >= 5 then
--         toRoman ( val - 5, roman ++ "V" )
--
--     else if val >= 4 then
--         toRoman ( val - 4, roman ++ "IV" )
--
--     else if val >= 1 then
--         toRoman ( val - 1, roman ++ "I" )
--
--     else
--         ( val, roman )
