module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, br, div, h1, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)
import Regex



---- MODEL ----


type alias Model =
    { arabic : Maybe String
    , roman : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { arabic = Nothing, roman = Nothing }, Cmd.none )


validRoman =
    Maybe.withDefault Regex.never <|
        Regex.fromString
            "^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$"



---- UPDATE ----


type Msg
    = UpdateArabic String
    | UpdateRoman String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateArabic s ->
            let
                newArabic =
                    if String.isEmpty s then
                        Nothing

                    else
                        Just s
            in
            ( { model | arabic = newArabic }, Cmd.none )

        UpdateRoman s ->
            let
                newRoman =
                    if String.isEmpty s then
                        Nothing

                    else
                        Just s
            in
            ( { model | roman = newRoman }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Roman" ]
        , text "Arabic: "
        , input [ onInput UpdateArabic ] []
        , text (displayRoman model.arabic)
        , br [] []
        , text "Roman: "
        , input [ onInput UpdateRoman ] []
        , text <| displayArabic model.roman
        , br [] []
        ]


displayArabic : Maybe String -> String
displayArabic roman =
    case roman of
        Just val ->
            let
                matches =
                    Regex.find validRoman val
            in
            if List.length matches == 1 then
                case toArabic ( 0, val ) of
                    Just ( arabic, _ ) ->
                        String.fromInt arabic

                    _ ->
                        "Error converting to Arabic"

            else
                "Invalid Roman numeral"

        _ ->
            ""


displayRoman : Maybe String -> String
displayRoman arabic =
    case arabic of
        Just val ->
            case String.toInt val of
                Just num ->
                    case toRoman ( num, "" ) of
                        Just ( _, roman ) ->
                            roman

                        _ ->
                            "Error converting to Roman"

                _ ->
                    "Not an integer"

        Nothing ->
            ""



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


toRoman : ( Int, String ) -> Maybe ( Int, String )
toRoman ( val, roman ) =
    case List.head <| List.filter (\( num, _ ) -> val >= num) romanDivs of
        Just ( num, char ) ->
            toRoman ( val - num, roman ++ char )

        _ ->
            Just ( val, roman )


toArabic : ( Int, String ) -> Maybe ( Int, String )
toArabic ( val, rom ) =
    case
        List.head <|
            List.filter (\( _, s ) -> String.startsWith s rom) romanDivs
    of
        Just ( num, char ) ->
            toArabic ( val + num, String.dropLeft (String.length char) rom )

        _ ->
            Just ( val, rom )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
