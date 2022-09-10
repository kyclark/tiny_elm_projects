module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, br, div, h1, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)
import Roman exposing (toArabic, toRoman)



---- MODEL ----


type alias Model =
    { arabic : Maybe String
    , roman : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { arabic = Nothing, roman = Nothing }, Cmd.none )



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
        , br [] []
        , text (displayRoman model.arabic)
        , br [] []
        , text "Roman: "
        , input [ onInput UpdateRoman ] []
        , br [] []
        , text <| displayArabic model.roman
        , br [] []
        ]


displayArabic : Maybe String -> String
displayArabic roman =
    case roman of
        Just val ->
            case toArabic val of
                Ok arabic ->
                    String.fromInt arabic

                Err err ->
                    err

        _ ->
            ""


displayRoman : Maybe String -> String
displayRoman arabic =
    case arabic of
        Just val ->
            case toRoman val of
                Ok roman ->
                    roman

                Err err ->
                    err

        Nothing ->
            ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
