module Main exposing (..)

import Browser
import Html exposing (Html, br, div, h1, img, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



---- UPDATE ----


type Msg
    = Set String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set newText ->
            ( newText, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "The Howler" ]
        , input [ onInput Set ] []
        , br [] []
        , text <| String.toUpper model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
