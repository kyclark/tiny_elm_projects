module Main exposing (..)

import Browser
import Dict exposing (Dict)
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
        Set text ->
            ( text, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Jump The Five" ]
        , input [ onInput Set ] []
        , br [] []
        , text (jumper model)
        ]


jumpTable : Dict String String
jumpTable =
    Dict.fromList
        [ ( "1", "9" )
        , ( "2", "8" )
        , ( "3", "7" )
        , ( "4", "6" )
        , ( "5", "0" )
        , ( "6", "4" )
        , ( "7", "3" )
        , ( "8", "2" )
        , ( "9", "1" )
        , ( "0", "5" )
        ]


jumper : String -> String
jumper text =
    String.join "" <|
        List.map
            (\char ->
                case Dict.get char jumpTable of
                    Just other ->
                        other

                    _ ->
                        char
            )
            (String.split "" text)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
