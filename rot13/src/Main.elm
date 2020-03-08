module Main exposing (Model, Msg(..), init, initialModel, main, update, view)

import Browser
import Dict exposing (Dict)
import Exts.List exposing (chunk)
import Html exposing (Html, div, h1, img, input, table, td, text, tr)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { message : String
    }


initialModel =
    { message = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage newMessage ->
            ( { model | message = newMessage }, Cmd.none )



--_ ->
--    ( model, Cmd.none )
---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Encryptobot" ]
        , table []
            [ tr []
                [ td [] [ text "What is your secret message?" ]
                , td [] [ input [ onInput UpdateMessage ] [] ]
                ]
            , tr []
                [ td [] [ text "Original message: " ]
                , td [] [ text model.message ]
                ]
            , tr []
                [ td [] [ text "Encrypted message: " ]
                , td [] [ text <| encrypt model.message ]
                ]
            ]
        ]


encrypt : String -> String
encrypt message =
    let
        upper =
            String.toUpper message

        rot13 =
            Dict.fromList
                [ ( 'A', "N" )
                , ( 'B', "O" )
                , ( 'C', "P" )
                , ( 'D', "Q" )
                , ( 'E', "R" )
                , ( 'F', "S" )
                , ( 'G', "T" )
                , ( 'H', "U" )
                , ( 'I', "V" )
                , ( 'J', "W" )
                , ( 'K', "X" )
                , ( 'L', "Y" )
                , ( 'M', "Z" )
                , ( 'N', "A" )
                , ( 'O', "B" )
                , ( 'P', "C" )
                , ( 'Q', "D" )
                , ( 'R', "E" )
                , ( 'S', "F" )
                , ( 'T', "G" )
                , ( 'U', "H" )
                , ( 'V', "I" )
                , ( 'W', "J" )
                , ( 'X', "K" )
                , ( 'Y', "L" )
                , ( 'Z', "M" )
                ]

        newLetter letter =
            case Dict.get letter rot13 of
                Just char ->
                    char

                _ ->
                    ""

        newLetters =
            List.map newLetter (String.toList upper)

        chunked =
            chunk 5 newLetters
    in
    String.join "" (List.concat (List.intersperse [ " " ] chunked))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
