module Main exposing (Model, Msg(..), init, initialModel, main, update, view)

import Browser
import Dict exposing (Dict)
import Exts.List exposing (chunk)
import Html exposing (Html, div, h1, img, input, table, td, text, textarea, tr)
import Html.Attributes exposing (align, cols, placeholder, rows, size, src)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { message : String
    , chunkSize : Int
    }


initialModel =
    { message = ""
    , chunkSize = 5
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateMessage String
    | UpdateChunkSize String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage newMessage ->
            ( { model | message = newMessage }, Cmd.none )

        UpdateChunkSize newChunkSize ->
            case String.toInt newChunkSize of
                Just n ->
                    case n > 0 of
                        True ->
                            ( { model | chunkSize = n }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Encryptobot" ]
        , table []
            [ tr []
                [ td [ align "right" ] [ text "Secret message:" ]
                , td
                    [ align "left" ]
                    [ textarea [ cols 50, rows 20, onInput UpdateMessage ] [] ]
                ]
            , tr []
                [ td [ align "right" ] [ text "Chunk size:" ]
                , td [ align "left" ]
                    [ input
                        [ onInput UpdateChunkSize
                        , placeholder (String.fromInt model.chunkSize)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [ align "right" ] [ text "Original message: " ]
                , td [ align "left" ] [ text model.message ]
                ]
            , tr []
                [ td [ align "right" ] [ text "Encrypted message: " ]
                , td [ align "left" ] [ text <| encrypt model ]
                ]
            ]
        ]


encrypt : Model -> String
encrypt model =
    let
        numToStr =
            Dict.fromList
                [ ( '0', "ZERO" )
                , ( '1', "ONE" )
                , ( '2', "TWO" )
                , ( '3', "THREE" )
                , ( '4', "FOUR" )
                , ( '5', "FIVE" )
                , ( '6', "SIX" )
                , ( '7', "SEVEN" )
                , ( '8', "EIGHT" )
                , ( '9', "NINE" )
                ]

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

        upper =
            String.toUpper model.message

        convertNum char =
            case Dict.get char numToStr of
                Just str ->
                    String.toList str

                _ ->
                    [ char ]

        convertedNums =
            List.concat (List.map convertNum (String.toList upper))

        newLetter letter =
            Dict.get letter rot13

        newLetters =
            List.filterMap newLetter convertedNums

        chunked =
            List.map (\c -> String.join "" c) <|
                chunk model.chunkSize newLetters
    in
    String.join " " chunked



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
