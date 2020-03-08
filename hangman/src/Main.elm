module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { word : String
    , isPlaying : Bool
    , guess : String
    , numGuesses : Int
    , message : String
    , maxGuesses : Int
    , gameIsOver : Bool
    , previousGuesses : List Char
    }


initialModel : Model
initialModel =
    { word = ""
    , isPlaying = False
    , guess = ""
    , numGuesses = 0
    , message = ""
    , maxGuesses = 6
    , gameIsOver = False
    , previousGuesses = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Guess
    | UpdateWord String
    | UpdateGuess String
    | TogglePlaying


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess ->
            ( { model
                | numGuesses = model.numGuesses + 1
                , message =
                    if model.word == model.guess then
                        "That's right!"

                    else
                        "Guess again, loser!"
                , gameIsOver =
                    model.word
                        == model.guess
                        || model.numGuesses
                        == model.maxGuesses
              }
            , Cmd.none
            )

        TogglePlaying ->
            ( { model | isPlaying = not model.isPlaying }, Cmd.none )

        UpdateGuess newGuess ->
            let
                guess =
                    if String.length newGuess == 1 then
                        newGuess

                    else
                        model.guess
            in
            ( { model | guess = guess, message = "" }, Cmd.none )

        UpdateWord newWord ->
            ( { model | word = newWord }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model" model
    in
    div []
        [ h1 [] [ text "Hangman" ]
        , inputWord model
        , gamePlay model
        , div [] [ text model.message ]
        ]


gamePlay : Model -> Html Msg
gamePlay model =
    let
        word =
            String.repeat (String.length model.word) " _ "

        imgName =
            "hangman" ++ String.fromInt model.numGuesses ++ ".png"

        base =
            div []
                [ div [] [ text ("Num Guesses: " ++ String.fromInt model.numGuesses) ]
                , div [] [ img [ src ("/img/" ++ imgName) ] [] ]
                , div [] [ text word ]
                , input [ onInput UpdateGuess ] []
                , button [ onClick Guess, disabled model.gameIsOver ] [ text "Guess" ]
                ]

        game =
            if model.isPlaying then
                base

            else
                div [] []
    in
    game


inputWord : Model -> Html Msg
inputWord model =
    if model.isPlaying then
        div [] []

    else
        div []
            [ input [ onInput UpdateWord ] []
            , button [ onClick TogglePlaying ] [ text "Play" ]
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
