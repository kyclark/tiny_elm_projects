module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (disabled, size, src, value)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { word : String
    , isPlaying : Bool
    , guess : String
    , numGuesses : Int
    , message : String
    , maxGuesses : Int
    , gameIsOver : Bool
    , previousGuesses : Set String
    }


initialModel : Model
initialModel =
    { word = "garage"
    , isPlaying = True
    , guess = ""
    , numGuesses = 0
    , message = ""
    , maxGuesses = 6
    , gameIsOver = False
    , previousGuesses = Set.empty
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
                _ =
                    Debug.log "newGuess" newGuess

                _ =
                    Debug.log "model" model

                --guess =
                --    if String.length newGuess == 1 then
                --        newGuess
                --    else
                --        model.guess
                newMessage =
                    if Set.member newGuess model.previousGuesses then
                        "You already guessed \""
                            ++ newGuess
                            ++ "\""

                    else
                        ""

                newPrevious =
                    Set.insert newGuess model.previousGuesses
            in
            ( { model
                | guess = newGuess
                , message = newMessage
                , previousGuesses = newPrevious
              }
            , Cmd.none
            )

        UpdateWord newWord ->
            ( { model | word = newWord }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    --let
    --    _ =
    --        Debug.log "model" model
    --in
    div []
        [ h1 [] [ text "Hangman" ]
        , inputWord model
        , gamePlay model
        , showPreviousGuesses model.previousGuesses
        , div [] [ text model.message ]
        ]


showPreviousGuesses : Set String -> Html Msg
showPreviousGuesses guesses =
    let
        previousGuesses =
            Set.toList guesses

        mkGuess guess =
            li [] [ text guess ]
    in
    case List.length previousGuesses of
        0 ->
            div [] []

        _ ->
            div []
                [ text "Previous Guesses: "
                , ul [] (List.map mkGuess previousGuesses)
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
                , text "Type a letter to guess: "
                , input [ size 1, onInput UpdateGuess ] []

                --, button [ onClick Guess, disabled model.gameIsOver ] [ text "Guess" ]
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
            [ text "Give me a word to guess: "
            , input [ onInput UpdateWord, value "" ] []
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
