module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (disabled, size, src, value)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { secretWord : String
    , displayWord : String
    , isPlaying : Bool
    , guess : String
    , numGuesses : Int
    , error : Maybe String
    , message : String
    , maxGuesses : Int
    , gameIsOver : Bool
    , previousGuesses : Set String
    }


initialModel : Model
initialModel =
    { secretWord = "garage"
    , displayWord = "______"
    , isPlaying = True
    , guess = ""
    , numGuesses = 0
    , error = Nothing
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
            let
                indexes =
                    String.indexes model.guess model.secretWord

                sub index =
                    if List.member index indexes then
                        String.slice index (index + 1) model.secretWord

                    else
                        String.slice index (index + 1) model.displayWord

                newDisplay =
                    String.join "" <|
                        List.map sub
                            (List.range 0 (String.length model.secretWord - 1))

                ( newMessage, guessInc ) =
                    if Set.member model.guess model.previousGuesses then
                        ( "You already guessed \""
                            ++ model.guess
                            ++ "\""
                        , 0
                        )

                    else if List.length indexes > 0 then
                        ( "Good guess!", 0 )

                    else
                        ( "Guess again, loser!", 1 )

                newPreviousGuesses =
                    Set.insert model.guess model.previousGuesses

                newNumGuesses =
                    model.numGuesses + guessInc

                noBlanks =
                    not (String.contains "_" newDisplay)

                hitMaxGuesses =
                    newNumGuesses == model.maxGuesses

                newGameIsOver =
                    case ( noBlanks, hitMaxGuesses ) of
                        ( True, True ) ->
                            True

                        _ ->
                            False
            in
            ( { model
                | numGuesses = newNumGuesses
                , message = newMessage
                , displayWord = newDisplay
                , gameIsOver = newGameIsOver
                , previousGuesses = newPreviousGuesses
              }
            , Cmd.none
            )

        TogglePlaying ->
            ( { model | isPlaying = not model.isPlaying }, Cmd.none )

        UpdateGuess guess ->
            let
                ( newGuess, newError ) =
                    if String.length guess /= 1 then
                        ( model.guess, Just "Guess must be one character" )

                    else
                        ( guess, Nothing )
            in
            ( { model
                | guess = newGuess
                , error = newError
              }
            , Cmd.none
            )

        UpdateWord newWord ->
            ( { model
                | secretWord = newWord
                , displayWord = String.repeat (String.length newWord) "_"
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hangman" ]
        , inputWord model
        , gamePlay model
        , showPreviousGuesses model.previousGuesses
        , div [] [ text model.message ]
        ]


showPreviousGuesses : Set String -> Html msg
showPreviousGuesses guesses =
    case Set.isEmpty guesses of
        True ->
            div [] []

        _ ->
            div []
                [ text "Previous Guesses: "
                , text <| String.join ", " <| Set.toList guesses
                ]


gamePlay : Model -> Html Msg
gamePlay model =
    let
        imgName =
            "hangman" ++ String.fromInt model.numGuesses ++ ".png"

        canGuess =
            case ( model.error, String.length model.guess, model.gameIsOver ) of
                ( Nothing, 1, False ) ->
                    True

                _ ->
                    False

        displayWord =
            String.join " " <|
                List.map String.fromChar <|
                    String.toList model.displayWord

        base =
            div []
                [ div []
                    [ text
                        ("Num Guesses: "
                            ++ String.fromInt model.numGuesses
                        )
                    ]
                , div [] [ img [ src ("/img/" ++ imgName) ] [] ]
                , div [] [ text displayWord ]
                , text "Type a letter to guess: "
                , input [ size 1, onInput UpdateGuess ] []
                , button
                    [ onClick Guess, disabled (not canGuess) ]
                    [ text "Guess" ]
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
