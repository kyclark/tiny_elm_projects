module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { secret : Int
    , guess : Maybe Int
    , errorMessage : Maybe String
    , numGuesses : Int
    , hasWon : Bool
    }


initialModel =
    { secret = 10
    , guess = Nothing
    , errorMessage = Nothing
    , numGuesses = 0
    , hasWon = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = SetGuess String
    | MakeGuess


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeGuess ->
            let
                ( newError, hasWon ) =
                    case model.guess of
                        Nothing ->
                            ( Nothing, False )

                        Just num ->
                            case compare num model.secret of
                                LT ->
                                    ( Just "Too low", False )

                                GT ->
                                    ( Just "Too high", False )

                                EQ ->
                                    ( Nothing, True )
            in
            ( { model
                | errorMessage = newError
                , hasWon = hasWon
                , numGuesses = model.numGuesses + 1
              }
            , Cmd.none
            )

        SetGuess guess ->
            let
                ( newGuess, newError ) =
                    case String.length guess of
                        0 ->
                            ( Nothing, Nothing )

                        _ ->
                            case String.toInt guess of
                                Just n ->
                                    ( Just n, Nothing )

                                _ ->
                                    ( Nothing
                                    , Just ("\"" ++ guess ++ "\" is not valid")
                                    )
            in
            ( { model | guess = newGuess, errorMessage = newError }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        error =
            case model.errorMessage of
                Just msg ->
                    "Error: " ++ msg

                _ ->
                    ""

        result =
            case model.hasWon of
                True ->
                    "You won!"

                _ ->
                    ""
    in
    div []
        [ h1 [] [ text "Guess the number" ]
        , input [ onInput SetGuess ] []
        , button [ onClick MakeGuess ] [ text "Guess" ]
        , br [] []
        , text ("Num guesses: " ++ String.fromInt model.numGuesses)
        , br [] []
        , text error
        , br [] []
        , text result
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
