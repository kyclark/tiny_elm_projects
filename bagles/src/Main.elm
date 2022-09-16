module Main exposing (..)

import Bagles exposing (bagles)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (Html, br, button, div, h1, img, input, text)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick, onInput)
import Random



---- MODEL ----


type alias Model =
    { rng : Random.Generator Int
    , secret : String
    , guess : String
    , numGuesses : Int
    , hasWon : Bool
    , message : String
    , numWins : Int
    }


initialModel =
    { rng = Random.int 100 999
    , secret = ""
    , guess = ""
    , numGuesses = 0
    , hasWon = False
    , message = ""
    , numWins = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ newGame initialModel.rng ] )


newGame : Random.Generator Int -> Cmd Msg
newGame rng =
    Random.generate NewGame rng



---- UPDATE ----


type Msg
    = Evaluate
    | NewGame Int
    | SetGuess String
    | StartOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Evaluate ->
            let
                ( hasWon, newMessage ) =
                    case bagles model.secret model.guess of
                        Ok message ->
                            ( True, message )

                        Err message ->
                            ( False, message )

                numWins =
                    model.numWins
                        + (if hasWon then
                            1

                           else
                            0
                          )
            in
            ( { model
                | hasWon = hasWon
                , message = newMessage
                , numWins = numWins
                , numGuesses = model.numGuesses + 1
              }
            , Cmd.none
            )

        NewGame newSecret ->
            ( { model | secret = String.fromInt newSecret }, Cmd.none )

        SetGuess newGuess ->
            ( { model | guess = newGuess }
            , Cmd.none
            )

        StartOver ->
            ( { initialModel | numWins = model.numWins }
            , newGame model.rng
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , h1 [] [ text "Bagles (Guess a 3-digit number)" ]
        , text model.secret
        , br [] []
        , text <| "Num Guesses: " ++ String.fromInt model.numGuesses
        , br [] []
        , text <| "Num Wins: " ++ String.fromInt model.numWins
        , br [] []
        , text <| "Your Guess: "
        , input [ onInput SetGuess ] []
        , button
            [ disabled model.hasWon
            , onClick Evaluate
            ]
            [ text "Evaluate" ]
        , button
            [ onClick StartOver
            ]
            [ text "New Game" ]
        , br [] []
        , text model.message

        -- , br [] []
        --, text <|
        --    "PICO = no hits, PICO = right number/wrong place, "
        --        ++ "FERMI = right number/place"
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
