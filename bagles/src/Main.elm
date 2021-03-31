module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, text)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt)
import Maybe
import Maybe.Extra exposing (isNothing)
import Random
import Result.Extra exposing (isOk)



---- MODEL ----


type alias Model =
    { rng : Random.Generator Int
    , secret : String
    , guess : String
    , numGuesses : Int
    , hasWon : Bool
    , status : String
    , numWins : Int
    }


initialModel =
    { rng = Random.int 100 999
    , secret = ""
    , guess = ""
    , numGuesses = 0
    , hasWon = False
    , status = ""
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
                result =
                    bagles model.secret model.guess

                ( hasWon, newStatus ) =
                    case result of
                        Ok s ->
                            ( True, s )

                        Err s ->
                            ( False, s )

                numWins =
                    case hasWon of
                        True ->
                            model.numWins + 1

                        _ ->
                            model.numWins
            in
            ( { model
                | hasWon = hasWon
                , status = newStatus
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
    div []
        [ h1 [] [ text "Bagles" ]

        --, text <| "Secret: " ++ model.secret
        --, br [] []
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
        , text model.status
        ]


bagles : String -> String -> Result String String
bagles secret guess =
    let
        secretChars =
            String.toList secret

        guessChars =
            List.map2 Tuple.pair
                (List.range 0 2)
                (String.toList guess)

        results =
            List.map
                (\( pos, char ) -> compare char pos secretChars)
                guessChars

        display =
            if List.all isNothing results then
                "BAGLES"

            else
                String.trim <|
                    String.join " "
                        (List.map (Maybe.withDefault "") results)
    in
    case secret == guess of
        True ->
            Ok "You guessed it!"

        _ ->
            Err display


compare : Char -> Int -> List Char -> Maybe String
compare char pos secret =
    if getAt pos secret == Just char then
        Just "FERMI"

    else if List.member char secret then
        Just "PICO"

    else
        Nothing



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
