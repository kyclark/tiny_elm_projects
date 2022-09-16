module Main exposing (..)

import Browser
import Coins exposing (expand, makeChange, numberAgree)
import Html exposing (Html, br, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)
import Maybe exposing (Maybe, withDefault)



---- MODEL ----


type alias Model =
    { amount : Maybe Int
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { amount = Nothing, error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = SetAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAmount amount ->
            let
                ( newAmount, newError ) =
                    case String.length amount of
                        0 ->
                            ( Nothing, Nothing )

                        _ ->
                            case String.toInt amount of
                                Just n ->
                                    if n < 0 then
                                        ( Nothing
                                        , Just "Amount must be > 0"
                                        )

                                    else if n > 100 then
                                        ( Nothing
                                        , Just "Amount must be <= 100"
                                        )

                                    else
                                        ( Just n, Nothing )

                                _ ->
                                    ( Nothing, Just "That is not a number" )
            in
            ( { model | amount = newAmount, error = newError }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        mkLi coinGroup =
            li [] [ text (expand coinGroup) ]

        mkUl coinGroups =
            case List.length coinGroups of
                0 ->
                    text ""

                _ ->
                    ul [] (List.map mkLi coinGroups)

        groups =
            case model.amount of
                Just n ->
                    mkUl (makeChange n)

                _ ->
                    text ""
    in
    div []
        [ h1 [] [ text "First Bank of Change" ]
        , text "Enter a value between 0 and 100: "
        , input [ onInput SetAmount ] []
        , br [] []
        , text (withDefault "" model.error)
        , groups
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
