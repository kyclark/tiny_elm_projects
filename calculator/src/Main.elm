module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, option, text)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type alias Model =
    { input : Int
    , result : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { input = 0, result = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = SetInput String
    | PerformOp Operation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInput val ->
            let
                newInput =
                    Maybe.withDefault 0 (String.toInt val)
            in
            ( { model | input = newInput }, Cmd.none )

        PerformOp op ->
            let
                curResult =
                    model.result

                newResult =
                    case op of
                        Add ->
                            curResult + model.input

                        Subtract ->
                            curResult - model.input

                        Multiply ->
                            curResult * model.input

                        Divide ->
                            curResult // model.input
            in
            ( { model | result = newResult }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text <| "Result = " ++ String.fromInt model.result
        , br [] []
        , input
            [ value (String.fromInt model.input)
            , onInput SetInput
            ]
            []
        , button [ onClick (PerformOp Add) ] [ text "+" ]
        , button [ onClick (PerformOp Subtract) ] [ text "-" ]
        , button [ onClick (PerformOp Multiply) ] [ text "*" ]
        , button [ onClick (PerformOp Divide) ] [ text "/" ]
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
