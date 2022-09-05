module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, option, table, td, text, tr)
import Html.Attributes exposing (colspan, src, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type alias Model =
    { input : String
    , operation : Maybe Operation
    , result : Maybe Float
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { input = ""
      , operation = Nothing
      , result = Nothing
      , error = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Clear
    | SetInput String
    | SetOperation Operation
    | PerformOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( { model | input = "" }, Cmd.none )

        SetInput val ->
            ( { model | input = model.input ++ val }, Cmd.none )

        SetOperation operation ->
            ( { model | operation = Just operation }, Cmd.none )

        PerformOp ->
            let
                input =
                    String.toFloat model.input

                ( newResult, newInput, newError ) =
                    case input of
                        Just num ->
                            let
                                curResult =
                                    case model.result of
                                        Just n ->
                                            n

                                        _ ->
                                            case model.operation of
                                                Just Add ->
                                                    0

                                                Just Subtract ->
                                                    0

                                                _ ->
                                                    1

                                res =
                                    case model.operation of
                                        Just Add ->
                                            curResult + num

                                        Just Subtract ->
                                            curResult - num

                                        Just Multiply ->
                                            curResult * num

                                        Just Divide ->
                                            curResult / num

                                        _ ->
                                            curResult
                            in
                            ( Just res, "", Nothing )

                        _ ->
                            ( model.result
                            , model.input
                            , Just (model.input ++ " is not a valid number")
                            )
            in
            ( { model
                | input = newInput
                , result = newResult
                , error = newError
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr []
                [ td [] [ button [ onClick (SetInput "1") ] [ text "1" ] ]
                , td [] [ button [ onClick (SetInput "2") ] [ text "2" ] ]
                , td [] [ button [ onClick (SetInput "3") ] [ text "3" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick (SetInput "4") ] [ text "4" ] ]
                , td [] [ button [ onClick (SetInput "5") ] [ text "5" ] ]
                , td [] [ button [ onClick (SetInput "6") ] [ text "6" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick (SetInput "7") ] [ text "7" ] ]
                , td [] [ button [ onClick (SetInput "8") ] [ text "8" ] ]
                , td [] [ button [ onClick (SetInput "9") ] [ text "9" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick (SetInput "0") ] [ text "0" ] ]
                , td [] [ button [ onClick (SetInput ".") ] [ text "." ] ]
                , td [] [ button [ onClick (SetOperation Add) ] [ text "+" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick (SetOperation Subtract) ] [ text "-" ] ]
                , td [] [ button [ onClick (SetOperation Multiply) ] [ text "*" ] ]
                , td [] [ button [ onClick (SetOperation Divide) ] [ text "/" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick Clear ] [ text "C" ] ]
                , td [] []
                , td [] []
                ]
            , tr []
                [ td []
                    [ text <| "Input = " ++ model.input
                    ]
                , td []
                    [ text <|
                        "Result = "
                            ++ String.fromFloat
                                (Maybe.withDefault 0 model.result)
                    ]
                , td []
                    [ text <| "Operation = " ++ Debug.toString model.operation ]
                ]
            ]
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
