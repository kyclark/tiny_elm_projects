module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, table, td, text, tr)
import Html.Attributes exposing (src)



---- MODEL ----


type Player
    = X
    | O


type alias Model =
    { currentPlayer : Player }


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = X }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , table []
            [ tr []
                [ td [] [ text "1" ]
                , td [] [ text "2" ]
                , td [] [ text "3" ]
                ]
            , tr []
                [ td [] [ text "4" ]
                , td [] [ text "5" ]
                , td [] [ text "6" ]
                ]
            , tr []
                [ td [] [ text "7" ]
                , td [] [ text "8" ]
                , td [] [ text "9" ]
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
