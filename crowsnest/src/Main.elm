module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



---- UPDATE ----


type Msg
    = UpdateModel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateModel value ->
            ( value, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        first =
            String.toLower (String.left 1 model)

        vowels =
            [ "a", "e", "i", "o", "u" ]

        article =
            if List.member first vowels then
                "an"

            else
                "a"
    in
    div []
        [ h1 [] [ text "Crow's Nest" ]
        , div [] [ text "What do you see?" ]
        , div [] [ input [ onInput UpdateModel ] [] ]
        , div []
            [ text
                ("Ahoy, Captain,  "
                    ++ article
                    ++ " "
                    ++ model
                    ++ " off the larboard bow!"
                )
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
