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
        UpdateModel newModel ->
            ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        isAlpha char =
            List.member char (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

        cmpThis =
            String.join "" <|
                List.map String.fromChar <|
                    List.filter isAlpha <|
                        String.toList (String.toUpper model)

        isPalindrome =
            cmpThis == String.reverse cmpThis

        verb =
            if isPalindrome then
                "is"

            else
                "is not"

        result =
            if String.length model == 0 then
                ""

            else
                "\"" ++ model ++ "\" " ++ verb ++ " a palindrome."
    in
    div []
        [ h1 [] [ text "Welcome to the Palindrome" ]
        , div [] [ input [ onInput UpdateModel ] [] ]
        , div [] [ text result ]
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
