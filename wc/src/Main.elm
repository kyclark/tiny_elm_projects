module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, text, textarea)
import Html.Attributes exposing (cols, placeholder, rows, src, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    String


sonnet =
    [ "Sonnet 29"
    , "William Shakespeare"
    , "When, in disgrace with fortune and men’s eyes,"
    , "I all alone beweep my outcast state,"
    , "And trouble deaf heaven with my bootless cries,"
    , "And look upon myself and curse my fate,"
    , "Wishing me like to one more rich in hope,"
    , "Featured like him, like him with friends possessed,"
    , "Desiring this man’s art and that man’s scope,"
    , "With what I most enjoy contented least;"
    , "Yet in these thoughts myself almost despising,"
    , "Haply I think on thee, and then my state,"
    , "(Like to the lark at break of day arising"
    , "From sullen earth) sings hymns at heaven’s gate;"
    , "For thy sweet love remembered such wealth brings"
    , "That then I scorn to change my state with kings."
    ]


init : ( Model, Cmd Msg )
init =
    ( String.join "\n" sonnet, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText val ->
            ( val, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Word Count" ]
        , div [] [ textarea [ cols 100, rows 20, value model, onInput UpdateText ] [] ]
        , wc model
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



--wc : String -> Html.msg


wc txt =
    let
        lines =
            String.split "\n" txt

        words =
            List.filter (\word -> String.length word > 0) <|
                List.concat <|
                    List.map (String.split " ") lines

        chars =
            List.concat <| List.map (String.split "") words

        _ =
            Debug.log "words" words
    in
    text <|
        "Lines = "
            ++ String.fromInt (List.length lines)
            ++ ", Words = "
            ++ String.fromInt (List.length words)
            ++ ", Characters = "
            ++ String.fromInt (List.length chars)
