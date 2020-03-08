module Main exposing (Card, Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, form, h1, img, input, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { question : String
    , answer : String
    , cards : List Card
    }


type alias Card =
    { question : String
    , answer : String
    }


initialModel : Model
initialModel =
    { question = ""
    , answer = ""
    , cards = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = AddCard
    | UpdateQuestion String
    | UpdateAnswer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCard ->
            let
                newCard =
                    Card model.question model.answer

                _ =
                    Debug.log "newCard" newCard
            in
            ( { model
                | question = ""
                , answer = ""
                , cards = model.cards ++ [ newCard ]
              }
            , Cmd.none
            )

        UpdateQuestion newQuestion ->
            ( { model | question = newQuestion }, Cmd.none )

        UpdateAnswer newAnswer ->
            ( { model | answer = newAnswer }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Flashcards" ]
        , inputForm
        , viewCards model.cards
        ]


inputForm : Html Msg
inputForm =
    div []
        [ div []
            [ text "Question: "
            , input [ onInput UpdateQuestion ] []
            ]
        , div []
            [ text "Answer: "
            , input [ onInput UpdateAnswer ] []
            ]
        , div []
            [ button [ onClick AddCard ] [ text "Add" ]
            ]
        ]


viewCards : List Card -> Html msg
viewCards cards =
    let
        viewCard card =
            div []
                [ div [] [ text ("Question: " ++ card.question) ]
                , div [] [ text ("Answer: " ++ card.answer) ]
                ]
    in
    case List.length cards of
        0 ->
            div [] [ text "No cards" ]

        _ ->
            div [] (List.map viewCard cards)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
