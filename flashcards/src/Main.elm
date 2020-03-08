module Main exposing (Card, Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, form, h1, img, input, text)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { question : String
    , answer : String
    , cards : List Card
    , hideCards : Bool
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
    , hideCards = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = AddCard
    | ToggleHideCards
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

        ToggleHideCards ->
            ( { model | hideCards = not model.hideCards }, Cmd.none )

        UpdateQuestion newQuestion ->
            ( { model | question = newQuestion }, Cmd.none )

        UpdateAnswer newAnswer ->
            ( { model | answer = newAnswer }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Flashcards" ]
        , createCard model
        , viewCards model
        ]


createCard : Model -> Html Msg
createCard model =
    div []
        [ div []
            [ text "Question: "
            , input [ onInput UpdateQuestion, value model.question ] []
            ]
        , div []
            [ text "Answer: "
            , input [ onInput UpdateAnswer, value model.answer ] []
            ]
        , div []
            [ button [ onClick AddCard ] [ text "Add" ]
            ]
        ]


viewCards : Model -> Html Msg
viewCards model =
    let
        viewCard card =
            div []
                [ div [] [ text ("Question: " ++ card.question) ]
                , div [] [ text ("Answer: " ++ card.answer) ]
                ]

        viewer =
            case model.hideCards of
                True ->
                    div [] []

                _ ->
                    case List.length model.cards of
                        0 ->
                            div [] [ text "No cards" ]

                        _ ->
                            div [] (List.map viewCard model.cards)

        hideShow =
            case model.hideCards of
                True ->
                    "Show"

                _ ->
                    "Hide"
    in
    div []
        [ div [] [ button [ onClick ToggleHideCards ] [ text hideShow ] ]
        , viewer
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
