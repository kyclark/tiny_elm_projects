module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, input, li, ol, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { currentItem : String
    , items : List String
    }


initialModel =
    { currentItem = ""
    , items = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = AddCurrentItem
    | UpdateCurrentItem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCurrentItem ->
            ( { model
                | items = model.items ++ [ model.currentItem ]
              }
            , Cmd.none
            )

        UpdateCurrentItem item ->
            ( { model | currentItem = item }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        stringIt vals =
            String.join " " vals

        itemList =
            case List.length model.items of
                0 ->
                    ""

                1 ->
                    case List.head model.items of
                        Just item ->
                            item

                        _ ->
                            ""

                2 ->
                    String.join " " <| List.intersperse " and " model.items

                _ ->
                    let
                        len =
                            List.length model.items

                        firstItems =
                            List.take (len - 1) model.items

                        lastItem =
                            List.drop (len - 1) model.items
                    in
                    String.join "" <|
                        List.intersperse ", " <|
                            firstItems
                                ++ [ stringIt [ "and ", stringIt lastItem ] ]

        itemSummary =
            case String.length itemList of
                0 ->
                    "Our basket is empty!"

                _ ->
                    "We'll take " ++ itemList ++ "."

        itemBullets =
            case List.length model.items of
                0 ->
                    text ""

                _ ->
                    ol [] (List.map (\val -> li [] [ text val ]) model.items)
    in
    div []
        [ h1 [] [ text "Picnic" ]
        , div []
            [ text "New item: "
            , input [ onInput UpdateCurrentItem ] []
            , button [ onClick AddCurrentItem ] [ text "Add" ]
            ]
        , div [] [ text <| "We're going on a picnic. " ++ itemSummary ]
        , itemBullets
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
