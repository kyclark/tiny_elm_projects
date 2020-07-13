module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, input, li, text)
import Html.Attributes exposing (placeholder, src)
import Html.Events exposing (onInput)
import List.Extra exposing (cartesianProduct)



---- MODEL ----


type alias Model =
    { name1 : Maybe String
    , name2 : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { name1 = Nothing, name2 = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = SetName1 String
    | SetName2 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName1 newValue ->
            let
                val =
                    case String.length newValue of
                        0 ->
                            Nothing

                        _ ->
                            Just newValue
            in
            ( { model | name1 = val }, Cmd.none )

        SetName2 newValue ->
            let
                val =
                    case String.length newValue of
                        0 ->
                            Nothing

                        _ ->
                            Just newValue
            in
            ( { model | name2 = val }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        combos =
            case ( model.name1, model.name2 ) of
                ( Just n1, Just n2 ) ->
                    let
                        start1 =
                            starts n1

                        end2 =
                            ends n2

                        cross1 =
                            cartesianProduct [ start1, end2 ]

                        join xs =
                            String.join "-" xs
                    in
                    List.map join cross1

                _ ->
                    [ "Nothing here" ]

        showName name =
            case name of
                Nothing ->
                    ""

                Just n ->
                    n
    in
    div []
        [ h1 [] [ text "Name Combiner" ]
        , div []
            [ input [ placeholder "Name1", onInput SetName1 ] []
            , input [ placeholder "Name2", onInput SetName2 ] []
            ]
        , div []
            [ text (showName model.name1)
            , text (showName model.name2)
            ]
        , div [] <|
            List.map
                (\t -> li [] [ text t ])
                combos
        ]


starts : String -> List String
starts word =
    List.map (\n -> String.slice 0 n word) <| List.range 1 (String.length word)


ends : String -> List String
ends word =
    let
        end =
            String.length word
    in
    List.map (\n -> String.slice n end word) <|
        List.range 0 (String.length word)



--combine name1 name2 =
--    let
--        start = ""
--    in
--        ""
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
