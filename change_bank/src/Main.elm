module Main exposing (..)

import Browser
import Html exposing (Html, br, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe, withDefault)



---- MODEL ----


type alias Model =
    { amount : Maybe Int
    , error : Maybe String
    }


type alias CoinGroup =
    { pennies : Int
    , nickels : Int
    , dimes : Int
    , quarters : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { amount = Nothing, error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = SetAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAmount amount ->
            let
                ( newAmount, newError ) =
                    case String.length amount of
                        0 ->
                            ( Nothing, Nothing )

                        _ ->
                            case String.toFloat amount of
                                Just n ->
                                    if n < 0 then
                                        ( Nothing
                                        , Just "Amount must be > $0.00"
                                        )

                                    else if n > 1 then
                                        ( Nothing
                                        , Just "Amount must be <= $1.00"
                                        )

                                    else
                                        ( Just (round (n * 100)), Nothing )

                                _ ->
                                    ( Nothing, Just "That is not a number" )
            in
            ( { model | amount = newAmount, error = newError }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        expand coinGroup =
            let
                pennies =
                    if coinGroup.pennies > 0 then
                        let
                            noun =
                                if coinGroup.pennies == 1 then
                                    " penny"

                                else
                                    " pennies"
                        in
                        Just (String.fromInt coinGroup.pennies ++ noun)

                    else
                        Nothing

                nickels =
                    if coinGroup.nickels > 0 then
                        let
                            noun =
                                if coinGroup.nickels == 1 then
                                    " nickel"

                                else
                                    " nickels"
                        in
                        Just (String.fromInt coinGroup.nickels ++ noun)

                    else
                        Nothing

                dimes =
                    if coinGroup.dimes > 0 then
                        let
                            noun =
                                if coinGroup.dimes == 1 then
                                    " dime"

                                else
                                    " dimes"
                        in
                        Just (String.fromInt coinGroup.dimes ++ noun)

                    else
                        Nothing

                quarters =
                    if coinGroup.quarters > 0 then
                        let
                            noun =
                                if coinGroup.quarters == 1 then
                                    " quarter"

                                else
                                    " quarters"
                        in
                        Just (String.fromInt coinGroup.quarters ++ noun)

                    else
                        Nothing
            in
            String.join ", " <|
                List.filterMap identity [ pennies, nickels, dimes, quarters ]

        mkLi item =
            li [] [ text (expand item) ]

        mkUl items =
            case List.length items of
                0 ->
                    text ""

                _ ->
                    ul [] (List.map mkLi items)

        groups =
            case model.amount of
                Just n ->
                    mkUl (makeChange n)

                _ ->
                    text ""
    in
    div []
        [ h1 [] [ text "First Bank of Change" ]
        , text "Enter a value between $0.00 and $1.00: "
        , input [ onInput SetAmount ] []
        , br [] []
        , text (withDefault "" model.error)
        , groups
        ]


makeChange : Int -> List CoinGroup
makeChange amount =
    let
        nickels =
            amount // 5

        dimes =
            amount // 10

        quarters =
            amount // 25

        combos =
            cartesianProduct
                [ List.range 0 nickels
                , List.range 0 dimes
                , List.range 0 quarters
                ]

        figure combo =
            case combo of
                [ n, d, q ] ->
                    let
                        bigCoins =
                            List.sum [ 5 * n, 10 * d, 25 * q ]
                    in
                    if bigCoins <= amount then
                        Just
                            { quarters = q
                            , dimes = d
                            , nickels = n
                            , pennies = amount - bigCoins
                            }

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap figure combos



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
