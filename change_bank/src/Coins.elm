module Coins exposing (CoinGroup, expand, makeChange, numberAgree)

import List.Extra exposing (cartesianProduct)


type alias CoinGroup =
    { pennies : Int
    , nickels : Int
    , dimes : Int
    , quarters : Int
    }


numberAgree : Int -> String -> String
numberAgree count noun =
    let
        len =
            String.length noun

        root =
            String.slice 0 (len - 1) noun

        lastLetter =
            String.slice (len - 1) len noun

        strCount =
            String.fromInt count
    in
    if count == 1 then
        strCount ++ " " ++ noun

    else if lastLetter == "y" then
        strCount ++ " " ++ root ++ "ies"

    else
        strCount ++ " " ++ noun ++ "s"


makeChange : Int -> List CoinGroup
makeChange amount =
    let
        combos =
            cartesianProduct
                [ List.range 0 (amount // 5)
                , List.range 0 (amount // 10)
                , List.range 0 (amount // 25)
                ]

        figure combo =
            case combo of
                [ nickels, dimes, quarters ] ->
                    let
                        bigCoins =
                            List.sum [ 5 * nickels, 10 * dimes, 25 * quarters ]

                        pennies =
                            amount - bigCoins
                    in
                    if bigCoins <= amount then
                        Just (CoinGroup pennies nickels dimes quarters)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap figure combos


expand : CoinGroup -> String
expand coinGroup =
    let
        pennies =
            if coinGroup.pennies > 0 then
                Just (numberAgree coinGroup.pennies "penny")

            else
                Nothing

        nickels =
            if coinGroup.nickels > 0 then
                Just (numberAgree coinGroup.nickels "nickel")

            else
                Nothing

        dimes =
            if coinGroup.dimes > 0 then
                Just (numberAgree coinGroup.dimes "dime")

            else
                Nothing

        quarters =
            if coinGroup.quarters > 0 then
                Just (numberAgree coinGroup.quarters "quarter")

            else
                Nothing
    in
    String.join ", " <|
        List.filterMap identity [ pennies, nickels, dimes, quarters ]
