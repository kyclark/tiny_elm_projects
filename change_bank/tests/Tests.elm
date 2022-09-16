module Tests exposing (..)

import Coins exposing (CoinGroup, expand, makeChange, numberAgree)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Coins Tests"
        [ test "1 penny" <|
            \_ ->
                Expect.equal "1 penny" (numberAgree 1 "penny")
        , test "2 pennies" <|
            \_ ->
                Expect.equal "2 pennies" (numberAgree 2 "penny")
        , test "1 nickel" <|
            \_ ->
                Expect.equal "1 nickel" (numberAgree 1 "nickel")
        , test "2 nickels" <|
            \_ ->
                Expect.equal "2 nickels" (numberAgree 2 "nickel")
        , test "1 dime" <|
            \_ ->
                Expect.equal "1 dime" (numberAgree 1 "dime")
        , test "2 dimes" <|
            \_ ->
                Expect.equal "2 dimes" (numberAgree 2 "dime")
        , test "1 quarter" <|
            \_ ->
                Expect.equal "1 quarter" (numberAgree 1 "quarter")
        , test "2 quarters" <|
            \_ ->
                Expect.equal "2 quarters" (numberAgree 2 "quarter")
        , test "makeChange 1" <|
            \_ ->
                Expect.equal
                    [ CoinGroup 1 0 0 0 ]
                    (makeChange 1)
        , test "makeChange 5" <|
            \_ ->
                Expect.equal
                    [ CoinGroup 5 0 0 0
                    , CoinGroup 0 1 0 0
                    ]
                    (makeChange 5)
        , test "makeChange 6" <|
            \_ ->
                Expect.equal
                    [ CoinGroup 6 0 0 0
                    , CoinGroup 1 1 0 0
                    ]
                    (makeChange 6)
        , test "makeChange 10" <|
            \_ ->
                Expect.equal
                    [ CoinGroup 10 0 0 0
                    , CoinGroup 0 0 1 0
                    , CoinGroup 5 1 0 0
                    , CoinGroup 0 2 0 0
                    ]
                    (makeChange 10)
        , test "makeChange 26" <|
            \_ ->
                Expect.equal
                    [ CoinGroup 26 0 0 0
                    , CoinGroup 1 0 0 1
                    , CoinGroup 16 0 1 0
                    , CoinGroup 6 0 2 0
                    , CoinGroup 21 1 0 0
                    , CoinGroup 11 1 1 0
                    , CoinGroup 1 1 2 0
                    , CoinGroup 16 2 0 0
                    , CoinGroup 6 2 1 0
                    , CoinGroup 11 3 0 0
                    , CoinGroup 1 3 1 0
                    , CoinGroup 6 4 0 0
                    , CoinGroup 1 5 0 0
                    ]
                    (makeChange 26)
        , test "expand 1 penny" <|
            \_ ->
                Expect.equal
                    "1 penny"
                    (expand (CoinGroup 1 0 0 0))
        , test "expand 1 penny 1 nickel" <|
            \_ ->
                Expect.equal
                    "1 penny, 1 nickel"
                    (expand (CoinGroup 1 1 0 0))
        , test "expand 2 pennies 3 nickels 4 quarters" <|
            \_ ->
                Expect.equal
                    "2 pennies, 3 nickels, 4 quarters"
                    (expand (CoinGroup 2 3 0 4))
        ]
