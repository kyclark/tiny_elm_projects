module Tests exposing (..)

import Bagles exposing (bagles, evaluate)
import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Bagles Test Suite"
        [ test "evaluate_fermi" <|
            \_ ->
                Expect.equal
                    (Just "FERMI")
                    (evaluate '1' 0 [ '1', '2', '3' ])
        , test "evaluate_pico" <|
            \_ ->
                Expect.equal
                    (Just "PICO")
                    (evaluate '1' 0 [ '3', '1', '2' ])
        , test "evaluate_miss" <|
            \_ ->
                Expect.equal
                    Nothing
                    (evaluate '9' 0 [ '3', '1', '2' ])
        , test "bagles1" <|
            \_ ->
                Expect.equal
                    (Err "BAGLES")
                    (bagles "123" "456")
        , test "bagles2" <|
            \_ ->
                Expect.equal
                    (Ok "You guessed it!")
                    (bagles "123" "123")
        , test "bagles3" <|
            \_ ->
                Expect.equal
                    (Err "FERMI")
                    (bagles "123" "198")
        , test "bagles4" <|
            \_ ->
                Expect.equal
                    (Err "PICO FERMI")
                    (bagles "234" "431")
        , test "bagles5" <|
            \_ ->
                Expect.equal
                    (Err "FERMI")
                    (bagles "234" "531")
        , test "bagles6" <|
            \_ ->
                Expect.equal
                    (Err "PICO PICO PICO")
                    (bagles "234" "423")
        ]
