module Tests exposing (..)

import Expect
import Main
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Bagles Test Suite"
        [ test "compare_fermi" <|
            \_ ->
                Expect.equal
                    (Just "FERMI")
                    (Main.compare '1' 0 [ '1', '2', '3' ])
        , test "compare_pico" <|
            \_ ->
                Expect.equal
                    (Just "PICO")
                    (Main.compare '1' 0 [ '3', '1', '2' ])
        , test "compare_miss" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Main.compare '9' 0 [ '3', '1', '2' ])
        , test "bagles1" <|
            \_ ->
                Expect.equal
                    (Err "BAGLES")
                    (Main.bagles "123" "456")
        , test "bagles2" <|
            \_ ->
                Expect.equal
                    (Ok "You guessed it!")
                    (Main.bagles "123" "123")
        , test "bagles3" <|
            \_ ->
                Expect.equal
                    (Err "FERMI")
                    (Main.bagles "123" "198")
        , test "bagles4" <|
            \_ ->
                Expect.equal
                    (Err "PICO FERMI")
                    (Main.bagles "234" "431")
        , test "bagles5" <|
            \_ ->
                Expect.equal
                    (Err "FERMI")
                    (Main.bagles "234" "531")
        , test "bagles6" <|
            \_ ->
                Expect.equal
                    (Err "PICO PICO PICO")
                    (Main.bagles "234" "423")
        ]
