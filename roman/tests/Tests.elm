module Tests exposing (..)

import Expect
import Roman exposing (toArabic, toRoman)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!
--all : Test
--all =
--    describe "A Test Suite"
--        [ test "Addition" <|
--            \_ ->
--                Expect.equal 10 (3 + 7)
--        , test "String.left" <|
--            \_ ->
--                Expect.equal "a" (String.left 1 "abcdefg")
--        , test "This test should fail" <|
--            \_ ->
--                Expect.fail "failed as expected!"
--        ]


toArabicTest : Test
toArabicTest =
    describe "toArabic"
        [ test "1" <|
            \_ -> Expect.equal (Ok 1) (toArabic "I")
        , test "2" <|
            \_ -> Expect.equal (Ok 2) (toArabic "II")
        , test "3" <|
            \_ -> Expect.equal (Ok 3) (toArabic "III")
        , test "4" <|
            \_ -> Expect.equal (Ok 4) (toArabic "IV")
        , test "5" <|
            \_ -> Expect.equal (Ok 5) (toArabic "V")
        , test "6" <|
            \_ -> Expect.equal (Ok 6) (toArabic "VI")
        , test "7" <|
            \_ -> Expect.equal (Ok 7) (toArabic "VII")
        , test "8" <|
            \_ -> Expect.equal (Ok 8) (toArabic "VIII")
        , test "9" <|
            \_ -> Expect.equal (Ok 9) (toArabic "IX")
        , test "10" <|
            \_ -> Expect.equal (Ok 10) (toArabic "X")
        , test "39" <|
            \_ -> Expect.equal (Ok 39) (toArabic "XXXIX")
        , test "160" <|
            \_ -> Expect.equal (Ok 160) (toArabic "CLX")
        , test "207" <|
            \_ -> Expect.equal (Ok 207) (toArabic "CCVII")
        , test "246" <|
            \_ -> Expect.equal (Ok 246) (toArabic "CCXLVI")
        , test "789" <|
            \_ -> Expect.equal (Ok 789) (toArabic "DCCLXXXIX")
        , test "1009" <|
            \_ -> Expect.equal (Ok 1009) (toArabic "MIX")
        , test "1066" <|
            \_ -> Expect.equal (Ok 1066) (toArabic "MLXVI")
        , test "1776" <|
            \_ -> Expect.equal (Ok 1776) (toArabic "MDCCLXXVI")
        , test "1918" <|
            \_ -> Expect.equal (Ok 1918) (toArabic "MCMXVIII")
        , test "1954" <|
            \_ -> Expect.equal (Ok 1954) (toArabic "MCMLIV")
        , test "2014" <|
            \_ -> Expect.equal (Ok 2014) (toArabic "MMXIV")
        , test "2421" <|
            \_ -> Expect.equal (Ok 2421) (toArabic "MMCDXXI")
        , test "3999" <|
            \_ -> Expect.equal (Ok 3999) (toArabic "MMMCMXCIX")
        , test "Empty string" <|
            \_ -> Expect.equal (Err "Empty string") (toArabic "")
        , test "Bad Roman" <|
            \_ ->
                Expect.equal
                    (Err "Invalid Roman numeral (XIM)")
                    (toArabic "XIM")
        ]


toRomanTest : Test
toRomanTest =
    describe "toRoman"
        [ test "1" <|
            \_ -> Expect.equal (Ok "I") (toRoman "1")
        , test "2" <|
            \_ -> Expect.equal (Ok "II") (toRoman "2")
        , test "3" <|
            \_ -> Expect.equal (Ok "III") (toRoman "3")
        , test "4" <|
            \_ -> Expect.equal (Ok "IV") (toRoman "4")
        , test "5" <|
            \_ -> Expect.equal (Ok "V") (toRoman "5")
        , test "6" <|
            \_ -> Expect.equal (Ok "VI") (toRoman "6")
        , test "7" <|
            \_ -> Expect.equal (Ok "VII") (toRoman "7")
        , test "8" <|
            \_ -> Expect.equal (Ok "VIII") (toRoman "8")
        , test "9" <|
            \_ -> Expect.equal (Ok "IX") (toRoman "9")
        , test "10" <|
            \_ -> Expect.equal (Ok "X") (toRoman "10")
        , test "39" <|
            \_ -> Expect.equal (Ok "XXXIX") (toRoman "39")
        , test "160" <|
            \_ -> Expect.equal (Ok "CLX") (toRoman "160")
        , test "207" <|
            \_ -> Expect.equal (Ok "CCVII") (toRoman "207")
        , test "246" <|
            \_ -> Expect.equal (Ok "CCXLVI") (toRoman "246")
        , test "789" <|
            \_ -> Expect.equal (Ok "DCCLXXXIX") (toRoman "789")
        , test "1009" <|
            \_ -> Expect.equal (Ok "MIX") (toRoman "1009")
        , test "1066" <|
            \_ -> Expect.equal (Ok "MLXVI") (toRoman "1066")
        , test "1776" <|
            \_ -> Expect.equal (Ok "MDCCLXXVI") (toRoman "1776")
        , test "1918" <|
            \_ -> Expect.equal (Ok "MCMXVIII") (toRoman "1918")
        , test "1954" <|
            \_ -> Expect.equal (Ok "MCMLIV") (toRoman "1954")
        , test "2014" <|
            \_ -> Expect.equal (Ok "MMXIV") (toRoman "2014")
        , test "2421" <|
            \_ -> Expect.equal (Ok "MMCDXXI") (toRoman "2421")
        , test "3999" <|
            \_ -> Expect.equal (Ok "MMMCMXCIX") (toRoman "3999")
        , test "0" <|
            \_ ->
                Expect.equal
                    (Err "Number (0) must be > 1 and < 4000")
                    (toRoman "0")
        , test "4000" <|
            \_ ->
                Expect.equal
                    (Err "Number (4000) must be > 1 and < 4000")
                    (toRoman "4000")
        , test "3.14" <|
            \_ ->
                Expect.equal
                    (Err "\"3.14\" is not an integer")
                    (toRoman "3.14")
        ]
