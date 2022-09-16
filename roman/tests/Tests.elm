module Tests exposing (..)

import Expect
import Roman exposing (arabicToRoman, romanToArabic)
import Test exposing (..)


romanToArabicTest : Test
romanToArabicTest =
    describe "romanToArabic"
        [ test "1" <|
            \_ -> Expect.equal (Ok 1) (romanToArabic "I")
        , test "2" <|
            \_ -> Expect.equal (Ok 2) (romanToArabic "II")
        , test "3" <|
            \_ -> Expect.equal (Ok 3) (romanToArabic "III")
        , test "4" <|
            \_ -> Expect.equal (Ok 4) (romanToArabic "IV")
        , test "5" <|
            \_ -> Expect.equal (Ok 5) (romanToArabic "V")
        , test "6" <|
            \_ -> Expect.equal (Ok 6) (romanToArabic "VI")
        , test "7" <|
            \_ -> Expect.equal (Ok 7) (romanToArabic "VII")
        , test "8" <|
            \_ -> Expect.equal (Ok 8) (romanToArabic "VIII")
        , test "9" <|
            \_ -> Expect.equal (Ok 9) (romanToArabic "IX")
        , test "10" <|
            \_ -> Expect.equal (Ok 10) (romanToArabic "X")
        , test "39" <|
            \_ -> Expect.equal (Ok 39) (romanToArabic "XXXIX")
        , test "40" <|
            \_ -> Expect.equal (Ok 40) (romanToArabic "XL")
        , test "50" <|
            \_ -> Expect.equal (Ok 50) (romanToArabic "L")
        , test "90" <|
            \_ -> Expect.equal (Ok 90) (romanToArabic "XC")
        , test "100" <|
            \_ -> Expect.equal (Ok 100) (romanToArabic "C")
        , test "160" <|
            \_ -> Expect.equal (Ok 160) (romanToArabic "CLX")
        , test "207" <|
            \_ -> Expect.equal (Ok 207) (romanToArabic "CCVII")
        , test "246" <|
            \_ -> Expect.equal (Ok 246) (romanToArabic "CCXLVI")
        , test "789" <|
            \_ -> Expect.equal (Ok 789) (romanToArabic "DCCLXXXIX")
        , test "1009" <|
            \_ -> Expect.equal (Ok 1009) (romanToArabic "MIX")
        , test "1066" <|
            \_ -> Expect.equal (Ok 1066) (romanToArabic "MLXVI")
        , test "1776" <|
            \_ -> Expect.equal (Ok 1776) (romanToArabic "MDCCLXXVI")
        , test "1918" <|
            \_ -> Expect.equal (Ok 1918) (romanToArabic "MCMXVIII")
        , test "1954" <|
            \_ -> Expect.equal (Ok 1954) (romanToArabic "MCMLIV")
        , test "2014" <|
            \_ -> Expect.equal (Ok 2014) (romanToArabic "MMXIV")
        , test "2421" <|
            \_ -> Expect.equal (Ok 2421) (romanToArabic "MMCDXXI")
        , test "3999" <|
            \_ -> Expect.equal (Ok 3999) (romanToArabic "MMMCMXCIX")
        , test "Empty string" <|
            \_ ->
                Expect.equal (Err "Invalid Roman numeral ()")
                    (romanToArabic "")
        , test "Bad Roman" <|
            \_ ->
                Expect.equal
                    (Err "Invalid Roman numeral (XIM)")
                    (romanToArabic "XIM")
        ]


arabicToRomanTest : Test
arabicToRomanTest =
    describe "arabicToRoman"
        [ test "1" <|
            \_ -> Expect.equal (Ok "I") (arabicToRoman "1")
        , test "2" <|
            \_ -> Expect.equal (Ok "II") (arabicToRoman "2")
        , test "3" <|
            \_ -> Expect.equal (Ok "III") (arabicToRoman "3")
        , test "4" <|
            \_ -> Expect.equal (Ok "IV") (arabicToRoman "4")
        , test "5" <|
            \_ -> Expect.equal (Ok "V") (arabicToRoman "5")
        , test "6" <|
            \_ -> Expect.equal (Ok "VI") (arabicToRoman "6")
        , test "7" <|
            \_ -> Expect.equal (Ok "VII") (arabicToRoman "7")
        , test "8" <|
            \_ -> Expect.equal (Ok "VIII") (arabicToRoman "8")
        , test "9" <|
            \_ -> Expect.equal (Ok "IX") (arabicToRoman "9")
        , test "10" <|
            \_ -> Expect.equal (Ok "X") (arabicToRoman "10")
        , test "39" <|
            \_ -> Expect.equal (Ok "XXXIX") (arabicToRoman "39")
        , test "40" <|
            \_ -> Expect.equal (Ok "XL") (arabicToRoman "40")
        , test "50" <|
            \_ -> Expect.equal (Ok "L") (arabicToRoman "50")
        , test "90" <|
            \_ -> Expect.equal (Ok "XC") (arabicToRoman "90")
        , test "100" <|
            \_ -> Expect.equal (Ok "C") (arabicToRoman "100")
        , test "160" <|
            \_ -> Expect.equal (Ok "CLX") (arabicToRoman "160")
        , test "207" <|
            \_ -> Expect.equal (Ok "CCVII") (arabicToRoman "207")
        , test "246" <|
            \_ -> Expect.equal (Ok "CCXLVI") (arabicToRoman "246")
        , test "789" <|
            \_ -> Expect.equal (Ok "DCCLXXXIX") (arabicToRoman "789")
        , test "1009" <|
            \_ -> Expect.equal (Ok "MIX") (arabicToRoman "1009")
        , test "1066" <|
            \_ -> Expect.equal (Ok "MLXVI") (arabicToRoman "1066")
        , test "1776" <|
            \_ -> Expect.equal (Ok "MDCCLXXVI") (arabicToRoman "1776")
        , test "1918" <|
            \_ -> Expect.equal (Ok "MCMXVIII") (arabicToRoman "1918")
        , test "1954" <|
            \_ -> Expect.equal (Ok "MCMLIV") (arabicToRoman "1954")
        , test "2014" <|
            \_ -> Expect.equal (Ok "MMXIV") (arabicToRoman "2014")
        , test "2421" <|
            \_ -> Expect.equal (Ok "MMCDXXI") (arabicToRoman "2421")
        , test "3999" <|
            \_ -> Expect.equal (Ok "MMMCMXCIX") (arabicToRoman "3999")
        , test "0" <|
            \_ ->
                Expect.equal
                    (Err "Number (0) must be > 1 and < 4000")
                    (arabicToRoman "0")
        , test "4000" <|
            \_ ->
                Expect.equal
                    (Err "Number (4000) must be > 1 and < 4000")
                    (arabicToRoman "4000")
        , test "3.14" <|
            \_ ->
                Expect.equal
                    (Err "\"3.14\" is not an integer")
                    (arabicToRoman "3.14")
        ]
