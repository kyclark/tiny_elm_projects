module Bagles exposing (bagles, evaluate)

import List.Extra exposing (getAt, zip)
import Maybe.Extra exposing (isNothing)


bagles : String -> String -> Result String String
bagles secret guess =
    let
        secretChars =
            String.toList secret

        results =
            List.map
                (\( pos, char ) -> evaluate char pos secretChars)
            <|
                zip (List.range 0 2)
                    (String.toList guess)

        display =
            if List.all isNothing results then
                "BAGLES"

            else
                String.trim <|
                    String.join " "
                        (List.map (Maybe.withDefault "") results)
    in
    case secret == guess of
        True ->
            Ok "You guessed it!"

        _ ->
            Err display


evaluate : Char -> Int -> List Char -> Maybe String
evaluate char pos secret =
    if getAt pos secret == Just char then
        Just "FERMI"

    else if List.member char secret then
        Just "PICO"

    else
        Nothing
