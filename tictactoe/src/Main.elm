module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, a, br, div, h1, img, table, td, text, tr)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Set



---- MODEL ----


type Player
    = X
    | O


type alias Cell =
    { num : String
    , player : Maybe Player
    }


type alias Cells =
    { one : Cell
    , two : Cell
    , three : Cell
    , four : Cell
    , five : Cell
    , six : Cell
    , seven : Cell
    , eight : Cell
    , nine : Cell
    }


type alias Model =
    { currentPlayer : Player
    , cells : Cells
    , winner : Maybe Player
    , numWinsX : Int
    , numWinsO : Int
    }


initialModel =
    { currentPlayer = X
    , cells =
        { one = { num = "1", player = Nothing }
        , two = { num = "2", player = Nothing }
        , three = { num = "3", player = Nothing }
        , four = { num = "4", player = Nothing }
        , five = { num = "5", player = Nothing }
        , six = { num = "6", player = Nothing }
        , seven = { num = "7", player = Nothing }
        , eight = { num = "8", player = Nothing }
        , nine = { num = "9", player = Nothing }
        }
    , winner = Nothing
    , numWinsX = 0
    , numWinsO = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Take String
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { initialModel
                | numWinsX = model.numWinsX
                , numWinsO = model.numWinsO
              }
            , Cmd.none
            )

        Take cellNumber ->
            let
                cells =
                    model.cells

                otherPlayer =
                    case model.currentPlayer of
                        X ->
                            O

                        O ->
                            X

                player =
                    model.currentPlayer

                setPlayer cell =
                    { cell | player = Just model.currentPlayer }

                ( newCells, newPlayer ) =
                    case cellNumber of
                        "1" ->
                            ( { cells | one = setPlayer cells.one }
                            , otherPlayer
                            )

                        "2" ->
                            ( { cells | two = setPlayer cells.two }
                            , otherPlayer
                            )

                        "3" ->
                            ( { cells | three = setPlayer cells.three }
                            , otherPlayer
                            )

                        "4" ->
                            ( { cells | four = setPlayer cells.four }
                            , otherPlayer
                            )

                        "5" ->
                            ( { cells | five = setPlayer cells.five }
                            , otherPlayer
                            )

                        "6" ->
                            ( { cells | six = setPlayer cells.six }
                            , otherPlayer
                            )

                        "7" ->
                            ( { cells | seven = setPlayer cells.seven }
                            , otherPlayer
                            )

                        "8" ->
                            ( { cells | eight = setPlayer cells.eight }
                            , otherPlayer
                            )

                        "9" ->
                            ( { cells | nine = setPlayer cells.nine }
                            , otherPlayer
                            )

                        _ ->
                            ( cells, model.currentPlayer )

                newWinner =
                    findWinner newCells

                ( newWinsX, newWinsO ) =
                    case newWinner of
                        Just X ->
                            ( model.numWinsX + 1, model.numWinsO )

                        Just O ->
                            ( model.numWinsX, model.numWinsO + 1 )

                        _ ->
                            ( model.numWinsX, model.numWinsO )
            in
            ( { model
                | cells = newCells
                , currentPlayer = newPlayer
                , winner = newWinner
                , numWinsX = newWinsX
                , numWinsO = newWinsO
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        ( winnerName, reset ) =
            case model.winner of
                Just player ->
                    ( playerName player ++ " wins!"
                    , a [ onClick Reset ] [ text "Reset" ]
                    )

                _ ->
                    ( "No winner", text "" )
    in
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , text
            ("X Wins: "
                ++ String.fromInt model.numWinsX
                ++ ", O Wins: "
                ++ String.fromInt model.numWinsO
            )
        , br [] []
        , text ("Current Player: " ++ playerName model.currentPlayer)
        , br [] []
        , text ("Winner: " ++ winnerName)
        , br [] []
        , reset
        , br [] []
        , board model
        ]


playerName : Player -> String
playerName player =
    case player of
        X ->
            "X"

        O ->
            "O"


board : Model -> Html Msg
board model =
    let
        cells =
            model.cells

        displayCell cell =
            case cell.player of
                Just player ->
                    text (playerName player)

                _ ->
                    case model.winner of
                        Just _ ->
                            text cell.num

                        Nothing ->
                            a [ onClick (Take cell.num) ] [ text cell.num ]
    in
    table []
        [ tr []
            [ td [] [ displayCell cells.one ]
            , td [] [ displayCell cells.two ]
            , td [] [ displayCell cells.three ]
            ]
        , tr []
            [ td [] [ displayCell cells.four ]
            , td [] [ displayCell cells.five ]
            , td [] [ displayCell cells.six ]
            ]
        , tr []
            [ td [] [ displayCell cells.seven ]
            , td [] [ displayCell cells.eight ]
            , td [] [ displayCell cells.nine ]
            ]
        ]


findWinner : Cells -> Maybe Player
findWinner cells =
    let
        allCells =
            [ cells.one
            , cells.two
            , cells.three
            , cells.four
            , cells.five
            , cells.six
            , cells.seven
            , cells.eight
            , cells.nine
            ]

        isPlayer cell player =
            if cell.player == Just player then
                Just cell.num

            else
                Nothing

        xs =
            Set.fromList <|
                List.filterMap (\cell -> isPlayer cell X) allCells

        os =
            Set.fromList <|
                List.filterMap (\cell -> isPlayer cell O) allCells

        winningCombos =
            [ Set.fromList [ "1", "2", "3" ]
            , Set.fromList [ "4", "5", "6" ]
            , Set.fromList [ "7", "8", "9" ]
            , Set.fromList [ "1", "4", "7" ]
            , Set.fromList [ "2", "5", "8" ]
            , Set.fromList [ "3", "6", "9" ]
            , Set.fromList [ "1", "5", "9" ]
            , Set.fromList [ "3", "5", "7" ]
            ]

        xWins =
            List.any
                (\combo -> Set.size (Set.intersect combo xs) == 3)
                winningCombos

        oWins =
            List.any
                (\combo -> Set.size (Set.intersect combo os) == 3)
                winningCombos
    in
    if xWins then
        Just X

    else if oWins then
        Just O

    else
        Nothing



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
