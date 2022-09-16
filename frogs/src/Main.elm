module Main exposing (Model, Msg(..), Slot(..), SlotNum, handleClick, init, initialModel, leftFrogLeap, main, rangeOfFrogs, rightFrogLeap, subscriptions, update, view, viewSlot, viewSlots, wonYet)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra
import Url


type Slot
    = LeftFrog
    | RightFrog
    | Space


type alias SlotNum =
    Int


type Msg
    = Reset
    | SlotClick SlotNum
    | Undo
    | ChangeNumberOfFrogs String


type alias Model =
    { slots : List Slot
    , goal : List Slot
    , message : String
    , previousStates : List (List Slot)
    , numberOfMoves : Int
    , numberOfFrogs : Int
    }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel 3, Cmd.none )


initialModel : Int -> Model
initialModel numberOfFrogs =
    let
        slots =
            List.repeat numberOfFrogs LeftFrog
                ++ [ Space ]
                ++ List.repeat numberOfFrogs RightFrog
    in
    { slots = slots
    , goal = List.reverse slots
    , message = "Click on a frog to make it move."
    , previousStates = []
    , numberOfMoves = 0
    , numberOfFrogs = numberOfFrogs
    }


rangeOfFrogs : List Int
rangeOfFrogs =
    List.range 3 6


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view model =
    let
        numberOption n =
            let
                v =
                    String.fromInt n
            in
            option [ value v ] [ text v ]
    in
    div []
        [ div [] [ viewSlots model.slots ]
        , div [] [ text model.message ]
        , div []
            [ text "Number of frogs: "
            , select [ onInput ChangeNumberOfFrogs ]
                (List.map numberOption (List.range 3 5))
            ]
        , div []
            [ text
                ("Number of moves: " ++ String.fromInt model.numberOfMoves)
            ]
        , div []
            [ button [ onClick Undo ] [ text "Undo" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div [] [ text (wonYet model) ]
        ]


wonYet model =
    case model.slots == model.goal of
        True ->
            "YOU WON!"

        _ ->
            ""


viewSlots : List Slot -> Html Msg
viewSlots slots =
    table []
        [ tr []
            (List.indexedMap viewSlot slots)
        ]


viewSlot : SlotNum -> Slot -> Html Msg
viewSlot index slot =
    let
        ( imgName, altText ) =
            case slot of
                LeftFrog ->
                    ( "./img/left-frog.png", "Left" )

                RightFrog ->
                    ( "./img/right-frog.png", "Right" )

                Space ->
                    ( "./img/space.png", "Space" )
    in
    td []
        [ img
            [ src imgName
            , alt altText
            , onClick (SlotClick index)
            ]
            []
        , text (String.fromInt index)
        ]


leftFrogLeap slots =
    case slots of
        x :: Space :: y :: [] ->
            [ Space, x, y ]

        x :: y :: Space :: [] ->
            [ Space, y, x ]

        x :: Space :: [] ->
            [ Space, x ]

        _ ->
            slots


rightFrogLeap slots =
    case slots of
        x :: Space :: y :: [] ->
            [ x, y, Space ]

        Space :: x :: y :: [] ->
            [ y, x, Space ]

        Space :: x :: [] ->
            [ x, Space ]

        _ ->
            slots


handleClick : SlotNum -> List Slot -> ( List Slot, String )
handleClick index slots =
    let
        thisSlot =
            List.Extra.getAt index slots
    in
    case thisSlot of
        Nothing ->
            ( slots, "Cannot move non-existing slot" )

        Just Space ->
            ( slots, "Cannot move the space" )

        _ ->
            let
                window =
                    3

                ( slotsBefore, leapSlots, slotsAfter ) =
                    case thisSlot of
                        Just LeftFrog ->
                            let
                                ( before, at ) =
                                    List.Extra.splitAt index slots
                            in
                            ( before, List.take window at, List.drop window at )

                        Just RightFrog ->
                            case index of
                                0 ->
                                    ( [], [], slots )

                                1 ->
                                    ( []
                                    , List.take 2 slots
                                    , List.drop 2 slots
                                    )

                                _ ->
                                    let
                                        twoBefore =
                                            index - (window - 1)

                                        ( x, y ) =
                                            List.Extra.splitAt twoBefore slots
                                    in
                                    ( x
                                    , List.take window y
                                    , List.drop window y
                                    )

                        _ ->
                            ( [], [], [] )

                message =
                    case List.member Space leapSlots of
                        True ->
                            ""

                        _ ->
                            "No place for the frog to move!"

                newLeapSlots =
                    case thisSlot of
                        Just LeftFrog ->
                            leftFrogLeap leapSlots

                        Just RightFrog ->
                            rightFrogLeap leapSlots

                        _ ->
                            []
            in
            ( slotsBefore ++ newLeapSlots ++ slotsAfter, message )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNumberOfFrogs n ->
            case String.toInt n of
                Just newNumber ->
                    let
                        min =
                            Maybe.withDefault 0 <| List.head rangeOfFrogs

                        max =
                            Maybe.withDefault 0 <|
                                List.head (List.reverse rangeOfFrogs)
                    in
                    if newNumber >= min && newNumber <= max then
                        ( initialModel newNumber, Cmd.none )

                    else
                        ( { model | message = "Not a good number of frogs." }
                        , Cmd.none
                        )

                Nothing ->
                    ( { model
                        | message = "Cannot convert " ++ n ++ " an integer"
                      }
                    , Cmd.none
                    )

        Reset ->
            let
                newModel =
                    initialModel model.numberOfFrogs
            in
            ( { newModel
                | numberOfMoves = model.numberOfMoves + 1
              }
            , Cmd.none
            )

        SlotClick index ->
            let
                ( newSlots, errorMsg ) =
                    handleClick index model.slots

                newMessage =
                    case errorMsg of
                        "" ->
                            "Move frog " ++ String.fromInt index

                        _ ->
                            errorMsg
            in
            ( { model
                | slots = newSlots
                , message = newMessage
                , previousStates = model.slots :: model.previousStates
                , numberOfMoves = model.numberOfMoves + 1
              }
            , Cmd.none
            )

        Undo ->
            case List.length model.previousStates of
                0 ->
                    ( { model | message = "No previous state" }, Cmd.none )

                _ ->
                    ( { model
                        | slots = List.concat <| List.take 1 model.previousStates
                        , previousStates = List.drop 1 model.previousStates
                        , message = "Undo"
                        , numberOfMoves = model.numberOfMoves + 1
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
