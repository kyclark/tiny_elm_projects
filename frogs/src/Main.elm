module Main exposing (Model, Msg(..), Slot(..), SlotNum, defaultNumberOfFrogs, handleClick, init, initialModel, leftFrogLeap, main, numberOption, rangeOfFrogs, rightFrogLeap, subscriptions, update, view, viewSlot, viewSlots, wonYet)

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
    = SlotClick SlotNum
    | Undo
    | Reset
    | ChangeNumberOfFrogs String


type alias Model =
    { slots : List Slot
    , goal : List Slot
    , msg : String
    , previousStates : List (List Slot)
    , numberOfMoves : Int
    , numberOfFrogs : Int
    }


initialModel : Int -> Model
initialModel numberOfFrogs =
    let
        slots =
            List.repeat numberOfFrogs LeftFrog
                ++ [ Space ]
                ++ List.repeat numberOfFrogs RightFrog
    in
    { slots = slots
    , msg =
        "Click on a frog to make it move. Frogs can only "
            ++ "move in the direction they are facing and only to an "
            ++ "adjacent open space or over another frog to the space."
    , previousStates = []
    , numberOfMoves = 0
    , numberOfFrogs = numberOfFrogs
    , goal = List.reverse slots
    }


rangeOfFrogs : List Int
rangeOfFrogs =
    List.range 3 6


defaultNumberOfFrogs : Int
defaultNumberOfFrogs =
    3


init : flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel defaultNumberOfFrogs, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


numberOption : Int -> Html msg
numberOption n =
    let
        v =
            String.fromInt n
    in
    option [ value v ] [ text v ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewSlots model.slots ]
        , div [] [ text model.msg ]
        , div []
            [ text "Number of frogs: "
            , select [ onInput ChangeNumberOfFrogs ] (List.map numberOption (List.range 3 5))
            ]
        , div [] [ text ("Number of moves: " ++ String.fromInt model.numberOfMoves) ]
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
        imgName =
            case slot of
                LeftFrog ->
                    "left-frog.png"

                RightFrog ->
                    "right-frog.png"

                Space ->
                    "space.png"

        altText =
            case slot of
                LeftFrog ->
                    "Left"

                RightFrog ->
                    "Right"

                Space ->
                    "Space"
    in
    td []
        [ img
            [ src ("./img/" ++ imgName)
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
    let
        foo =
            Debug.log "slots" slots
    in
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
                                    ( x, List.take window y, List.drop window y )

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
                Just i ->
                    let
                        min =
                            case List.head rangeOfFrogs of
                                Just x ->
                                    x

                                _ ->
                                    0

                        max =
                            case List.head (List.reverse rangeOfFrogs) of
                                Just x ->
                                    x

                                _ ->
                                    0
                    in
                    case ( i >= min, i <= max ) of
                        ( True, True ) ->
                            ( initialModel i, Cmd.none )

                        _ ->
                            ( { model | msg = "Not a good number of frogs." }, Cmd.none )

                Nothing ->
                    ( { model | msg = "Cannot convert " ++ n ++ " an integer" }, Cmd.none )

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

        Undo ->
            case List.length model.previousStates of
                0 ->
                    ( { model | msg = "No previous state" }, Cmd.none )

                _ ->
                    ( { model
                        | slots = List.concat <| List.take 1 model.previousStates
                        , previousStates = List.drop 1 model.previousStates
                        , msg = "Undo"
                        , numberOfMoves = model.numberOfMoves + 1
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
                , msg = newMessage
                , previousStates = model.slots :: model.previousStates
                , numberOfMoves = model.numberOfMoves + 1
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
