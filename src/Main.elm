port module Main exposing (main)

import Browser exposing (Document)
import DateTime
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import Field exposing (Field)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra
import Update exposing (UpdateTuple)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading LoadingModel
    | Running RunningModel


type alias LoadingModel =
    { countdowns : Dict String Posix
    , windowWidth : Int
    }


type alias RunningModel =
    { countdowns : Dict String Posix
    , newCountdown : NewCountdown
    , windowWidth : Int
    , now : Posix
    , zone : Zone
    }


type alias NewCountdown =
    { name : Field String String
    , date : Field String ( Int, Month, Int )
    , time : Field String ( Int, Int )
    }


type alias Flags =
    { windowWidth : Float
    , saveData : Value
    }


init : Flags -> ( Model, Cmd Msg )
init { windowWidth, saveData } =
    ( Loading
        { countdowns =
            case Json.Decode.decodeValue decodeCountdowns saveData of
                Ok countdowns ->
                    countdowns

                Err _ ->
                    Dict.empty
        , windowWidth = floor windowWidth
        }
    , Task.map2 InitializeApp
        Time.here
        Time.now
        |> Task.perform identity
    )


decodeCountdowns : Decoder (Dict String Posix)
decodeCountdowns =
    Json.Decode.dict decodePosix


decodePosix : Decoder Posix
decodePosix =
    Json.Decode.map Time.millisToPosix Json.Decode.int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60 * 1000) Tick



---- UPDATE


port save : Value -> Cmd msg


encodeCountdowns : Dict String Posix -> Value
encodeCountdowns =
    Dict.toList
        >> List.map (Tuple.mapSecond encodePosix)
        >> Json.Encode.object


encodePosix : Posix -> Value
encodePosix =
    Time.posixToMillis >> Json.Encode.int


type Msg
    = WindowResize Int Int
    | Tick Posix
    | InitializeApp Zone Posix
    | SetName String
    | SetDate String
    | SetTime String
    | AddCountdown
    | DeleteCountdown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            Update.save (Update.updateHelper updateLoading msg loadingModel)

        Running runningModel ->
            Update.save (Update.updateHelper updateRunning msg runningModel)


updateLoading : Msg -> UpdateTuple LoadingModel Msg -> UpdateTuple Model Msg
updateLoading msg =
    case msg of
        InitializeApp zone now ->
            Update.map
                (\model ->
                    Running
                        { countdowns = model.countdowns
                        , newCountdown = initializedNewCountdown zone now
                        , windowWidth = model.windowWidth
                        , now = now
                        , zone = zone
                        }
                )

        _ ->
            Update.map Loading


updateRunning : Msg -> UpdateTuple RunningModel Msg -> UpdateTuple Model Msg
updateRunning msg =
    Update.map Running
        << (case msg of
                WindowResize width _ ->
                    Update.map (\model -> { model | windowWidth = width })

                Tick now ->
                    Update.map
                        (\model ->
                            { model
                                | now = now
                                , countdowns =
                                    Dict.filter
                                        (\_ timeUntil -> Time.posixToMillis now - Time.posixToMillis timeUntil <= 0)
                                        model.countdowns
                            }
                        )
                        >> saveCountdowns

                SetName name ->
                    Update.map
                        (\model ->
                            updateNewCountdown
                                (\newCountdown -> { newCountdown | name = Field.fromString (parseName model.countdowns) name })
                                model
                        )

                SetDate date ->
                    Update.map
                        (updateNewCountdown
                            (\newCountdown -> { newCountdown | date = Field.fromString DateTime.parseYearMonthDay date })
                        )

                SetTime time ->
                    Update.map
                        (updateNewCountdown
                            (\newCountdown -> { newCountdown | time = Field.fromString DateTime.parseHourMinute time })
                        )

                AddCountdown ->
                    Update.map
                        (\model ->
                            case
                                ( Field.getValue model.newCountdown.name
                                , Field.getValue model.newCountdown.date
                                , Field.getValue model.newCountdown.time
                                )
                            of
                                ( Just name, Just ( year, month, day ), Just ( hour, minute ) ) ->
                                    let
                                        timeUntil =
                                            Time.Extra.partsToPosix model.zone
                                                (Debug.log "parts"
                                                    { year = year
                                                    , month = month
                                                    , day = day
                                                    , hour = hour
                                                    , minute = minute
                                                    , second = 0
                                                    , millisecond = 0
                                                    }
                                                )
                                    in
                                    { model
                                        | countdowns = Dict.insert name timeUntil model.countdowns
                                        , newCountdown = initializedNewCountdown model.zone model.now
                                    }

                                _ ->
                                    model
                        )
                        >> saveCountdowns

                DeleteCountdown id ->
                    Update.map (\model -> { model | countdowns = Dict.remove id model.countdowns })
                        >> saveCountdowns

                _ ->
                    identity
           )


saveCountdowns : UpdateTuple RunningModel Msg -> UpdateTuple RunningModel Msg
saveCountdowns =
    Update.addMsg (.countdowns >> encodeCountdowns >> save)


updateNewCountdown : (NewCountdown -> NewCountdown) -> RunningModel -> RunningModel
updateNewCountdown fn model =
    { model | newCountdown = fn model.newCountdown }


initializedNewCountdown : Zone -> Posix -> NewCountdown
initializedNewCountdown zone time =
    { name = Field.init ""
    , date = Field.init (DateTime.posixToYearMonthDay zone time)
    , time = Field.init (DateTime.posixToHourMinute zone time)
    }


parseName : Dict String Posix -> String -> Result String String
parseName countdowns input =
    if String.isEmpty input then
        Err "A name is required"

    else
        case Dict.get input countdowns of
            Nothing ->
                Ok input

            Just _ ->
                Err "No duplicate names allowed"



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Until"
    , body =
        [ layout
            [ width fill
            , height fill
            , padding 16
            ]
            (case model of
                Loading loadingModel ->
                    viewLoading loadingModel

                Running runningModel ->
                    viewBody runningModel
            )
        ]
    }


viewLoading : LoadingModel -> Element Msg
viewLoading _ =
    text "Loading"


viewBody : RunningModel -> Element Msg
viewBody model =
    column
        [ width fill
        , spacing 16
        ]
        [ viewNewCountdown model.newCountdown
        , table
            [ spacing 8 ]
            { data =
                Dict.toList model.countdowns
                    |> List.sortBy (Tuple.second >> Time.posixToMillis)
            , columns =
                [ { header = none
                  , width = shrink
                  , view = Tuple.first >> text >> el [ centerY ]
                  }
                , { header = none
                  , width = fill
                  , view = Tuple.second >> viewCountdown model.now (findLongest model.now model.countdowns)
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \( name, _ ) ->
                            button
                                { label = text "Delete"
                                , onPress = Just (DeleteCountdown name)
                                }
                  }
                ]
            }
        ]


findLongest : Posix -> Dict String Posix -> Int
findLongest now countdowns =
    Dict.foldl
        (\_ timeUntil longest ->
            let
                nowMillis =
                    Time.posixToMillis now

                timeUntilMillis =
                    Time.posixToMillis timeUntil

                remainingTime =
                    timeUntilMillis - nowMillis
            in
            if remainingTime > longest then
                remainingTime

            else
                longest
        )
        0
        countdowns


viewNewCountdown : NewCountdown -> Element Msg
viewNewCountdown { name, date, time } =
    row
        [ spacing 16 ]
        [ viewField
            { input =
                \value ->
                    Input.text
                        []
                        { label = Input.labelAbove [] (text "Name")
                        , placeholder = Nothing
                        , text = value
                        , onChange = SetName
                        }
            , errorToString = identity
            , value = name
            }
        , viewField
            { input =
                \value ->
                    Element.Extra.date
                        []
                        { label = "Date"
                        , date = value
                        , onChange = SetDate
                        }
            , errorToString = identity
            , value = date
            }
        , viewField
            { input =
                \value ->
                    Element.Extra.time
                        []
                        { label = "Time"
                        , time = value
                        , onChange = SetTime
                        }
            , errorToString = identity
            , value = time
            }
        , button
            { label = text "Add"
            , onPress =
                if Field.isValid name && Field.isValid date && Field.isValid time then
                    Just AddCountdown

                else
                    Nothing
            }
        ]


button : { onPress : Maybe msg, label : Element msg } -> Element msg
button =
    Input.button
        [ Border.solid
        , Border.width 3
        , paddingXY 16 8
        ]


viewField : { input : String -> Element msg, errorToString : err -> String, value : Field err a } -> Element msg
viewField { input, errorToString, value } =
    column
        [ width fill, spacing 8, alignTop ]
        [ input (Field.getInput value)
        , case Field.getError value of
            Nothing ->
                none

            Just err ->
                paragraph [ Font.color (rgb 1 0 0) ] [ text (errorToString err) ]
        ]


viewCountdown : Posix -> Int -> Posix -> Element Msg
viewCountdown now longest timeUntil =
    let
        nowMillis =
            Time.posixToMillis now

        remainingTime =
            Time.posixToMillis timeUntil - nowMillis

        remainingWidth =
            floor (toFloat remainingTime / toFloat longest * percentageScale)
    in
    column
        [ width fill, moveDown 16 ]
        [ el
            [ height (px 16)
            , width fill
            , Border.solid
            , Border.widthEach
                { top = 0
                , bottom = 2
                , left = 2
                , right = 2
                }
            ]
            none
        , row
            [ height (px 16)
            , width fill
            , moveUp 16
            ]
            (if remainingWidth > 0 then
                [ el
                    [ height fill
                    , width (fillPortion (max remainingWidth 1))
                    , Background.color (rgb 0.2 0.7 0.4)
                    ]
                    none
                , el [ width (fillPortion (percentageScale - remainingWidth)) ] none
                ]

             else
                [ el
                    [ height fill
                    , width fill
                    , Background.color (rgb 0.9 0.4 0.4)
                    ]
                    none
                ]
            )
        ]


percentageScale : number
percentageScale =
    1000
