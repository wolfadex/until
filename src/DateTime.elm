module DateTime exposing
    ( parseHourMinute
    , parseYearMonthDay
    , posixToHourMinute
    , posixToYearMonthDay
    )

import Time exposing (Month(..), Posix, Zone)


parseYearMonthDay : String -> Result String ( Int, Month, Int )
parseYearMonthDay input =
    case String.split "-" input of
        [ yearStr, monthStr, dayStr ] ->
            case
                ( String.toInt yearStr
                , String.toInt monthStr |> Maybe.andThen monthFromInt
                , String.toInt dayStr
                )
            of
                ( Just year, Just month, Just day ) ->
                    Ok ( year, month, day )

                ( Nothing, _, _ ) ->
                    Err ("Expect year to be an integer but found " ++ yearStr)

                ( _, Nothing, _ ) ->
                    Err ("Expect month to be an integer but found " ++ monthStr)

                ( _, _, Nothing ) ->
                    Err ("Expect day to be an integer but found " ++ dayStr)

        _ ->
            Err ("Expected a date in the format YYYY-MM-DD but found " ++ input)


posixToYearMonthDay : Zone -> Posix -> String
posixToYearMonthDay zone time =
    let
        year =
            Time.toYear zone time
                |> String.fromInt

        month =
            Time.toMonth zone time
                |> monthToInt
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay zone time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    String.join "-" [ year, month, day ]


parseHourMinute : String -> Result String ( Int, Int )
parseHourMinute input =
    case String.split ":" input of
        [ hourStr, minStr ] ->
            case ( String.toInt hourStr, String.toInt minStr ) of
                ( Just hour, Just minute ) ->
                    Ok ( hour, minute )

                ( Nothing, _ ) ->
                    Err ("Expected hour to be an integer but found " ++ hourStr)

                ( _, Nothing ) ->
                    Err ("Expected minute to be an integer but found " ++ minStr)

        _ ->
            Err ("Expected a time in the format HH:mm but found " ++ input)


posixToHourMinute : Zone -> Posix -> String
posixToHourMinute zone time =
    let
        hour =
            Time.toHour zone time
                |> String.fromInt

        minute =
            Time.toMinute zone time
                |> String.fromInt
    in
    String.join ":" [ hour, minute ]



---- MONTH HELPERS


monthFromInt : Int -> Maybe Month
monthFromInt val =
    case val of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
