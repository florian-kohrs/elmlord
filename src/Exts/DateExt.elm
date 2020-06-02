module DateExt exposing (..)


type alias Date =
    { year : Int, month : Month }


showDate : Date -> String
showDate date =
    showMonth date.month ++ " " ++ String.fromInt date.year ++ " AD"


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


monthToInt : Month -> Int
monthToInt m =
    case m of
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


intToMonth : Int -> Month
intToMonth i =
    case modBy 12 i of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


addMonths : Int -> Date -> Date
addMonths i date =
    let
        newMonth =
            intToMonth (monthToInt date.month + i)

        newYears =
            (monthToInt date.month + i - 1) // 12
    in
    Date (date.year + newYears) newMonth


showMonth : Month -> String
showMonth m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "Juni"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
