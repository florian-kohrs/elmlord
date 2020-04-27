module Fraction exposing (..)


type Fraction
    = Fraction1
    | Fraction2
    | Fraction3


fractionName : Fraction -> String
fractionName fraction =
    case fraction of
        Fraction1 ->
            "Fraction1"

        Fraction2 ->
            "Fraction2"

        Fraction3 ->
            "Fraction3"
