module Faction exposing (..)


type Faction
    = Faction1
    | Faction2
    | Faction3
    | Faction4


getFaction : Int -> Faction
getFaction i =
    if i == 0 then
        Faction1

    else if i == 1 then
        Faction2

    else if i == 2 then
        Faction3

    else
        Faction4


factionName : Faction -> String
factionName faction =
    case faction of
        Faction1 ->
            "Faction1"

        Faction2 ->
            "Faction2"

        Faction3 ->
            "Faction3"

        Faction4 ->
            "Faction4"
