module Faction exposing (..)


type Faction
    = Faction1
    | Faction2
    | Faction3


factionName : Faction -> String
factionName faction =
    case faction of
        Faction1 ->
            "Faction1"

        Faction2 ->
            "Faction2"

        Faction3 ->
            "Faction3"
