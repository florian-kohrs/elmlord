module Troops exposing (..)


type TroopType
    = Archer
    | Spear
    | Sword
    | Knight


troopCost : TroopType -> Int
troopCost t =
    case t of
        Archer ->
            50

        Spear ->
            45

        Sword ->
            60

        Knight ->
            120


troopName : TroopType -> String
troopName t =
    case t of
        Archer ->
            "Archer"

        Spear ->
            "Spear"

        Sword ->
            "Sword"

        Knight ->
            "Knight"
