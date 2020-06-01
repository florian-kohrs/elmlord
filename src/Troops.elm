module Troops exposing (..)

type alias Troop =
    {
        amount: Int
        , troopType: TroopType
    }

type TroopType
    = Archer
    | Spear
    | Sword
    | Knight

troopTypeList : List TroopType
troopTypeList = [Archer, Spear, Sword, Knight]

troopCost : TroopType -> Float
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

troopWage : TroopType -> Float
troopWage t =
    case t of
        Archer ->
            0.4

        Spear ->
            0.2

        Sword ->
            0.5

        Knight ->
            1.0


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

