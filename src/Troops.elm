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
troopTypeList = [Sword, Spear, Archer, Knight]

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

troopDamage : TroopType -> Float
troopDamage t =
    case t of
        Archer ->
            4

        Spear ->
            2

        Sword ->
            3

        Knight ->
            6

troopDefense : TroopType -> Float
troopDefense t =
    case t of
        Archer ->
            3

        Spear ->
            5

        Sword ->
            7

        Knight ->
            10


troopPriority : TroopType -> Float
troopPriority t =
    case t of
        Archer ->
            0.15

        Spear ->
            0.3

        Sword ->
            0.25

        Knight ->
            0.3

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


    