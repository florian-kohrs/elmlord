module Troops exposing (..)

import OperatorExt


type alias Troop =
    { amount : Int
    , troopType : TroopType
    }


type TroopType
    = Archer
    | Spear
    | Sword
    | Knight


troopTypeList : List TroopType
troopTypeList =
    [ Sword, Spear, Archer, Knight ]


updateTroops : List Troop -> TroopType -> Int -> List Troop
updateTroops tr ty v =
    case tr of
        [] ->
            []

        x :: xs ->
            OperatorExt.ternary (x.troopType == ty) { x | amount = x.amount + v } x :: updateTroops xs ty v



troopDifferences : Troop -> Troop -> Troop
troopDifferences t1 t2 =
    { amount = t2.amount - t1.amount
    , troopType = t1.troopType
    }


emptyTroops : List Troop
emptyTroops =
    [ { amount = 0, troopType = Sword }
    , { amount = 0, troopType = Spear }
    , { amount = 0, troopType = Archer }
    , { amount = 0, troopType = Knight }
    ]


sumTroops : List Troop -> Float
sumTroops l =
    List.foldr (+) 0.0 (List.map (\x -> toFloat x.amount) l)



-- Resolve a troop type to different, for values like
-- wages, costs, fighting-stats, etc.
----------------------------------------------------------


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
            15

        Spear ->
            10

        Sword ->
            12

        Knight ->
            25


troopDefense : TroopType -> Float
troopDefense t =
    case t of
        Archer ->
            30

        Spear ->
            50

        Sword ->
            70

        Knight ->
            100


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


battlefieldBonus : TroopType -> Float
battlefieldBonus t =
    case t of
        Archer ->
            1.15

        Spear ->
            1.25

        Sword ->
            1.2

        Knight ->
            1.2
