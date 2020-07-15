module Troops exposing (..)

import Dict
import OperatorExt


type alias Army =
    Dict.Dict Int Int


type TroopType
    = Archer
    | Spear
    | Sword
    | Knight


mergeTroops : Army -> Army -> Army
mergeTroops a1 a2 =
    Dict.merge Dict.insert (\k v1 v2 r -> Dict.insert k (v1 + v2) r) Dict.insert a1 a2 Dict.empty


troopTypeToInt : TroopType -> Int
troopTypeToInt t =
    case t of
        Archer ->
            0

        Spear ->
            1

        Sword ->
            2

        Knight ->
            3


intToTroopType : Int -> TroopType
intToTroopType i =
    case i of
        0 ->
            Archer

        1 ->
            Spear

        2 ->
            Sword

        3 ->
            Knight

        _ ->
            Spear


troopTypeList : List TroopType
troopTypeList =
    [ Sword, Spear, Archer, Knight ]


updateTroops : Army -> TroopType -> Int -> Army
updateTroops army t i =
    Dict.update (troopTypeToInt t) (\v -> Just (Maybe.withDefault 0 v + i)) army


emptyTroops : Army
emptyTroops =
    List.foldl (\t dict -> Dict.insert (troopTypeToInt t) 0 dict) Dict.empty troopTypeList


startTroops : Army
startTroops =
    List.foldl (\( t, v ) dict -> Dict.insert (troopTypeToInt t) v dict)
        Dict.empty
        [ ( Archer, 10 ), ( Spear, 45 ), ( Sword, 20 ), ( Knight, 5 ) ]


sumTroops : Army -> Int
sumTroops a =
    List.foldl (+) 0 (Dict.values a)


sumTroopStats : Army -> Int
sumTroopStats =
    Dict.foldl (\k v r -> round (troopDamage (intToTroopType k) + troopDefense (intToTroopType k)) * v + r) 0



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
