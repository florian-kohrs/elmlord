module Troops exposing (..)
import OperatorExt

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


updateTroops : List Troop -> TroopType -> Int -> List Troop
updateTroops tr ty v =
        case tr of 
            [] ->
                []

            (x :: xs) -> 
                OperatorExt.ternary (x.troopType == ty) {x | amount = x.amount + v} x :: updateTroops xs ty v
    
checkTroopTreshhold : List Troop -> TroopType -> Int -> Bool
checkTroopTreshhold tr ty v =
        case tr of 
            [] ->
                False

            (x :: xs) -> 
                x.amount - v >= 0 ||  checkTroopTreshhold xs ty v

filterTroopList : List Troop -> TroopType -> List Troop
filterTroopList l t =
            List.filter (\x -> x.troopType == t) l