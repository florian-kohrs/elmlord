module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
import OperatorExt exposing (ternary)
import PathAgent
import Pathfinder
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Float


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , land : List Settlement
    , agent : PathAgent.Agent
    }


type alias BattleStats =
    { player : Lord
    , enemy : Lord
    , round : Int
    , playerCasualties : List Troop
    , enemyCasualties : List Troop
    , finished : Bool
    }


type LordList
    = Cons Lord (List Lord)


type alias Settlement =
    { entity : WorldEntity
    , settlementType : SettlementType
    , income : Float
    , isSieged : Bool
    }


type alias WorldEntity =
    { army : List Troop
    , faction : Faction
    , position : Point
    , name : String
    }


type SettlementType
    = Village
    | Castle


type alias SettlementInfo =
    { sType : SettlementType
    , position : Vector.Point
    , name : String
    , faction : Faction
    }



-- temp before refactoring


buyTroops : Lord -> TroopType -> Lord
buyTroops l t =
    handleTroopInteraction
        (l.gold - Troops.troopCost t > 0)
        { l | gold = l.gold - Troops.troopCost t, entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t 5) l.entity }
        l


stationTroops : Lord -> TroopType -> Settlement -> Lord
stationTroops l t s =
    handleTroopInteraction
        (checkTroopTreshhold (Troops.filterTroopList l.entity.army t) t 5)
        { l | entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t -5) l.entity, land = updateSettlementTroops l.land s.entity.name t 5 }
        l


takeTroops : Lord -> TroopType -> Settlement -> Lord
takeTroops l t s =
    handleTroopInteraction
        (checkTroopTreshhold (Troops.filterTroopList s.entity.army t) t 5)
        { l | entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t 5) l.entity, land = updateSettlementTroops l.land s.entity.name t -5 }
        l


handleTroopInteraction : Bool -> Lord -> Lord -> Lord
handleTroopInteraction bool l1 l2 =
    ternary bool l1 l2


updateEntitiesArmy : List Troop -> WorldEntity -> WorldEntity
updateEntitiesArmy l e =
    { e | army = l }


updatePlayerArmy : Lord -> List Troop -> Lord
updatePlayerArmy l t =
    { l | entity = updateEntitiesArmy t l.entity }


updateSettlementTroops : List Settlement -> String -> TroopType -> Int -> List Settlement
updateSettlementTroops l s t a =
    List.map (\x -> { x | entity = updateEntitiesArmy (Troops.updateTroops x.entity.army t a) x.entity }) (List.filter (\y -> y.entity.name == s) l)



{- updateLord : Lord -> Lord
   updateLord s =
       let
           goldIncome =
               foldl (cons ((+) 5)) land 0
       in
       { s | gold = s.gold + goldIncome }
-}
-- https://www.fantasynamegenerators.com/town_names.php
-- around 15 Castle names


castleNames : List String
castleNames =
    [ "Stathford"
    , "Wingston"
    , "Boroughton"
    , "Peterbrugh"
    , "Wimborne"
    , "Westwend"
    , "Kingcardine"
    , "Helmfirth"
    , "Accrington"
    , "Mournstead"
    , "Alcombey"
    , "Aeberuthey"
    , "Bradford"
    , "Bamborourgh"
    , "Everton"
    ]



-- around 30 Villages names


villageNames : List String
villageNames =
    [ "Haran"
    , "Hillfar"
    , "Waekefield"
    , "Sudbury"
    , "Murkwell"
    , "Caerfyrddin"
    , "Llanybydder"
    , "Galssop"
    , "Farnworth"
    , "Porthaethwy"
    , "Favorsham"
    , "Kilead"
    , "Kald"
    , "Holsworthy"
    , "Wolfwater"
    , "Southwold"
    , "Marnmouth"
    , "Kilmarnock"
    , "Far Water"
    , "Aylesbury"
    , "Dornwich"
    , "Haran"
    , "Murkwell"
    , "Drumnacanvy"
    , "Waeldestone"
    , "Bracklewhyte"
    , "Peatsland"
    , "Ballachulish"
    , "Arbington"
    , "Torrine"
    ]


npcs : LordList -> List Lord
npcs (Cons _ ls) =
    ls


mapLordList : (Lord -> Lord) -> LordList -> LordList
mapLordList f (Cons p ps) =
    Cons (f p) (List.map f ps)


getPlayer : LordList -> Lord
getPlayer (Cons p _) =
    p


isLordInOwnSettlement : Lord -> Bool
isLordInOwnSettlement lord =
    List.any ((==) lord.entity.position) (List.map (\s -> s.entity.position) lord.land)


isLordOnSettlement : Lord -> Settlement -> Bool
isLordOnSettlement lord s =
    lord.entity.position == s.entity.position


resetUsedMovement : Lord -> Lord
resetUsedMovement lord =
    { lord | agent = PathAgent.resetUsedMovement lord.agent }


getLordRemainingMovement : Lord -> Float
getLordRemainingMovement l =
    PathAgent.remainingMovement l.agent


getSettlement : List Settlement -> String -> Maybe Settlement
getSettlement l s =
    case List.filter (\x -> x.entity.name == s) l of
        [] ->
            Nothing

        x :: _ ->
            Just x


updatePlayer : LordList -> Lord -> LordList
updatePlayer (Cons _ ps) np =
    Cons np ps


tailLordList : LordList -> List Lord
tailLordList (Cons _ ps) =
    ps


updateEnemyLord : LordList -> List Lord -> LordList
updateEnemyLord (Cons p _) pss =
    Cons p pss


flattenLordList : LordList -> List Lord
flattenLordList (Cons p ps) =
    p :: ps


getLordByName : LordList -> String -> Maybe Lord
getLordByName l str =
    case List.filter (\x -> x.entity.name == str) (flattenLordList l) of
        [] ->
            Nothing

        x :: _ ->
            Just x


getSettlementByName : List Settlement -> String -> Maybe Settlement
getSettlementByName l s =
    case List.filter (\x -> x.entity.name == s) l of
        [] ->
            Nothing

        x :: _ ->
            Just x


setPosition : WorldEntity -> Vector.Point -> WorldEntity
setPosition entity pos =
    { entity | position = pos }


applyLordGoldIncome : Lord -> Lord
applyLordGoldIncome lord =
    { lord | gold = lord.gold + calculateRoundIncome lord }


createCapitalFor : WorldEntity -> String -> Settlement
createCapitalFor e name =
    { entity = { army = [], faction = e.faction, position = e.position, name = name }, settlementType = Castle, income = 1.0, isSieged = False }


editSettlmentInfoPosition : Vector.Point -> SettlementInfo -> SettlementInfo
editSettlmentInfoPosition p i =
    { i | position = p }


getSettlementFor : SettlementInfo -> Settlement
getSettlementFor info =
    { entity = { army = [], faction = info.faction, position = info.position, name = info.name }, settlementType = info.sType, income = 1.0, isSieged = False }


combineSettlementName : Settlement -> String
combineSettlementName settlement =
    getSettlementNameByType settlement.settlementType ++ " - " ++ settlement.entity.name


getSettlementNameByType : SettlementType -> String
getSettlementNameByType s =
    case s of
        Village ->
            "Village"

        Castle ->
            "Castle"


settlementImageName : SettlementType -> String
settlementImageName s =
    getSettlementNameByType s ++ ".png"



-- calc income


calculateRoundIncome : Lord -> Float
calculateRoundIncome lord =
    sumSettlementsIncome lord.land - sumTroopWages (flattenTroops (sumLordTroops lord) Troops.troopTypeList)


sumSettlementsIncome : List Settlement -> Float
sumSettlementsIncome s =
    foldr (\x v -> x.income + v) 0 s


sumTroopWages : List Troop -> Float
sumTroopWages t =
    foldr (\x v -> toFloat x.amount * troopWage x.troopType + v) 0 t


sumLordTroops : Lord -> List Troop
sumLordTroops lord =
    lord.entity.army ++ foldr (\x y -> x.entity.army ++ y) [] lord.land



-- refactor it


flattenTroops : List Troop -> List TroopType -> List Troop
flattenTroops troops types =
    case types of
        [] ->
            []

        y :: ys ->
            { amount = List.foldr (\t v -> t.amount + v) 0 (List.filter (\x -> x.troopType == y) troops), troopType = y } :: flattenTroops troops ys


lordToMapIcon : Lord -> String
lordToMapIcon l =
    let
        fileName =
            case l.entity.faction of
                Faction.Faction1 ->
                    "mini1.png"

                Faction.Faction2 ->
                    "mini2.png"

                Faction.Faction3 ->
                    "mini3.png"

                Faction.Faction4 ->
                    "mini4.png"
    in
    "profiles/" ++ fileName


factionToImage : Faction -> String
factionToImage fac =
    case fac of
        Faction.Faction1 ->
            "faction1.png"

        Faction.Faction2 ->
            "faction2.png"

        Faction.Faction3 ->
            "faction3.png"

        Faction.Faction4 ->
            "faction4.png"
