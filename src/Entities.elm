module Entities exposing (..)

import Faction
import List
import OperatorExt
import PathAgent
import Troops
import Vector


type alias Gold =
    Float



-- all the different types that are used for the model
----------------------------------------------------------


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , land : List Settlement
    , agent : PathAgent.Agent
    }


type alias BattleStats =
    { attacker : Lord
    , defender : Lord
    , round : Int
    , attackerCasualties : List Troops.Troop
    , defenderCasualties : List Troops.Troop
    , settlement : Maybe Settlement
    , siege : Bool
    , finished : Bool
    }


type LordList
    = Cons Lord (List Lord)


type alias Settlement =
    { entity : WorldEntity
    , settlementType : SettlementType
    , recruitLimits : List Troops.Troop
    , income : Float
    , isSieged : Bool
    }


type alias WorldEntity =
    { army : List Troops.Troop
    , faction : Faction.Faction
    , position : Vector.Point
    , name : String
    }


type SettlementType
    = Village
    | Castle


type alias SettlementInfo =
    { sType : SettlementType
    , position : Vector.Point
    , name : String
    , faction : Faction.Faction
    }



-- functions for general handling or update of entities
----------------------------------------------------------


updateEntityFaction : Faction.Faction -> WorldEntity -> WorldEntity
updateEntityFaction fa we =
    { we | faction = fa }


setPosition : WorldEntity -> Vector.Point -> WorldEntity
setPosition entity pos =
    { entity | position = pos }



-- functions for the handling of armies / troops inside of entities
----------------------------------------------------------


buyTroops : Lord -> Troops.TroopType -> Settlement -> Lord
buyTroops l t s =
    let
        amount =
            getPossibleTroopAmount s.recruitLimits t
    in
    { l | gold = l.gold - (Troops.troopCost t * (toFloat amount / 5.0)) , entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t amount) l.entity, land = updateSettlementRecruits l.land s.entity.name t amount }


stationTroops : Lord -> Troops.TroopType -> Settlement -> Lord
stationTroops l t s =
    let
        amount =
            getPossibleTroopAmount l.entity.army t
    in
    { l | entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t (-1 * amount)) l.entity, land = updateSettlementTroops l.land s.entity.name t amount }


takeTroops : Lord -> Troops.TroopType -> Settlement -> Lord
takeTroops l t s =
    let
        amount =
            getPossibleTroopAmount s.entity.army t
    in
    { l | entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t amount) l.entity, land = updateSettlementTroops l.land s.entity.name t (-1 * amount) }


updateEntitiesArmy : List Troops.Troop -> WorldEntity -> WorldEntity
updateEntitiesArmy l e =
    { e | army = l }


getPossibleTroopAmount : List Troops.Troop -> Troops.TroopType -> Int
getPossibleTroopAmount l t =
    let
        currentAmount =
            (Maybe.withDefault { amount = 2, troopType = t } (List.head (List.filter (\x -> x.troopType == t) l))).amount
    in
    OperatorExt.ternary (currentAmount >= 5) 5 currentAmount



-- is needed for the direct update of the lord troops inside the battle stats


updatePlayerArmy : Lord -> List Troops.Troop -> Lord
updatePlayerArmy l t =
    { l | entity = updateEntitiesArmy t l.entity }


flattenTroops : List Troops.Troop -> List Troops.TroopType -> List Troops.Troop
flattenTroops troops types =
    case types of
        [] ->
            []

        y :: ys ->
            { amount = List.foldr (\t v -> t.amount + v) 0 (List.filter (\x -> x.troopType == y) troops), troopType = y } :: flattenTroops troops ys


sumLordTroops : Lord -> List Troops.Troop
sumLordTroops lord =
    lord.entity.army ++ List.foldr (\x y -> x.entity.army ++ y) [] lord.land



-- functions for the handling of settlements
----------------------------------------------------------


updateSettlementTroops : List Settlement -> String -> Troops.TroopType -> Int -> List Settlement
updateSettlementTroops l s t a =
    List.map (\x -> { x | entity = updateEntitiesArmy (Troops.updateTroops x.entity.army t a) x.entity }) (List.filter (\y -> y.entity.name == s) l)


updateSettlementRecruits : List Settlement -> String -> Troops.TroopType -> Int -> List Settlement
updateSettlementRecruits l s t v =
    List.map
        (\x -> { x | recruitLimits = List.map (\z -> { amount = OperatorExt.ternary (z.troopType == t) (z.amount - v) z.amount, troopType = z.troopType }) x.recruitLimits })
        (List.filter (\y -> y.entity.name == s) l)


getSettlementByName : List Settlement -> String -> Maybe Settlement
getSettlementByName l s =
    case List.filter (\x -> x.entity.name == s) l of
        [] ->
            Nothing

        x :: _ ->
            Just x


applySettlementNewRecruits : List Settlement -> List Settlement
applySettlementNewRecruits ls =
    List.map (\x -> { x | recruitLimits = List.map (\y -> { y | amount = y.amount + 5 }) x.recruitLimits }) ls


getSettlementBonus : Settlement -> List Settlement -> Float
getSettlementBonus s l =
    if s.settlementType == Village then
        1.1

    else
        List.foldr (\_ y -> 0.1 + y) 1 l


combineSettlementName : Settlement -> String
combineSettlementName settlement =
    getSettlementNameByType settlement.settlementType ++ " - " ++ settlement.entity.name


getLordCapital : List Settlement -> Maybe Settlement
getLordCapital l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if x.settlementType == Castle then
                Just x

            else
                getLordCapital xs


createCapitalFor : WorldEntity -> String -> Settlement
createCapitalFor e name =
    { entity = { army = [], faction = e.faction, position = e.position, name = name }, settlementType = Castle, recruitLimits = Troops.emptyTroops, income = 5.0, isSieged = False }


editSettlmentInfoPosition : Vector.Point -> SettlementInfo -> SettlementInfo
editSettlmentInfoPosition p i =
    { i | position = p }


getSettlementFor : SettlementInfo -> Settlement
getSettlementFor info =
    { entity = { army = Troops.startTroops, faction = info.faction, position = info.position, name = info.name }, settlementType = info.sType, recruitLimits = Troops.emptyTroops, income = 1.5, isSieged = False }



-- functions for the handling of lords and their data
----------------------------------------------------------


npcs : LordList -> List Lord
npcs (Cons _ ls) =
    ls


getPlayer : LordList -> Lord
getPlayer (Cons p _) =
    p


isLordInOwnSettlement : Lord -> Bool
isLordInOwnSettlement lord =
    List.any ((==) lord.entity.position) (List.map (\s -> s.entity.position) lord.land)


isLordOnSettlement : Lord -> Settlement -> Bool
isLordOnSettlement lord s =
    lord.entity.position == s.entity.position && lord.entity.faction == s.entity.faction


resetUsedMovement : Lord -> Lord
resetUsedMovement lord =
    { lord | agent = PathAgent.resetUsedMovement lord.agent }


updatePlayer : LordList -> Lord -> LordList
updatePlayer (Cons _ ps) np =
    Cons np ps


tailLordList : LordList -> List Lord
tailLordList (Cons _ ps) =
    ps


flattenLordList : LordList -> List Lord
flattenLordList (Cons p ps) =
    p :: ps


findLordWithSettlement : Settlement -> List Lord -> Maybe Lord
findLordWithSettlement settlement =
    List.foldr
        (\l r ->
            if l.entity.faction == settlement.entity.faction then
                Just l

            else
                r
        )
        Nothing


applyLordNewRecruits : Lord -> Lord
applyLordNewRecruits lord =
    { lord | land = applySettlementNewRecruits lord.land }



-- functions for income calculation of the lord
----------------------------------------------------------


applyLordGoldIncome : Lord -> Lord
applyLordGoldIncome lord =
    { lord | gold = lord.gold + calculateRoundIncome lord }


calculateRoundIncome : Lord -> Float
calculateRoundIncome lord =
    sumSettlementsIncome lord.land - sumTroopWages (flattenTroops (sumLordTroops lord) Troops.troopTypeList)


sumSettlementsIncome : List Settlement -> Float
sumSettlementsIncome s =
    List.foldr (\x v -> x.income + v) 0 s


sumTroopWages : List Troops.Troop -> Float
sumTroopWages t =
    List.foldr (\x v -> toFloat x.amount * Troops.troopWage x.troopType + v) 0 t



-- Resolve image paths, names, types, etc. of the entities
----------------------------------------------------------


getSettlementNameByType : SettlementType -> String
getSettlementNameByType s =
    case s of
        Village ->
            "Village"

        Castle ->
            "Castle"


getSettlementImage : Settlement -> String
getSettlementImage s =
    "./assets/images/settlements/" ++ getSettlementNameByType s.settlementType ++ ".png"


getPlayerImage : Lord -> String
getPlayerImage l =
    "./assets/images/profiles/" ++ factionToImage l.entity.faction


lordToMapIcon : Lord -> String
lordToMapIcon l =
    "./assets/images/profiles/mini/" ++ factionToImage l.entity.faction


factionToImage : Faction.Faction -> String
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



-- around 15 names for castles and 30 for villages
-- https://www.fantasynamegenerators.com/town_names.php was used as a source


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
