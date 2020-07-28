module Entities exposing (..)

import Building
import Dict
import Entities.Model exposing (..)
import Faction
import List
import OperatorExt
import Troops
import Vector


setLordEntity : Lord -> WorldEntity -> Lord
setLordEntity l e =
    { l | entity = e }



-- all the different types that are used for the model
----------------------------------------------------------


lordSettlementCount : Lord -> Int
lordSettlementCount l =
    List.foldl
        (always ((+) 1))
        0
        l.land



-- functions for general handling or update of entities
----------------------------------------------------------


updateEntityFaction : Faction.Faction -> WorldEntity -> WorldEntity
updateEntityFaction fa we =
    { we | faction = fa, army = Troops.emptyTroops }


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
    { l
        | gold =
            l.gold
                - (((100.0 - Building.resolveBonusFromBuildings s.buildings Building.Fortress) / 100)
                    * Troops.troopCost t
                    * (toFloat amount / 5.0)
                  )
        , entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t amount) l.entity
        , land = updateSettlementRecruits s t -amount l.land
    }


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


disbandTroops : Lord -> Troops.TroopType -> Lord
disbandTroops l t =
    let
        amount =
            getPossibleTroopAmount l.entity.army t
    in
    { l | entity = updateEntitiesArmy (Troops.updateTroops l.entity.army t (-1 * amount)) l.entity }


upgradeBuilding : Lord -> Building.Building -> Settlement -> Lord
upgradeBuilding l b s =
    { l | gold = l.gold - upgradeBuildingCost b, land = updateSettlementBuildings l.land s.entity.name b.buildingType }


upgradeBuildingCost : Building.Building -> Float
upgradeBuildingCost b =
    Building.upgradeCostBase b.buildingType * Basics.toFloat (b.level + 1)


updateEntitiesArmy : Troops.Army -> WorldEntity -> WorldEntity
updateEntitiesArmy army e =
    { e | army = army }


getPossibleTroopAmount : Troops.Army -> Troops.TroopType -> Int
getPossibleTroopAmount army t =
    case Dict.get (Troops.troopTypeToInt t) army of
        Nothing ->
            0

        Just amount ->
            min 5 amount



-- is needed for the direct update of the lord troops inside the battle stats


updatePlayerArmy : Lord -> Troops.Army -> Lord
updatePlayerArmy l t =
    { l | entity = updateEntitiesArmy t l.entity }


sumLordTroops : Lord -> Troops.Army
sumLordTroops lord =
    Troops.mergeTroops
        lord.entity.army
        (sumLordSettlementTroops lord)


sumLordSettlementTroops : Lord -> Troops.Army
sumLordSettlementTroops lord =
    List.foldl (\s dict -> Troops.mergeTroops dict s.entity.army) Dict.empty lord.land



-- functions for the handling of settlements
----------------------------------------------------------


updateSettlementTroops : List Settlement -> String -> Troops.TroopType -> Int -> List Settlement
updateSettlementTroops l s t a =
    List.map
        (\x ->
            { x
                | entity =
                    OperatorExt.ternary
                        (x.entity.name == s)
                        (updateEntitiesArmy (Troops.updateTroops x.entity.army t a) x.entity)
                        x.entity
            }
        )
        l


updateSettlementRecruits : Settlement -> Troops.TroopType -> Int -> List Settlement -> List Settlement
updateSettlementRecruits s t add =
    List.map
        (\x ->
            { x
                | recruitLimits =
                    if x.entity.name == s.entity.name then
                        Dict.map
                            (\k v ->
                                if k == Troops.troopTypeToInt t then
                                    v + add

                                else
                                    v
                            )
                            x.recruitLimits

                    else
                        x.recruitLimits
            }
        )


updateSettlementBuildings : List Settlement -> String -> Building.BuildingType -> List Settlement
updateSettlementBuildings l s b =
    List.map
        (\x ->
            { x
                | buildings =
                    OperatorExt.ternary
                        (x.entity.name == s)
                        (Building.upgradeBuildingType x.buildings b)
                        x.buildings
            }
        )
        l


getSettlementByName : List Settlement -> String -> Maybe Settlement
getSettlementByName l s =
    case List.filter (\x -> x.entity.name == s) l of
        [] ->
            Nothing

        x :: _ ->
            Just x


applySettlementNewRecruits : List Settlement -> List Settlement
applySettlementNewRecruits =
    List.map (\s -> { s | recruitLimits = Dict.map (\t amount -> amount + Basics.round (5.0 + Building.resolveBonusFromBuildings s.buildings Building.Barracks)) s.recruitLimits })


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
    { entity = { army = Dict.empty, faction = e.faction, position = e.position, name = name }, settlementType = Castle, recruitLimits = Troops.emptyTroops, income = 5.0, isSieged = False, buildings = Building.startBuildings }


editSettlmentInfoPosition : Vector.Point -> SettlementInfo -> SettlementInfo
editSettlmentInfoPosition p i =
    { i | position = p }


getSettlementFor : SettlementInfo -> Settlement
getSettlementFor info =
    { entity = { army = Troops.startTroops, faction = info.faction, position = info.position, name = info.name }, settlementType = info.sType, recruitLimits = Troops.emptyTroops, income = 1.5, isSieged = False, buildings = Building.startBuildings }



-- functions for the handling of lords and their data
----------------------------------------------------------


isLordInOwnSettlement : Lord -> Bool
isLordInOwnSettlement lord =
    List.any ((==) lord.entity.position) (List.map (\s -> s.entity.position) lord.land)


isLordOnSettlement : Lord -> Settlement -> Bool
isLordOnSettlement lord s =
    lord.entity.position == s.entity.position && lord.entity.faction == s.entity.faction


landlordOnSettlement : Settlement -> List Lord -> Maybe Lord
landlordOnSettlement s =
    List.foldl
        (\l r ->
            if isLandlord s l && isLordOnSettlement l s then
                Just l

            else
                r
        )
        Nothing


isLandlord : Settlement -> Lord -> Bool
isLandlord s l =
    List.foldl (\s2 b -> b || s2.entity.name == s.entity.name) False l.land


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


factionToLord : Faction.Faction -> List Lord -> Maybe Lord
factionToLord f l =
    List.head (List.filter (\x -> x.entity.faction == f) l)



-- functions for income calculation of the lord
----------------------------------------------------------


applyLordGoldIncome : Lord -> Lord
applyLordGoldIncome lord =
    { lord | gold = lord.gold + calculateRoundIncome lord }


calculateRoundIncome : Lord -> Float
calculateRoundIncome lord =
    sumSettlementsIncome lord.land - sumTroopWages (sumLordTroops lord)


sumSettlementsIncome : List Settlement -> Float
sumSettlementsIncome s =
    List.foldr (\x v -> x.income + Building.resolveBonusFromBuildings x.buildings Building.Marketplace + v) 0 s


sumTroopWages : Troops.Army -> Float
sumTroopWages =
    Dict.foldl (\k v wages -> toFloat v * Troops.troopWage (Troops.intToTroopType k) + wages) 0



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


validatePlayerName : String -> Bool
validatePlayerName v =
    List.any (\x -> x == v) Entities.Model.aiNames


changeLordName : String -> Lord -> Lord
changeLordName name lord =
    { lord | entity = changeEntitiyName name lord.entity }


changeEntitiyName : String -> Entities.Model.WorldEntity -> Entities.Model.WorldEntity
changeEntitiyName name we =
    { we | name = name }
