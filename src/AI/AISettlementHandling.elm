module AI.AISettlementHandling exposing (..)

import AI.AIGoldManager exposing (..)
import AI.AITroopHandling exposing (..)
import AI.Model exposing (..)
import Balancing
import Building
import Dict
import Entities
import Entities.Model
import ListExt
import MaybeExt
import PathAgent
import Pathfinder
import Troops
import Vector


troopStrengthToBotherAddingToSettlement : Int
troopStrengthToBotherAddingToSettlement =
    100


hasTroopsToSatisfySettlementDefense : AI -> Entities.Model.Settlement -> Bool
hasTroopsToSatisfySettlementDefense ai s =
    (Troops.sumTroopStats <|
        AI.AITroopHandling.takeTroopsToLeaveArmyAtStrength
            (round <| estimatedSettlementDefenseStrength ai.lord s.settlementType)
            s.entity.army
    )
        >= troopStrengthToBotherAddingToSettlement


checkSettlementsForTroops : Int -> AI -> List AiRoundActionPreference
checkSettlementsForTroops targetStrength ai =
    List.foldl
        (\s r ->
            let
                recruitableTroopsDict =
                    AI.AITroopHandling.tryBuyTroopsWithTotalStrenghtFrom ai targetStrength s

                troopStrength =
                    Troops.sumTroopStats recruitableTroopsDict
            in
            AiRoundActionPreference
                (DoSomething (HireTroops recruitableTroopsDict s))
                (min 1 (toFloat troopStrength / toFloat targetStrength))
                :: r
        )
        []
        ai.lord.land


settlementDefenseArmyRating : Entities.Model.Lord -> List SettlmentDefenseRating
settlementDefenseArmyRating l =
    List.foldl
        (\s r ->
            SettlmentDefenseRating s
                (rateSettlementDefense l (Troops.sumTroopStats s.entity.army) s.settlementType)
                :: r
        )
        []
        l.land


rateSettlementDefense : Entities.Model.Lord -> Int -> Entities.Model.SettlementType -> Float
rateSettlementDefense lord strength entityType =
    toFloat strength / estimatedSettlementDefenseStrength lord entityType


estimatedSettlementDefenseStrength : Entities.Model.Lord -> Entities.Model.SettlementType -> Float
estimatedSettlementDefenseStrength l t =
    case t of
        Entities.Model.Village ->
            estimatedNormalVillageTroopStrength l

        Entities.Model.Castle ->
            estimatedNormalCastleTroopStrength l



--evaluates if any settlements controlled by the current ai have insufficent defense


evaluateSettlementDefense : AI -> SettlmentDefenseRating -> Maybe AiRoundActionPreference
evaluateSettlementDefense ai settlementDefenseRating =
    if
        settlementDefenseRating.armyStrengthVariance
            + Balancing.acceptedSettlementLackOfDefense
            / ai.strategy.defendMultiplier
            >= 1
    then
        Nothing

    else
        Just
            (AiRoundActionPreference
                (DoSomething (SwapTroops Dict.empty settlementDefenseRating.settlement))
                ((settlementDefenseRating.armyStrengthVariance ^ -1) * ai.strategy.defendMultiplier)
            )



{-
   Sums the troop strength of the settlement troops and adds the landlords troops if
   he is on the same position
-}


settlementDefenseStrength :
    AI
    -> Entities.Model.Settlement
    -> List Entities.Model.Lord
    -> Int
settlementDefenseStrength ai s enemies =
    let
        settlementDefense =
            round <| entityStrength s.entity
    in
    case Entities.landlordOnSettlement s enemies of
        Nothing ->
            settlementDefense

        Just l ->
            round (entityStrength l.entity) + settlementDefense


entityStrength : Entities.Model.WorldEntity -> Float
entityStrength e =
    toFloat (Troops.sumTroopStats e.army)


estimatedNormalVillageTroopStrength : Entities.Model.Lord -> Float
estimatedNormalVillageTroopStrength l =
    toFloat 250


estimatedNormalCastleTroopStrength : Entities.Model.Lord -> Float
estimatedNormalCastleTroopStrength l =
    let
        x =
            toFloat <| Entities.lordSettlementCount l
    in
    400 * ((1 / x) + ((1 - (1 / x)) / (x * x * 0.01 + 1)))
