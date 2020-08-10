module AI.AITroopHandling exposing (..)

import AI.AIGoldManager exposing (..)
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


acceptedLackOfDefenseStrength : Int
acceptedLackOfDefenseStrength =
    100


troopStrengthToBotherAddingToSettlement : Int
troopStrengthToBotherAddingToSettlement =
    100


neededTroopsStrengthFactor : Int
neededTroopsStrengthFactor =
    1000


estimatedNormalVillageTroopStrength : AI -> Float
estimatedNormalVillageTroopStrength ai =
    toFloat 250 * ai.strategy.defendMultiplier


estimatedNormalCastleTroopStrength : AI -> Float
estimatedNormalCastleTroopStrength ai =
    let
        x =
            toFloat <| Entities.lordSettlementCount ai.lord
    in
    --(400 + (50 * x)) * ai.strategy.defendMultiplier
    1000 + (500 * ((1 / x) + ((1 - (1 / x)) / (x * x * 0.01 + 1)))) * ai.strategy.defendMultiplier


estimatedNormalPlayerTroopStrength : AI -> Float
estimatedNormalPlayerTroopStrength ai =
    let
        x =
            Entities.lordSettlementCount ai.lord
    in
    toFloat (300 + 50 * x) * max ai.strategy.battleMultiplier ai.strategy.siegeMultiplier


estimatedSettlementDefenseStrength : AI -> Entities.Model.SettlementType -> Float
estimatedSettlementDefenseStrength ai t =
    case t of
        Entities.Model.Village ->
            estimatedNormalVillageTroopStrength ai

        Entities.Model.Castle ->
            estimatedNormalCastleTroopStrength ai


hireTroopsIfNeeded : AI -> List AiRoundActionPreference
hireTroopsIfNeeded ai =
    let
        neededStrength =
            totalNeededTroopStrength ai
    in
    if neededStrength > 0 then
        checkSettlementsForTroops neededStrength ai

    else
        []


checkSettlementsForTroops : Int -> AI -> List AiRoundActionPreference
checkSettlementsForTroops targetStrength ai =
    List.foldl
        (\s r ->
            let
                recruitableTroopsDict =
                    tryBuyTroopsWithTotalStrenghtFrom ai targetStrength s

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


totalNeededTroopStrength : AI -> Int
totalNeededTroopStrength ai =
    max 0 <|
        List.foldl
            (\s neededStrength ->
                neededStrength
                    + --max 0
                      (round
                        (estimatedSettlementDefenseStrength
                            ai
                            s.settlementType
                        )
                        - Troops.sumTroopStats ai.lord.entity.army
                      )
            )
            (max
                0
                (round (estimatedNormalPlayerTroopStrength ai)
                    - Troops.sumTroopStats ai.lord.entity.army
                )
            )
            ai.lord.land


hasTroopsToSatisfySettlementDefense : AI -> Bool
hasTroopsToSatisfySettlementDefense ai =
    (Troops.sumTroopStats <|
        takeTroopsToLeaveArmyAtStrength
            (round <| estimatedNormalPlayerTroopStrength ai)
            ai.lord.entity.army
    )
        >= troopStrengthToBotherAddingToSettlement


takeDispensableTroopsWithMaxStrength : Troops.Army -> Int -> Int -> Troops.Army
takeDispensableTroopsWithMaxStrength sourceArmy sourceNeededStrength maxStrength =
    takeTroopsToLeaveArmyAtStrength
        (max
            sourceNeededStrength
            (Troops.sumTroopStats sourceArmy - maxStrength)
        )
        sourceArmy


takeTroopsToLeaveArmyAtStrength : Int -> Troops.Army -> Troops.Army
takeTroopsToLeaveArmyAtStrength strength army =
    Tuple.second <|
        Dict.foldl
            (\k v ( currentStrength, dict ) ->
                let
                    troopStats =
                        Troops.troopStrengthDeffSum <| Troops.intToTroopType k

                    notAvailableAmount =
                        clamp 0 v ((strength - currentStrength) // troopStats)
                in
                ( currentStrength + troopStats * notAvailableAmount, Dict.insert k (v - notAvailableAmount) dict )
            )
            ( 0, Dict.empty )
            army


takeTroopsWithStrength : Int -> Troops.Army -> Troops.Army
takeTroopsWithStrength neededStrength army =
    Tuple.second <|
        Dict.foldl
            (\k v ( currentStrength, dict ) ->
                let
                    troopStats =
                        Troops.troopStrengthDeffSum <| Troops.intToTroopType k

                    amount =
                        clamp 0 v ((neededStrength - currentStrength) // troopStats)
                in
                ( currentStrength + troopStats * amount, Dict.insert k amount dict )
            )
            ( 0, Dict.empty )
            army


estimatedTroopStrengthForGold : Int -> Int
estimatedTroopStrengthForGold gold =
    round Troops.averageTroopStrengthCostRatio * gold


troopAmountWithStrength : Int -> Int -> Int
troopAmountWithStrength strength neededStrength =
    max 0 <| round (toFloat neededStrength / toFloat strength)


tryBuyTroopsWithTotalStrenghtFrom : AI -> Int -> Entities.Model.Settlement -> Dict.Dict Int Int
tryBuyTroopsWithTotalStrenghtFrom aI targetStrength settlement =
    Tuple.second <|
        Dict.foldl
            (\k v ( gold, dict ) ->
                let
                    troopCost =
                        Troops.troopCost (Troops.intToTroopType k)

                    amount =
                        AI.AIGoldManager.takeAsMuchAsPossible troopCost
                            gold
                            (min v
                                (troopAmountWithStrength
                                    (Troops.troopStrengthDeffSum <| Troops.intToTroopType k)
                                    (targetStrength - round Troops.averageTroopStrengthCostRatio)
                                )
                            )
                in
                ( gold - toFloat (amount * troopCost), Dict.insert k amount dict )
            )
            ( aI.lord.gold, Dict.empty )
            settlement.recruitLimits


getSettlementRatingForTroopRecruition : AI -> Int -> List ( Entities.Model.Settlement, Float )
getSettlementRatingForTroopRecruition ai neededStrength =
    List.foldl
        (\s r ->
            let
                recruitArmy =
                    tryBuyTroopsWithTotalStrenghtFrom ai neededStrength s
            in
            ( s, max 1 <| toFloat (Troops.sumTroopStats recruitArmy) / toFloat neededStrength ) :: r
        )
        []
        ai.lord.land
