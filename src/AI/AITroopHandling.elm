module AI.AITroopHandling exposing (..)

import AI.AIGoldManager exposing (..)
import AI.AISettlementHandling
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


maximalAcceptedSettlementStrength : AI -> Entities.Model.Settlement -> Int
maximalAcceptedSettlementStrength ai s =
    round <| estimatedSettlementDefenseStrength ai s.settlementType * 1.5


maximalAcceptedPlayerStrength : AI -> Int
maximalAcceptedPlayerStrength ai =
    round <| toFloat (estimatedNormalPlayerTroopStrength ai) * 1.5


acceptedLackOfDefenseStrength : Int
acceptedLackOfDefenseStrength =
    300


troopStrengthToBotherAddingToSettlement : Int
troopStrengthToBotherAddingToSettlement =
    400


estimatedNormalVillageTroopStrength : AI -> Float
estimatedNormalVillageTroopStrength ai =
    toFloat 1500 * ai.strategy.defendMultiplier


estimatedNormalCastleTroopStrength : AI -> Float
estimatedNormalCastleTroopStrength ai =
    let
        x =
            toFloat <| Entities.lordSettlementCount ai.lord
    in
    --(400 + (50 * x)) * ai.strategy.defendMultiplier
    2000 + (600 * ((1 / x) + ((1 - (1 / x)) / (x * x * 0.01 + 1)))) * ai.strategy.defendMultiplier


estimatedNormalPlayerTroopStrength : AI -> Int
estimatedNormalPlayerTroopStrength ai =
    let
        x =
            Entities.lordSettlementCount ai.lord
    in
    (1500 + 400 * x) * round (max ai.strategy.battleMultiplier ai.strategy.siegeMultiplier)


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
            totalNeededTroopStrength ai identity
    in
    if neededStrength > 0 then
        checkSettlementsForRecruits neededStrength ai

    else
        []


settlementDisposableTroops : AI -> Entities.Model.Settlement -> Troops.Army
settlementDisposableTroops ai s =
    takeTroopsToLeaveArmyAtStrength (maximalAcceptedSettlementStrength ai s) s.entity.army


takeTroopsFromSettlements : AI -> List AiRoundActionPreference
takeTroopsFromSettlements ai =
    let
        neededStrength =
            totalNeededTroopStrength ai <| max 0
    in
    ListExt.justList <| List.foldl (\s actions -> checkSettlementForAvaiableTroops neededStrength ai s :: actions) [] ai.lord.land


checkSettlementForAvaiableTroops : Int -> AI -> Entities.Model.Settlement -> Maybe AiRoundActionPreference
checkSettlementForAvaiableTroops targetStrength ai s =
    let
        availableTroops =
            settlementDisposableTroops ai s

        troopStrength =
            Troops.sumTroopsStats availableTroops
    in
    if troopStrength > 0 then
        Just <|
            AiRoundActionPreference
                (DoSomething <| SwapTroops (Troops.invertArmy availableTroops) s)
                (min 2 (toFloat troopStrength / toFloat targetStrength))

    else
        Nothing


checkSettlementsForRecruits : Int -> AI -> List AiRoundActionPreference
checkSettlementsForRecruits targetStrength ai =
    List.foldl
        (\s r ->
            let
                recruitableTroopsDict =
                    tryBuyTroopsWithTotalStrenghtFrom ai targetStrength s

                troopStrength =
                    Troops.sumTroopsStats recruitableTroopsDict
            in
            AiRoundActionPreference
                (DoSomething (HireTroops recruitableTroopsDict s))
                (min 2.5
                    (max 0.5 (toFloat troopStrength / toFloat targetStrength)
                        * (AI.AISettlementHandling.settlementRecruitUsage ai.lord s * 2)
                    )
                )
                :: r
        )
        []
        ai.lord.land



--evaluates if any settlements controlled by the current ai have insufficent defense


evaluateSettlementDefense : AI -> Entities.Model.Settlement -> Maybe AiRoundActionPreference
evaluateSettlementDefense ai s =
    if
        settlementLackOfTroopStrength ai s
            - acceptedLackOfDefenseStrength
            < 0
    then
        Nothing

    else if hasTroopsToSatisfySettlementDefense ai then
        Just
            (AiRoundActionPreference
                (DoSomething
                    (SwapTroops
                        (takeDispensableTroopsWithMaxStrength
                            ai.lord.entity.army
                            (estimatedNormalPlayerTroopStrength ai)
                            (settlementLackOfTroopStrength ai s)
                        )
                        s
                    )
                )
                (clamp
                    0
                    2
                    (estimatedSettlementDefenseStrength ai s.settlementType
                        / max 1 (toFloat (Troops.sumTroopsStats s.entity.army))
                    )
                )
            )

    else
        Nothing


settlementLackOfTroopStrength : AI -> Entities.Model.Settlement -> Int
settlementLackOfTroopStrength ai s =
    round (estimatedSettlementDefenseStrength ai s.settlementType)
        - Troops.sumTroopsStats s.entity.army


totalNeededTroopStrength : AI -> (Int -> Int) -> Int
totalNeededTroopStrength ai op =
    max 0 <|
        List.foldl
            (\s neededStrength ->
                neededStrength
                    + op
                        (round
                            (estimatedSettlementDefenseStrength
                                ai
                                s.settlementType
                            )
                            - Troops.sumTroopsStats ai.lord.entity.army
                        )
            )
            (max
                0
                (estimatedNormalPlayerTroopStrength ai
                    - Troops.sumTroopsStats ai.lord.entity.army
                )
            )
            ai.lord.land


hasTroopsToSatisfySettlementDefense : AI -> Bool
hasTroopsToSatisfySettlementDefense ai =
    (Troops.sumTroopsStats <|
        takeTroopsToLeaveArmyAtStrength
            (estimatedNormalPlayerTroopStrength ai)
            ai.lord.entity.army
    )
        >= troopStrengthToBotherAddingToSettlement


takeDispensableTroopsWithMaxStrength : Troops.Army -> Int -> Int -> Troops.Army
takeDispensableTroopsWithMaxStrength sourceArmy sourceNeededStrength maxStrength =
    takeTroopsToLeaveArmyAtStrength
        (max
            sourceNeededStrength
            (Troops.sumTroopsStats sourceArmy - maxStrength)
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
            ( s, max 1 <| toFloat (Troops.sumTroopsStats recruitArmy) / toFloat neededStrength ) :: r
        )
        []
        ai.lord.land
