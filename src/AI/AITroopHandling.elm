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


estimatedNormalVillageTroopStrength : Entities.Model.Lord -> Float
estimatedNormalVillageTroopStrength l =
    toFloat 250


estimatedNormalPlayerTroopStrength : Entities.Model.Lord -> Float
estimatedNormalPlayerTroopStrength l =
    let
        x =
            Entities.lordSettlementCount l
    in
    toFloat <| 300 + 50 * x


troopStrengthToBotherAddingToSettlement : Int
troopStrengthToBotherAddingToSettlement =
    100


transformSwapTroopsToBuyTroops : AI -> Int -> Entities.Model.Settlement -> AiRoundActions
transformSwapTroopsToBuyTroops ai neededStrength s =
  if hasTroopsToSatisfySettlementDefense ai then
    DoSomething (SwapTroops (takeTroopsWithStrength neededStrength ai.lord.entity.army)
  else
    DoSomething (HireTroops )

hasTroopsToSatisfySettlementDefense : AI -> Bool
hasTroopsToSatisfySettlementDefense ai =
    (Troops.sumTroopStats <|
        AI.takeTroopsToLeaveArmyAtStrength
            (round <| estimatedNormalPlayerTroopStrength ai.lord)
            ai.lord.entity.army
    )
        >= troopStrengthToBotherAddingToSettlement



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



estimatedSettlementDefenseStrength : Entities.Model.Lord -> Entities.Model.SettlementType -> Float
estimatedSettlementDefenseStrength l t =
    case t of
        Entities.Model.Village ->
            estimatedNormalVillageTroopStrength l

        Entities.Model.Castle ->
            estimatedNormalCastleTroopStrength l
