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


takeTroopsToLeaveArmyAtStrength : Int -> Troops.Army -> Troops.Army
takeTroopsToLeaveArmyAtStrength strength =
    Tuple.second <|
        Dict.foldl
            (\k v ( currentStrength, dict ) ->
                let
                    troopStats =
                        Troops.troopStrengthDeffSum <| Troops.intToTroopType k

                    notAvailableAmount =
                        clamp 0 v (round (strength - currentStrength)) / troopStats
                in
                ( currentStrength + troopStats * notAvailableAmount, Dict.insert k (v - notAvailableAmount) )
            )
            ( i, Dict.empty )


estimatedTroopStrengthForGold : Int -> Int
estimatedTroopStrengthForGold i =
    Troops.averageTroopStrengthCostRatio int


troopAmountWithStrength : Int -> Int -> Int -> Int
troopAmountWithStrength strength neededStrength =
    max 0 <| round (neededStrength / strength)


tryBuyTroopsWithTotalStrenghtFrom : AI -> Int -> Settlement -> Dict.Dict Troops.TroopType Int
tryBuyTroopsWithTotalStrenghtFrom aI targetStrength settlement =
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
                                (targetStrength - Troops.averageTroopStrengthCostRatio)
                            )
                        )
            in
            ( gold - amount * troopCost, Dict.insert k amount dict )
        )
        ( aI.lord.gold, Dict.empty )
        settlement.recruitLimits
