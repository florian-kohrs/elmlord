module AI.AIGoldManager exposing (..)

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


filterActionIfLordIsBroke : BasicAction -> AI -> Maybe BasicAction
filterActionIfLordIsBroke a ai =
    case a of
        HireTroops army s ->
            if ai.lord.gold > Entities.sumArmyBuyCost s army then
                Just <| HireTroops army s

            else
                Nothing

        ImproveBuilding s b ->
            if ai.lord.gold >= Building.upgradeBuildingCost b then
                Just <| ImproveBuilding s b

            else
                Nothing

        action ->
            Just action


takeAsMuchAsPossible : Int -> Float -> Int -> Int
takeAsMuchAsPossible cost gold maxValue =
    min maxValue (floor (gold / toFloat cost))


goldIncomePerRound : AI -> Float
goldIncomePerRound ai =
    Entities.calculateRoundIncome ai.lord
