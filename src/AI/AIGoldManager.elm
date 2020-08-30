module AI.AIGoldManager exposing (..)

import AI.Model exposing (..)
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
            if ai.lord.gold >= Entities.sumArmyBuyCost army then
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


getBuildingBuildFactor : AI -> Building.BuildingType -> Float
getBuildingBuildFactor ai t =
    case t of
        Building.Quarters ->
            1 * ai.strategy.growArmyMultiplier

        Building.Barracks ->
            1 * max ai.strategy.battleMultiplier ai.strategy.siegeMultiplier

        Building.Fortress ->
            1 * ai.strategy.defendMultiplier


getBuildingBuildFactors : AI -> Entities.Model.Settlement -> Building.Building -> Maybe AiRoundActionPreference
getBuildingBuildFactors ai capital b =
    if b.level < 3 && ai.lord.gold > Building.upgradeBuildingCost b then
        Just <|
            AiRoundActionPreference
                (DoSomething <|
                    ImproveBuilding capital b
                )
                (min (2 + ai.strategy.improveSettlementsMultiplier) <|
                    (getBuildingBuildFactor ai b.buildingType
                        * (goldIncomePerRound
                            ai
                            * 1.75
                            / Building.upgradeBuildingCost b
                            + logBase 10
                                (ai.lord.gold
                                    / Building.upgradeBuildingCost b
                                )
                            / 1.5
                          )
                    )
                )

    else
        Nothing
