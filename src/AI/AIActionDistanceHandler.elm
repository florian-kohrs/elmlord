module AI.AIActionDistanceHandler exposing (..)

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


distanceFromCapitalSiegeActionPenalty : Int -> Float
distanceFromCapitalSiegeActionPenalty turns =
    toFloat (turns - 3) * 0.15


distanceFromVillageSiegeActionPenalty : Int -> Float
distanceFromVillageSiegeActionPenalty turns =
    toFloat turns * 0.1


distanceSwapTroopsActionPenalty : Int -> Float
distanceSwapTroopsActionPenalty turns =
    toFloat turns * 0.05


distanceHireTroopsActionPenalty : Int -> Float
distanceHireTroopsActionPenalty turns =
    toFloat turns * 0.03


distanceImproveBuildingActionPenalty : Int -> Float
distanceImproveBuildingActionPenalty turns =
    toFloat turns * 0.09


distanceFromMoveToPenalty : Int -> Float
distanceFromMoveToPenalty turns =
    toFloat turns * 0.075



{-
   For now ai lords are heavily against attacking a lord they cant reach in
   this turn
-}


distanceFromAttackLordPenalty : Int -> Float
distanceFromAttackLordPenalty turns =
    toFloat turns * 1.5


applyActionDistancePenalty : (Vector.Point -> Int) -> AiRoundActionPreference -> AiRoundActionPreference
applyActionDistancePenalty turnsToPoint action =
    let
        destination =
            getAiRoundActionDestination action.action

        turnsToAction =
            MaybeExt.foldMaybe (\p -> turnsToPoint p) 0 destination
    in
    { action
        | actionValue =
            action.actionValue
                - getActionDistancePenalty action.action turnsToAction
    }


getActionDistancePenalty : AiRoundActions -> Int -> Float
getActionDistancePenalty a turnsToPoint =
    case a of
        EndRound ->
            0

        GoSomeWhere _ ->
            distanceFromMoveToPenalty turnsToPoint

        DoSomething baseAction ->
            getBaseActionDistancePenalty baseAction turnsToPoint


getBaseActionDistancePenalty : BasicAction -> Int -> Float
getBaseActionDistancePenalty basicAction i =
    case basicAction of
        AttackLord l ->
            max 0 <| distanceFromAttackLordPenalty i

        HireTroops _ _ ->
            if i < 0 then
                -2

            else
                distanceHireTroopsActionPenalty i

        SwapTroops _ _ ->
            distanceSwapTroopsActionPenalty i

        SiegeSettlement s ->
            if s.settlementType == Entities.Model.Castle then
                distanceFromCapitalSiegeActionPenalty i

            else
                distanceFromVillageSiegeActionPenalty i

        ImproveBuilding _ _ ->
            distanceImproveBuildingActionPenalty i


getAiRoundActionDestination : AiRoundActions -> Maybe Vector.Point
getAiRoundActionDestination a =
    case a of
        EndRound ->
            Nothing

        GoSomeWhere p ->
            Just p

        DoSomething basicAction ->
            Just <| getBasicActionDestination basicAction


getBasicActionDestination : BasicAction -> Vector.Point
getBasicActionDestination basicAction =
    case basicAction of
        AttackLord l ->
            l.entity.position

        HireTroops _ s ->
            s.entity.position

        SwapTroops _ s ->
            s.entity.position

        SiegeSettlement s ->
            s.entity.position

        ImproveBuilding s _ ->
            s.entity.position
