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


distanceFromSiegeActionPenalty : Int -> Float
distanceFromSiegeActionPenalty turns =
    toFloat turns * 0.08


distanceSwapTroopsActionPenalty : Int -> Float
distanceSwapTroopsActionPenalty turns =
    toFloat turns * 0.06


distanceHireTroopsActionPenalty : Int -> Float
distanceHireTroopsActionPenalty turns =
    toFloat turns * 0.04


distanceImproveBuildingActionPenalty : Int -> Float
distanceImproveBuildingActionPenalty turns =
    toFloat turns * 0.09


distanceFromDefensiveActionPenalty : Int -> Float
distanceFromDefensiveActionPenalty turns =
    toFloat turns * 0.05


distanceFromMoveToPenalty : Int -> Float
distanceFromMoveToPenalty turns =
    toFloat turns * 0.075



{-
   For now ai lords are heavily against attacking a lord they cant reach in
   this turn
-}


distanceFromAttackLordPenalty : Int -> Float
distanceFromAttackLordPenalty turns =
    toFloat turns


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
            distanceFromAttackLordPenalty i

        HireTroops _ _ ->
            distanceHireTroopsActionPenalty i

        SwapTroops _ _ ->
            distanceSwapTroopsActionPenalty i

        SiegeSettlement _ ->
            distanceFromSiegeActionPenalty i

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
