module AI exposing (..)

import Balancing
import Building
import Dict
import Entities
import ListExt
import MaybeExt
import PathAgent
import PathAgentExt
import Pathfinder
import Vector


type alias AI =
    { lord : Entities.Lord, strategy : ActionMultipliers }


type alias ActionMultipliers =
    { siegeMultiplier : Float
    , defendMultiplier : Float
    , battleMultiplier : Float
    , growArmyMultiplier : Float
    }


type alias AiRoundActionPreference =
    { action : AiRoundActions, actionValue : Float }


type AiRoundActions
    = EndRound
    | GoSomeWhere Vector.Point
    | DoSomething BasicAction


type BasicAction
    = AttackLord Entities.Lord
    | HireTroops (Dict.Dict Troops.TroopType Int) Entities.Settlement
    | SwapTroops (Dict.Dict Troops.TroopType Int) Entities.Settlement
    | SiegeSettlement Entities.Settlement
    | ImproveBuilding Entities.Settlement Building.Building


type alias EnemyStatus =
    { strengthDiff : Float, turnsTillReached : Float, settlement : Entities.Settlement }


type alias SettlmentDefenseRating =
    { settlement : Entities.Settlement, armyStrengthVariance : Float }


updateAi : AI -> AiRoundActions -> (Entities.Lord -> Vector.Point -> Entities.Lord) -> AI
updateAi ai action moveTowards =
    case action of
        EndRound ->
            ai

        GoSomeWhere p ->
            { ai | lord = moveTowards ai.lord p }

        DoSomething basicAction ->
            ai


getAiAction : AI -> (Entities.Lord -> Vector.Point -> Int) -> List Entities.Lord -> AiRoundActions
getAiAction ai distanceTo enemies =
    case
        List.head
            (List.sortBy (\action -> action.actionValue) (getAiActions ai distanceTo enemies))
    of
        Nothing ->
            EndRound

        Just action ->
            action.action


getAiActions : AI -> (Entities.Lord -> Vector.Point -> Int) -> List Entities.Lord -> List AiRoundActionPreference
getAiActions ai getTurnsToPoint enemies =
    let
        ownSettlementDefenseRatings =
            settlementArmiesStrength ai.lord

        enemySettlementStates =
            List.foldl (\l r -> settlementArmiesStrength l ++ r) [] enemies

        roundActions =
            ListExt.justList (List.foldl (\s r -> evaluateSettlementDefense ai s :: r) [] ownSettlementDefenseRatings)
    in
    roundActions



--evaluate if any settlements controlled by the current ai are too weak


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
        AiRoundActionPreference
            (DoSomething (SwapTroops Dict.empty settlementDefenseRating.settlement))
            (settlementDefenseRating.armyStrengthVariance / ai.strategy.defendMultiplier)


estimatedNormalPlayerTroopStrength : Entities.Lord -> Float
estimatedNormalPlayerTroopStrength l =
    let
        x =
            Entities.lordSettlementCount l
    in
    toFloat <| 300 + 50 * x


estimatedNormalVillageTroopStrength : Entities.Lord -> Float
estimatedNormalVillageTroopStrength l =
    toFloat 250


estimatedNormalCastleTroopStrength : Entities.Lord -> Float
estimatedNormalCastleTroopStrength l =
    let
        x =
            toFloat <| Entities.lordSettlementCount l
    in
    400 * ((1 / x) + ((1 - (1 / x)) / (x * x * 0.01 + 1)))


rateSettlementDefense : Entities.Lord -> Int -> Entities.SettlementType -> Float
rateSettlementDefense lord strength entityType =
    case entityType of
        Entities.Village ->
            toFloat strength / estimatedNormalVillageTroopStrength lord

        Entities.Castle ->
            toFloat strength / estimatedNormalCastleTroopStrength lord


settlementArmiesStrength : Entities.Lord -> List SettlmentDefenseRating
settlementArmiesStrength l =
    List.foldl
        (\s r ->
            SettlmentDefenseRating s
                (rateSettlementDefense l (Troops.sumTroopStats s.entity.army) s.settlementType)
                :: r
        )
        []
        l.land


lordArmyComparison : Entities.Lord -> Entities.Lord -> Float
lordArmyComparison l1 l2 =
    let
        diff =
            lordStrength l1 / lordStrength l2
    in
    1 + Balancing.lordStrengthDiffFactor * diff


lordStrength : Entities.Lord -> Float
lordStrength l =
    toFloat (Troops.sumTroopStats l.entity.army)
        / estimatedNormalPlayerTroopStrength l


lordStrengthDiff : Entities.Lord -> Entities.Lord -> Float
lordStrengthDiff attacker defender =
    Troops.sumTroopStats attacker.entity.army / Troops.sumTroopStats defender.entity.army
