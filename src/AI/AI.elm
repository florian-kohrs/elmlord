module AI exposing (..)

import AI.AIActionDistanceHandler
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


showAiRoundAction : AiRoundActions -> String
showAiRoundAction aiRoundActions =
    case aiRoundActions of
        EndRound ->
            "End Round"

        GoSomeWhere p ->
            "Go to " ++ Vector.showPoint p

        DoSomething basicAction ->
            showBasicAction basicAction


showBasicAction : BasicAction -> String
showBasicAction basicAction =
    case basicAction of
        AttackLord _ ->
            "Attack Lord"

        HireTroops intTroopTypeTroopsDictDict settlementModelEntities ->
            "Hire Troops"

        SwapTroops intTroopTypeTroopsDictDict settlementModelEntities ->
            "Swap Troops"

        SiegeSettlement settlementModelEntities ->
            "Siege Settlement: " ++ settlementModelEntities.entity.name

        ImproveBuilding settlementModelEntities buildingBuilding ->
            "Improve Building"


getAiActionMultiplier : Float -> Float
getAiActionMultiplier f =
    1 + sin (2 * pi * f) / 3


setLord : AI -> Entities.Model.Lord -> AI
setLord ai l =
    { ai | lord = l }


updateAi : AI -> AiRoundActions -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> AI
updateAi ai action moveTowards =
    case action of
        EndRound ->
            ai

        GoSomeWhere p ->
            { ai | lord = moveTowards ai.lord p }

        DoSomething basicAction ->
            executeBasicAiAction ai basicAction moveTowards


executeBasicAiAction : AI -> BasicAction -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> AI
executeBasicAiAction ai action moveTowards =
    case action of
        SiegeSettlement s ->
            let
                movedAI =
                    { ai | lord = moveTowards ai.lord s.entity.position }
            in
            if movedAI.lord.entity.position == s.entity.position then
                movedAI
                --implement / search ui acion form main

            else
                movedAI

        _ ->
            ai


getAiAction : AI -> (Entities.Model.Lord -> Vector.Point -> Int) -> (Entities.Model.Lord -> Vector.Point -> Bool) -> List Entities.Model.Lord -> AiRoundActions
getAiAction ai distanceTo canMoveInTurn enemies =
    case
        List.head <|
            List.sortBy (\action -> -action.actionValue) <|
                List.map
                    (AI.AIActionDistanceHandler.applyActionDistancePenalty (canMoveInTurn ai.lord))
                    (AiRoundActionPreference EndRound 0.0 :: getAiActions ai enemies)
    of
        Nothing ->
            EndRound

        Just action ->
            case AI.AIActionDistanceHandler.getAiRoundActionDestination action.action of
                Nothing ->
                    EndRound

                Just p ->
                    if canMoveInTurn ai.lord p then
                        action.action

                    else
                        EndRound


getAiActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getAiActions ai enemies =
    let
        ownSettlementDefenseActions =
            getSettlementDefenseActions ai enemies

        enemySettlementStates =
            getSettlementAttackActions ai enemies
    in
    if PathAgent.remainingMovement ai.lord.agent == 0 then
        []

    else
        enemySettlementStates


getSettlementDefenseActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementDefenseActions ai enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> evaluateSettlementDefense ai s :: r)
            []
            (settlementDefenseArmyRating ai.lord)


getSettlementAttackActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementAttackActions ai enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> evaluateSettlementSiegeAction ai s enemies :: r)
            []
            (List.concat <| List.map (\l -> l.land) enemies)


evaluateSettlementSiegeAction : AI -> Entities.Model.Settlement -> List Entities.Model.Lord -> Maybe AiRoundActionPreference
evaluateSettlementSiegeAction ai s ls =
    let
        siegeStrengthDiff =
            toFloat (Troops.sumTroopStats ai.lord.entity.army)
                / (toFloat (settlementDefenseStrength ai s ls)
                    * MaybeExt.foldMaybe
                        (\l ->
                            1 + Balancing.settlementDefenseBoni s l
                        )
                        1
                        (Entities.landlordOnSettlement s ls)
                  )
    in
    if
        siegeStrengthDiff
            >= 1
    then
        Just
            (AiRoundActionPreference
                (DoSomething (SiegeSettlement s))
                (siegeStrengthDiff
                    * ai.strategy.siegeMultiplier
                )
            )

    else
        Nothing


estimatedNormalPlayerTroopStrength : Entities.Model.Lord -> Float
estimatedNormalPlayerTroopStrength l =
    let
        x =
            Entities.lordSettlementCount l
    in
    toFloat <| 300 + 50 * x


lordArmyComparison : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordArmyComparison l1 l2 =
    let
        diff =
            AI.AISettlementHandling.entityStrength l1.entity / AI.AISettlementHandling.entityStrength l2.entity
    in
    1 + Balancing.lordStrengthDiffFactor * diff


lordStrengthDiff : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordStrengthDiff attacker defender =
    AI.AISettlementHandling.entityStrength attacker.entity / AI.AISettlementHandling.entityStrength defender.entity
