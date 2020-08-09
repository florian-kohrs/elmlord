module AI exposing (..)

import AI.AIActionDistanceHandler
import AI.AISettlementHandling
import AI.AITroopHandling
import AI.Model exposing (..)
import Balancing
import Battle
import Building
import Dict
import Entities
import Entities.Lords
import Entities.Model
import ListExt
import Map.Model
import MapData
import MaybeExt
import PathAgent
import Pathfinder
import Troops
import Vector



--stores for each key (from to Vector) the needed steps to reach


type alias PathLookUp =
    Dict.Dict Int Int


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
            "Swap Troops "
                ++ Dict.foldr
                    (\k v s ->
                        s
                            ++ "TroopIndex: "
                            ++ String.fromInt k
                            ++ " Amount: "
                            ++ String.fromInt v
                    )
                    ""
                    intTroopTypeTroopsDictDict

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


updateAi : AI -> AiRoundActions -> (Vector.Point -> Map.Model.Terrain) -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> Entities.Lords.LordList -> Entities.Lords.LordList
updateAi ai action tileOnPos moveTowards lordList =
    case action of
        EndRound ->
            lordList

        GoSomeWhere p ->
            Entities.Lords.replaceAi lordList <| { ai | lord = moveTowards ai.lord p }

        DoSomething basicAction ->
            executeBasicAiAction ai basicAction tileOnPos moveTowards lordList


executeBasicAiAction : AI -> BasicAction -> (Vector.Point -> Map.Model.Terrain) -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> Entities.Lords.LordList -> Entities.Lords.LordList
executeBasicAiAction ai action tileOnPos moveTowards lordList =
    let
        destination =
            AI.AIActionDistanceHandler.getBasicActionDestination action

        movedAI =
            { ai | lord = moveTowards ai.lord destination }
    in
    if movedAI.lord.entity.position == destination then
        case action of
            SiegeSettlement s ->
                siegeSettlement ai
                    s
                    (tileOnPos destination)
                    (Entities.Lords.replaceAi lordList movedAI)

            SwapTroops dict s ->
                Entities.Lords.replaceAi lordList <| { ai | lord = Entities.swapLordTroopsWithSettlement ai.lord s dict }

            _ ->
                Entities.Lords.replaceAi lordList <| movedAI

    else
        Entities.Lords.replaceAi lordList <| movedAI


siegeSettlement : AI -> Entities.Model.Settlement -> Map.Model.Terrain -> Entities.Lords.LordList -> Entities.Lords.LordList
siegeSettlement ai s t ls =
    MaybeExt.foldMaybe (\b -> Battle.applyBattleAftermath ls <| Battle.skipBattle t b) ls (Battle.getBattleSiegeStats ai.lord ls s)


getAiAction : AI -> (Entities.Model.Lord -> Vector.Point -> Int) -> (Entities.Model.Lord -> Vector.Point -> Bool) -> List Entities.Model.Lord -> AiRoundActions
getAiAction ai distanceTo canMoveInTurn enemies =
    case
        --improvement: apply distance penalty (big overhead on pathfinder) to head from
        --action list and stop if it is still first after penalty
        List.head <|
            List.sortBy (\action -> -action.actionValue) <|
                List.map
                    (AI.AIActionDistanceHandler.applyActionDistancePenalty (distanceTo ai.lord))
                    (AiRoundActionPreference EndRound 0.0 :: getAiActions ai enemies)
        --maybe replace endround 0.0 with go to capital
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

        hireTroops =
            AI.AITroopHandling.hireTroopsIfNeeded ai

        attackOthers =
            getAttackLordsActions ai enemies
    in
    ownSettlementDefenseActions
        ++ enemySettlementStates
        ++ hireTroops
        ++ attackOthers


getSettlementDefenseActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementDefenseActions ai enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> AI.AISettlementHandling.evaluateSettlementDefense ai s :: r)
            []
            ai.lord.land


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


getAttackLordsActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getAttackLordsActions ai =
    List.foldl
        (\l actions ->
            let
                preference =
                    1 - lordStrengthDiff ai.lord l
            in
            if preference >= 0 then
                AiRoundActionPreference (DoSomething (AttackLord l)) (preference * ai.strategy.battleMultiplier) :: actions

            else
                actions
        )
        []


evaluateSettlementSiegeAction : AI -> Entities.Model.Settlement -> List Entities.Model.Lord -> Maybe AiRoundActionPreference
evaluateSettlementSiegeAction ai s ls =
    let
        siegeStrengthDiff =
            toFloat (Troops.sumTroopStats ai.lord.entity.army)
                / (toFloat (AI.AISettlementHandling.settlementDefenseStrength ai s ls)
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
