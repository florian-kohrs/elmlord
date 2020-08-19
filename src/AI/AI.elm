module AI exposing (..)

import AI.AIActionDistanceHandler
import AI.AIGoldManager
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


showAiRoundActionPreference : AiRoundActionPreference -> String
showAiRoundActionPreference a =
    "Action: " ++ showAiRoundAction a.action ++ ", preference: " ++ String.fromFloat a.actionValue


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
        AttackLord l ->
            "Attack Lord " ++ l.entity.name

        HireTroops intTroopTypeTroopsDictDict settlementModelEntities ->
            "Hire Troops from "
                ++ settlementModelEntities.entity.name
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

        SwapTroops intTroopTypeTroopsDictDict settlementModelEntities ->
            "Swap Troops with "
                ++ settlementModelEntities.entity.name
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
            let
                destination =
                    AI.AIActionDistanceHandler.getBasicActionDestination basicAction

                movedAI =
                    { ai | lord = moveTowards ai.lord destination }
            in
            executeBasicAiAction movedAI destination basicAction tileOnPos moveTowards (Entities.Lords.replaceAi lordList <| movedAI)


executeBasicAiAction : AI -> Vector.Point -> BasicAction -> (Vector.Point -> Map.Model.Terrain) -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> Entities.Lords.LordList -> Entities.Lords.LordList
executeBasicAiAction ai destination action tileOnPos moveTowards lordList =
    if ai.lord.entity.position == destination then
        case action of
            SiegeSettlement s ->
                siegeSettlement
                    ai
                    s
                    (tileOnPos destination)
                    lordList

            AttackLord l ->
                attackLord
                    ai
                    l
                    (tileOnPos destination)
                    lordList

            SwapTroops dict s ->
                Entities.Lords.replaceAi lordList <| { ai | lord = Entities.swapLordTroopsWithSettlement ai.lord s dict }

            HireTroops dict s ->
                Entities.Lords.replaceAi lordList <| { ai | lord = Entities.recruitTroops dict ai.lord s }

            ImproveBuilding s b ->
                Entities.Lords.replaceAi lordList <| { ai | lord = Entities.upgradeBuilding ai.lord b s }

    else
        lordList


siegeSettlement : AI -> Entities.Model.Settlement -> Map.Model.Terrain -> Entities.Lords.LordList -> Entities.Lords.LordList
siegeSettlement ai s t ls =
    MaybeExt.foldMaybe
        (\b ->
            Battle.applyBattleAftermath ls <|
                Battle.skipBattle t b
        )
        ls
        (Battle.getBattleSiegeStats ai.lord ls s)


attackLord : AI -> Entities.Model.Lord -> Map.Model.Terrain -> Entities.Lords.LordList -> Entities.Lords.LordList
attackLord ai l t ls =
    Battle.applyBattleAftermath ls <|
        Battle.skipBattle t <|
            Battle.getLordBattleStats ai.lord l


getAiAction : AI -> (Entities.Model.Lord -> Vector.Point -> Int) -> (Entities.Model.Lord -> Vector.Point -> Bool) -> List Entities.Model.Lord -> AiRoundActions
getAiAction ai distanceTo canMoveInTurn enemies =
    case
        --improvement: apply distance penalty (big overhead on pathfinder) to head from
        --action list and stop if it is still first after penalty
        List.head <|
            List.sortBy (\action -> -action.actionValue) <|
                getAiActionsWithDistancePenalty ai distanceTo enemies
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


getAiActionsWithDistancePenalty : AI -> (Entities.Model.Lord -> Vector.Point -> Int) -> List Entities.Model.Lord -> List AiRoundActionPreference
getAiActionsWithDistancePenalty ai distanceTo enemies =
    List.map
        (AI.AIActionDistanceHandler.applyActionDistancePenalty (distanceTo ai.lord))
        (getAiActions ai enemies)


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

        takeTroops =
            AI.AITroopHandling.takeTroopsFromSettlements ai

        hireTroops =
            AI.AITroopHandling.hireTroopsIfNeeded ai

        improveBuildingFactor =
            getImproveBuildingActions ai

        attackOthers =
            getAttackLordsActions ai enemies
    in
    AiRoundActionPreference EndRound 0.0
        :: (ownSettlementDefenseActions
                ++ enemySettlementStates
                ++ hireTroops
                ++ takeTroops
                ++ attackOthers
                ++ improveBuildingFactor
           )


getSettlementDefenseActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementDefenseActions ai enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> AI.AITroopHandling.evaluateSettlementDefense ai s :: r)
            []
            ai.lord.land


getImproveBuildingActions : AI -> List AiRoundActionPreference
getImproveBuildingActions ai =
    case Entities.getLordCapital ai.lord.land of
        Nothing ->
            []

        Just capital ->
            ListExt.justList <| List.map (AI.AIGoldManager.getBuildingBuildFactors ai capital) capital.buildings


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
                strengthFactor =
                    lordStrengthDiff ai.lord l

                preference =
                    min (2 + ai.strategy.battleMultiplier) <| strengthFactor
            in
            if preference >= 0 && not (Entities.isLordInOwnSettlement l) then
                AiRoundActionPreference (DoSomething (AttackLord l)) preference :: actions

            else
                actions
        )
        []


evaluateSettlementSiegeAction : AI -> Entities.Model.Settlement -> List Entities.Model.Lord -> Maybe AiRoundActionPreference
evaluateSettlementSiegeAction ai s ls =
    let
        siegeStrengthDiff =
            toFloat (Troops.sumArmyStats ai.lord.entity.army)
                / max
                    1
                    (toFloat (AI.AISettlementHandling.settlementDefenseStrength s (Entities.landlordOnSettlement s ls))
                        * MaybeExt.foldMaybe
                            (\l ->
                                Entities.getSettlementBonus s ai.lord.land
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
                (min (2 + ai.strategy.siegeMultiplier)
                    (settlementSiegeBoni s
                        + logBase 10 (siegeStrengthDiff * siegeStrengthDiff)
                    )
                )
            )

    else
        Nothing


settlementSiegeBoni : Entities.Model.Settlement -> Float
settlementSiegeBoni s =
    case s.settlementType of
        Entities.Model.Castle ->
            0.5

        Entities.Model.Village ->
            0


lordStrengthDiff : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordStrengthDiff attacker defender =
    toFloat (Troops.sumArmyStats attacker.entity.army) / (max 1 <| toFloat <| Troops.sumArmyStats defender.entity.army)
