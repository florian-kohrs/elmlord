module AI exposing (..)

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


type alias AI =
    { lord : Entities.Model.Lord, strategy : ActionMultipliers }


type alias ActionMultipliers =
    { siegeMultiplier : Float
    , defendMultiplier : Float
    , battleMultiplier : Float
    , growArmyMultiplier : Float
    , improveSettlements : Float
    }


type alias AiRoundActionPreference =
    { action : AiRoundActions, actionValue : Float }


type AiRoundActions
    = EndRound
    | GoSomeWhere Vector.Point
    | DoSomething BasicAction


type BasicAction
    = AttackLord Entities.Model.Lord
    | HireTroops (Dict.Dict Troops.TroopType Int) Entities.Model.Settlement
    | SwapTroops (Dict.Dict Troops.TroopType Int) Entities.Model.Settlement
    | SiegeSettlement Entities.Model.Settlement
    | ImproveBuilding Entities.Model.Settlement Building.Building


type alias EnemyStatus =
    { strengthDiff : Float, turnsTillReached : Float, settlement : Entities.Model.Settlement }


type alias SettlmentDefenseRating =
    { settlement : Entities.Model.Settlement, armyStrengthVariance : Float }


updateAi : AI -> AiRoundActions -> (Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord) -> AI
updateAi ai action moveTowards =
    case action of
        EndRound ->
            ai

        GoSomeWhere p ->
            { ai | lord = moveTowards ai.lord p }

        DoSomething basicAction ->
            ai


getAiAction : AI -> (Entities.Model.Lord -> Vector.Point -> Int) -> List Entities.Model.Lord -> AiRoundActions
getAiAction ai distanceTo enemies =
    case
        List.head
            (List.sortBy (\action -> action.actionValue) (getAiActions ai distanceTo enemies))
    of
        Nothing ->
            EndRound

        Just action ->
            action.action


getAiActions :
    AI
    -> (Entities.Model.Lord -> Vector.Point -> Int)
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getAiActions ai getTurnsToPoint enemies =
    let
        ownSettlementDefenseActions =
            getSettlementDefenseActions ai getTurnsToPoint enemies

        enemySettlementStates =
            List.foldl (\l r -> settlementArmiesStrength l ++ r) [] enemies
    in
    ownSettlementDefenseActions


getSettlementDefenseActions :
    AI
    -> (Entities.Model.Lord -> Vector.Point -> Int)
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementDefenseActions ai getTurnsToPoint enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> evaluateSettlementDefense ai s :: r)
            []
            (settlementDefenseArmyRating ai.lord)


getSettlementAttackActions :
    AI
    -> (Entities.Model.Lord -> Vector.Point -> Int)
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementAttackActions ai getTurnsToPoint enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> evaluateSettlementDefense ai s :: r)
            []
            (settlementDefenseArmyRating ai.lord)



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
        Just
            (AiRoundActionPreference
                (DoSomething (SwapTroops Dict.empty settlementDefenseRating.settlement))
                (settlementDefenseRating.armyStrengthVariance / ai.strategy.defendMultiplier)
            )


estimatedNormalPlayerTroopStrength : Entities.Model.Lord -> Float
estimatedNormalPlayerTroopStrength l =
    let
        x =
            Entities.lordSettlementCount l
    in
    toFloat <| 300 + 50 * x


estimatedNormalVillageTroopStrength : Entities.Model.Lord -> Float
estimatedNormalVillageTroopStrength l =
    toFloat 250


estimatedNormalCastleTroopStrength : Entities.Model.Lord -> Float
estimatedNormalCastleTroopStrength l =
    let
        x =
            toFloat <| Entities.lordSettlementCount l
    in
    400 * ((1 / x) + ((1 - (1 / x)) / (x * x * 0.01 + 1)))


rateSettlementDefense : Entities.Model.Lord -> Int -> Entities.Model.SettlementType -> Float
rateSettlementDefense lord strength entityType =
    case entityType of
        Entities.Model.Village ->
            toFloat strength / estimatedNormalVillageTroopStrength lord

        Entities.Model.Castle ->
            toFloat strength / estimatedNormalCastleTroopStrength lord


settlementDefenseArmyRating : Entities.Model.Lord -> List SettlmentDefenseRating
settlementDefenseArmyRating l =
    List.foldl
        (\s r ->
            SettlmentDefenseRating s
                (rateSettlementDefense l (Troops.sumTroopStats s.entity.army) s.settlementType)
                :: r
        )
        []
        l.land


settlementDefenseStrength :
    AI
    -> Entities.Model.Settlement
    -> List Entities.Model.Lord
    -> Int
settlementDefenseStrength ai s enemies =
    let
        settlementDefenseStrength =
            entityStrength s
    in
    case Entities.landlordOnSettlement s enemies of
        Nothing ->
            settlementDefenseStrength

        Just l ->
            entityStrength l + settlementDefenseStrength


lordArmyComparison : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordArmyComparison l1 l2 =
    let
        diff =
            lordStrength l1.entity / lordStrength l2.entity
    in
    1 + Balancing.lordStrengthDiffFactor * diff


entityStrength : Entities.Model.WorldEntity -> Float
entityStrength e =
    toFloat (Troops.sumTroopStats e.army)
        / estimatedNormalPlayerTroopStrength l


lordStrengthDiff : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordStrengthDiff attacker defender =
    toFloat (Troops.sumTroopStats attacker.entity.army) / toFloat (Troops.sumTroopStats defender.entity.army)
