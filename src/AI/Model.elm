module AI.Model exposing (..)

import Building
import Dict
import Entities.Model
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


showAiRoundAction : AiRoundActions -> String
showAiRoundAction aiRoundActions =
    case aiRoundActions of
        EndRound ->
            "End Round"

        GoSomeWhere p ->
            "Go to " ++ Vector.showPoint p

        DoSomething basicAction ->
            showBasicAction basicAction


getAiRoundActionDestination : AiRoundActions -> Maybe Vector.Point
getAiRoundActionDestination a =
    case a of
        EndRound ->
            Nothing

        GoSomeWhere p ->
            Just p

        DoSomething basicAction ->
            Just <| getBasicActionDestination basicAction


type BasicAction
    = AttackLord Entities.Model.Lord
    | HireTroops (Dict.Dict Troops.TroopType Int) Entities.Model.Settlement
    | SwapTroops (Dict.Dict Troops.TroopType Int) Entities.Model.Settlement
    | SiegeSettlement Entities.Model.Settlement
    | ImproveBuilding Entities.Model.Settlement Building.Building


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


type alias EnemyStatus =
    { strengthDiff : Float, turnsTillReached : Float, settlement : Entities.Model.Settlement }


type alias SettlmentDefenseRating =
    { settlement : Entities.Model.Settlement, armyStrengthVariance : Float }
