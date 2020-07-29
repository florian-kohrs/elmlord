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
