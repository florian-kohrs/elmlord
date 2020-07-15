module AI exposing (..)

import Dict
import Entities
import PathAgent
import Pathfinder
import Types
import Vector


type alias AI =
    { lord : Entities.Lord, strategy : ActionMultipliers }


type alias ActionMultipliers =
    { siegeM : Float, defendM : Float, battleM : Float, growArmyM : Float }


type Strategy
    = Siege
    | Defend
    | Battle
    | BigArmy


type alias Action =
    { actionType : ActionType, urgency : Float }


type ActionType
    = DefendSettlement Entities.Settlement
    | Attacklord Entities.Lord
    | GoToSettlement SettlementInteraction
    | SiegeSettlement Entities.Settlement


type SettlementInteraction
    = StationTroops
    | TakeTroops
    | ImproveBuilding
    | HireTroops


type AiRoundActions
    = EndRound
    | GoSomeWhere Vector.Point
    | DoSomething BasicAction


type BasicAction
    = AttackLord Entities.Lord
    | HireTroops (Dict.Dict Entities.TroopType Int) Entities.Settlement
    | SwapTroops (Dict.Dict Entities.TroopType Int) Entities.Settlement
    | SiegeSettlement Entities.Settlement


type alias SettlementStatus =
    { isSiegedBy : Maybe Entities.Lord, turnsTillReached : Float, settlement : Entities.Settlement }


type alias EnemyStatus =
    { strengthDiff : Float, turnsTillReached : Float, settlement : Entities.Settlement }

type alias EntityDefenseRating =
  {entity : Entities.WorldEntity, armyStrength : Float}

type EntityType =
   LordType
  | Settlement Entities.SettlementType

estimatedNormalPlayerTroopStrength : Lord -> Int
estimatedNormalPlayerTroopStrength =
    let
        x = Entities.lordSettlementCount l
    in
    300 + 50 * x

estimatedNormalVillageTroopStrength : Lord -> Int
estimatedNormalVillageTroopStrength l =
    250

estimatedNormalCastleTroopStrength : Lord -> Int
estimatedNormalCastleTroopStrength l =
  let
      x = Entities.lordSettlementCount l
  in
    400 * ((1 / x) + ((1 - (1 / x)) / (x*x *0.01 + 1)))



armyStrengthEvaluator : Lord -> Int -> EntityType -> Int
armyStrengthEvaluator lord strength entityType =
    case entityType of
      LordType ->
        strength / estimatedNormalPlayerTroopStrength lord
      Settlement Entities.Village ->
        strength / estimatedNormalVillageTroopStrength lord
      Settlement Entities.Castle ->
        strength / estimatedNormalCastleTroopStrength lord

entityArmyStrength : Entities.Lord -> List EntityDefenseRating
entityArmyStrength l =
    list.foldl (\s -> EntityDefenseRating s.entity (Troops.sumTroopStats s.entity.army) (Settlement s.settlementType)) [] l.land :: EntityDefenseRating l.entity (Troops.sumTroopStats l.entity.army) LordType

getAiAction : AI -> List Entities.Lord -> AiRoundActions
getAiAction ai lordEntitiesList =
    let
        remainingMovement = Entities.getLordRemainingMovement ai.lord
    in
    if remainingMovement == 0 then
      EndRound
    else
      --evaulate  weak enemy castles  and too weak enemy castles.  apply  distance  as penalty

settlementStates : AI -> List Entities.Lord -> List SettlementStatus
settlementStates ai others =
    List.map
        (\s ->
            SettlementStatus (settlementSiegedBy s others)
                (ai.getDistance s.entity.position)
                s
        )
        ai.lord.land


settlementSiegedBy : Entities.Settlement -> List Entities.Lord -> Maybe Entities.Lord
settlementSiegedBy s =
    List.foldr
        (\l r ->
            if l.entity.position == s.entity.position then
                Just l

            else
                r
        )
        Nothing


strengthDiff : Entities.Lord -> Entities.Lord -> Float
strengthDiff attacker defender =
    0.5
