module AI.AISettlementHandling exposing (..)

import AI.Model exposing (..)
import AI.AIGoldManager exposing (..)
import AI.AITroopHandling exposing (..)
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


checkSettlementsForTroops : Int -> AI -> List AiRoundActionPreference
checkSettlementsForTroops targetStrength ai =
  List.foldl (\s r ->
    let
      recruitableTroopsDict =
        AI.AITroopHandling.tryBuyTroopsWithTotalStrenghtFrom ai targetStrength s

      troopStrength =
        Troops.sumTroopStats recruitableTroopsDict
    in
    case      ) [] ai.lord.land
