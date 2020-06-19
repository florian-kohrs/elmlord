module AI exposing (..)

import Entities
import PathAgent
import Pathfinder
import Types
import Vector


x =
    0



{-
   type alias AI =
       { lord : Entities.Lord, strategy : Strategy, nav : Pathfinder.NavigatableMap }


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


   type alias SettlementStatus =
       { isSiegedBy : Maybe Entities.Lord, turnsTillReached : Float, settlement : Entities.Settlement }


   type alias EnemyStatus =
       { strengthDiff : Float, turnsTillReached : Float, settlement : Entities.Settlement }


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
-}
