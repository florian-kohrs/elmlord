module AI.AISettlementHandling exposing (..)

import AI.AIGoldManager exposing (..)
import AI.AITroopHandling exposing (..)
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



--evaluates if any settlements controlled by the current ai have insufficent defense


evaluateSettlementDefense : AI -> Entities.Model.Settlement -> Maybe AiRoundActionPreference
evaluateSettlementDefense ai s =
    if
        settlementLackOfTroopStrength ai s
            - AI.AITroopHandling.acceptedLackOfDefenseStrength
            < 0
    then
        Nothing

    else if AI.AITroopHandling.hasTroopsToSatisfySettlementDefense ai then
        Just
            (AiRoundActionPreference
                (DoSomething
                    (SwapTroops
                        (AI.AITroopHandling.takeDispensableTroopsWithMaxStrength
                            ai.lord.entity.army
                            (round <| AI.AITroopHandling.estimatedNormalPlayerTroopStrength ai)
                            (settlementLackOfTroopStrength ai s)
                        )
                        s
                    )
                )
                (clamp
                    0
                    2
                    (AI.AITroopHandling.estimatedSettlementDefenseStrength ai s.settlementType
                        / max 1 (toFloat (Troops.sumTroopStats s.entity.army))
                    )
                )
            )

    else
        Nothing


settlementLackOfTroopStrength : AI -> Entities.Model.Settlement -> Int
settlementLackOfTroopStrength ai s =
    round (AI.AITroopHandling.estimatedSettlementDefenseStrength ai s.settlementType)
        - Troops.sumTroopStats s.entity.army



{-
   Sums the troop strength of the settlement troops and adds the landlords troops if
   he is on the same position
-}


settlementDefenseStrength :
    AI
    -> Entities.Model.Settlement
    -> List Entities.Model.Lord
    -> Int
settlementDefenseStrength ai s enemies =
    let
        settlementDefense =
            round <| entityStrength s.entity
    in
    case Entities.landlordOnSettlement s enemies of
        Nothing ->
            settlementDefense

        Just l ->
            round (entityStrength l.entity) + settlementDefense


entityStrength : Entities.Model.WorldEntity -> Float
entityStrength e =
    toFloat (Troops.sumTroopStats e.army)
