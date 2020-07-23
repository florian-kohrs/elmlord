module Balancing exposing (..)

-- AI


acceptedSettlementLackOfDefense : Float
acceptedSettlementLackOfDefense =
    0.1



{-
   @params turns -> turns the lord  would  need  to reach the action
-}


distanceFromAggresiveActionPenalty : Int -> Float
distanceFromAggresiveActionPenalty turns =
    toFloat turns * 0.1


distanceFromDefensiveActionPenalty : Int -> Float
distanceFromDefensiveActionPenalty turns =
    toFloat turns * 0.05



{-
   For now ai lords are heavily against attacking a lord they cant reach in
   this turn
-}


distanceFromAttackLordPenalty : Int -> Float
distanceFromAttackLordPenalty turns =
    toFloat turns



{-
   Additional faktor multiplied to the army strength difference
   determining wether the ai is attacking or fleeing from other lords
-}


lordStrengthDiffFactor : Float
lordStrengthDiffFactor =
    2


improveBuildingFactor : Float
improveBuildingFactor =
    0.5



-- should also consider speed difference
