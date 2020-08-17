module Balancing exposing (..)

import Entities.Model



-- AI


acceptedSettlementLackOfDefense : Float
acceptedSettlementLackOfDefense =
    0.1



{-
   @params turns -> turns the lord  would  need  to reach the action
-}
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


settlementDefenseBoni : Entities.Model.Settlement -> Entities.Model.Lord -> Float
settlementDefenseBoni s landlord =
    case s.settlementType of
        Entities.Model.Village ->
            0.1

        Entities.Model.Castle ->
            toFloat (List.length landlord.land - 1) * 0.15


addGoldCastle : Float
addGoldCastle =
    1500.0
