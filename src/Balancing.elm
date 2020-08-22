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


addGoldCastle : Float
addGoldCastle =
    2500.0
