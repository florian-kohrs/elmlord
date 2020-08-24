module AI.AISettlementHandling exposing (..)

import AI.AIGoldManager exposing (..)
import AI.Model exposing (..)
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



{-
   Sums the troop strength of the settlement troops and adds the landlords troops if
   he is on the same position
-}


settlementDefenseStrength :
    Entities.Model.Settlement
    -> Maybe Entities.Model.Lord
    -> Int
settlementDefenseStrength s extraDefender =
    case extraDefender of
        Nothing ->
            Troops.sumArmyStats s.entity.army

        Just l ->
            Troops.sumArmyStats l.entity.army + Troops.sumArmyStats s.entity.army


settlementRecruitUsage : Entities.Model.Lord -> Entities.Model.Settlement -> Troops.Army -> Float
settlementRecruitUsage l s a =
    case
        Maybe.andThen
            (\c -> Building.getBuilding Building.Quarters c.buildings)
        <|
            Entities.getLordCapital l.land
    of
        Nothing ->
            0

        Just quarters ->
            let
                ( maxRecruitStrength, currentRecruitStrength ) =
                    List.foldl
                        (\t ( maxStrength, currentStrength ) ->
                            ( maxStrength + Troops.sumTroopStats t (Entities.settlementTroopsRecruitLimit s quarters.level t)
                            , currentStrength + Troops.getTroopTypeInArmyStats a t
                            )
                        )
                        ( 0, 0 )
                        Troops.troopTypeList
            in
            toFloat currentRecruitStrength / toFloat maxRecruitStrength
