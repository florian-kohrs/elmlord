module AI.AISettlementHandling exposing (..)

import AI.AIGoldManager exposing (..)
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
    toFloat (Troops.sumTroopsStats e.army)


settlementRecruitUsage : Entities.Model.Lord -> Entities.Model.Settlement -> Float
settlementRecruitUsage l s =
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
                            , currentStrength + Troops.getTroopTypeInArmyStats s.recruitLimits t
                            )
                        )
                        ( 0, 0 )
                        Troops.troopTypeList
            in
            toFloat currentRecruitStrength / toFloat maxRecruitStrength
