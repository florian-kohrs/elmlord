module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
import Pathfinder
import RedundantDataManager
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Int


type alias Lord =
    { entity : WorldEntity, gold : Gold, action : Action, land : List Settlement, moveSpeed : Float }



{- updateLord : Lord -> Lord
   updateLord s =
       let
           goldIncome =
               foldl (cons ((+) 5)) land 0
       in
       { s | gold = s.gold + goldIncome }
-}


type alias Action =
    { actionType : ActionType, actionMotive : ActionMotive }


type ActionMotive
    = AttackLord
    | Siege
    | Defend
    | Flee


type ActionType
    = Wait
    | Travel Point (Maybe Pathfinder.Path)


type alias Settlement =
    { entity : WorldEntity, settlementType : SettlementType, isSieged : Bool }


type alias WorldEntity =
    { army : List TroopType, faction : Faction, position : Point, name : String }


setPosition : WorldEntity -> Vector.Point -> WorldEntity
setPosition entity pos =
    { entity | position = pos }


type SettlementType
    = Village
    | Town


createCapitalFor : WorldEntity -> Settlement
createCapitalFor e =
    { entity = { army = [], faction = e.faction, position = e.position, name = e.name ++ "`s Capital`" }, settlementType = Town, isSieged = False }


type alias SettlementInfo =
    { sType : SettlementType, position : Vector.Point }


getSettlementFor : Lord -> SettlementInfo -> Settlement
getSettlementFor l info =
    { entity = { army = [], faction = l.entity.faction, position = info.position, name = "" }, settlementType = info.sType, isSieged = False }


settlementImageName : SettlementType -> String
settlementImageName s =
    case s of
        Village ->
            "Village.png"

        Town ->
            "Town.png"
