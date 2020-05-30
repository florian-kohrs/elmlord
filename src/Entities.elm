module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
import RedundantDataManager
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Int


type alias Lord =
    { 
        entity : WorldEntity
        , gold : Gold
        , action : Action
        , land : List Settlement
        , moveSpeed : Float 
    }



{- updateLord : Lord -> Lord
   updateLord s =
       let
           goldIncome =
               foldl (cons ((+) 5)) land 0
       in
       { s | gold = s.gold + goldIncome }
-}


type Action
    = Wait
    | GoTo Point
    | Chase Int -- -> references the index of the chased Lord
    | Siege Settlement


actionImportanceMultiplier : Action -> Float
actionImportanceMultiplier a =
    case a of
        Wait ->
            0.8

        GoTo point ->
            1

        Chase int ->
            1.1

        Siege settlement ->
            1.3


type alias Settlement =
    { 
        entity : WorldEntity
        , settlementType : SettlementType
        , income: Float
        , isSieged : Bool }


type alias WorldEntity =
    { 
        army : List Troop
        , faction : Faction
        , position : Point
        , name : String 
    }


setPosition : WorldEntity -> Vector.Point -> WorldEntity
setPosition entity pos =
    { entity | position = pos }


type SettlementType
    = Village
    | Castle


createCapitalFor : WorldEntity -> Settlement
createCapitalFor e =
    { entity = { army = [], faction = e.faction, position = e.position, name = e.name ++ "`s Capital`" }, settlementType = Castle, income = 1.00, isSieged = False }


type alias SettlementInfo =
    { 
        sType : SettlementType
        , position : Vector.Point 
    }


getSettlementFor : Lord -> SettlementInfo -> Settlement
getSettlementFor l info =
    { entity = { army = [], faction = l.entity.faction, position = info.position, name = "" }, settlementType = info.sType, income = 1.00, isSieged = False }


combineSettlementName : Settlement -> String
combineSettlementName settlement = 
    getSettlementNameByType settlement.settlementType ++ " - "++ settlement.entity.name


getSettlementNameByType : SettlementType -> String
getSettlementNameByType s =
    case s of 
        Village -> 
            "Village"

        Castle -> 
            "Castle"

settlementImageName : SettlementType -> String
settlementImageName s =
    getSettlementNameByType s ++ ".png"
