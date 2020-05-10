module Entities exposing (..)

import Faction exposing (..)
import List exposing (..)
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
    { entity : WorldEntity, settlementType : SettlementType, isSieged : Bool }


type alias WorldEntity =
    { army : List TroopType, faction : Faction, position : Point, name : String }


type SettlementType
    = Village
    | Town
