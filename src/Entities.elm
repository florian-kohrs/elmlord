module Entities exposing (..)

import Fraction exposing (..)
import List exposing (..)
import Troops exposing (..)
import Vector exposing (..)


type alias Gold =
    Int


type alias Sovereign =
    { entity : WorldEntity, gold : Gold, action : Action, land : List Sovereign }


type Action
    = Wait
    | GoTo Point
    | Chase Sovereign -- -> wouldnt work since its not referenced
    | Siege Settlement


type alias Settlement =
    { entity : WorldEntity, settlementType : SettlementType }


type alias WorldEntity =
    { army : List TroopType, fraction : Fraction, position : Point, name : String }


type SettlementType
    = Village
    | Town
