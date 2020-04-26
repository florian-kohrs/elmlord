module Sovereigns exposing (..)


import List exposing (..)
import Fraction exposing (..)
import Troops exposing (..)
import Vector exposing (..)


type alias Gold = Int


type alias Sovereign =
  {entity : WorldEntity, gold : Gold, action Action }

type Action
  = Wait
  | GoTo Point
  | Chase Sovereign -- -> woudnt work since its not referenced

type alias Settlement =
  {entity : WorldEntity, settlementType : SettlementType}


type alias WorldEntity
  {army : List TroopType, fraction : Fraction, position : Point, name : String}


type SettlementType
  = Village
  | Town
