module Entities.Model exposing (..)

import Building
import Dict
import Faction
import List
import PathAgent.Model
import Troops
import Vector


type alias Gold =
    Float


type alias WorldEntity =
    { army : Troops.Army
    , faction : Faction.Faction
    , position : Vector.Point
    , name : String
    }


type alias Lord =
    { entity : WorldEntity
    , gold : Gold
    , land : List Settlement
    , agent : PathAgent.Model.Agent
    }


type alias Settlement =
    { entity : WorldEntity
    , settlementType : SettlementType
    , recruitLimits : Troops.Army
    , income : Float
    , isSieged : Bool
    , buildings : List Building.Building
    }


type alias SettlementInfo =
    { sType : SettlementType
    , position : Vector.Point
    , name : String
    , faction : Faction.Faction
    }


type SettlementType
    = Village
    | Castle
