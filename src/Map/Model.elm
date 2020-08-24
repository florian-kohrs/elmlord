module Map.Model exposing (..)

import Dict
import Entities.Model
import Faction
import Vector


type alias Map =
    Dict.Dict Int MapTile


type alias MapTile =
    { indices : Vector.Point
    , point : Vector.Vector
    , terrain : Terrain
    , settlement : Maybe Entities.Model.Settlement
    , lords : List Entities.Model.Lord
    }


type alias MapTileDesign =
    { backgroundColor : String
    , strokeColor : String
    , strokeWidth : String
    }


type Terrain
    = Grass
    | Water
    | Forest
    | Mountain


type TerrainMoveType
    = CantWalkOn
    | CanWalkOn Float
