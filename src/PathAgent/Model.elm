module PathAgent.Model exposing (Agent)

import Dict exposing (Dict)
import MapData
import MaybeExt
import Pathfinder
import Vector


type alias Agent =
    { target : Maybe Vector.Point, speed : Float, usedMovement : Float }
