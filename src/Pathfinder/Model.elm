module Pathfinder.Model exposing (..)

import Dict exposing (Dict)
import Vector


type PathPart
    = PathPart
        { parent : Maybe PathPart
        , minDistanceToTarget : Float
        , previousDistance : Float
        , position : Vector.Point
        }


type alias Path =
    { target : Vector.Point
    , path : List PathTile
    }


type alias PathTile =
    { indices : Vector.Point
    , timeLoss : Float
    }


type alias PathInfo =
    { nav : NavigatableMap, target : Vector.Point }


type alias NavigatableMap =
    { timeToCrossField : Vector.Point -> Maybe Float
    , getCircumjacentFields : Vector.Point -> Bool -> List Vector.Point
    , getMinDistanceBetween : Vector.Point -> Vector.Point -> Float
    }


type alias PathTailLookup =
    Dict Int ()


type alias PathTails =
    List PathPart
