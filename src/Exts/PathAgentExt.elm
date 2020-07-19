module PathAgentExt exposing (..)

--shift functions to other modules

import Dict exposing (Dict)
import Entities
import Map
import MapData
import MapDrawer
import MapGenerator
import MaybeExt
import PathAgent
import Pathfinder
import Types
import Vector


canMoveToPoint : MapDrawer.MapClickAction -> Vector.Point -> Bool
canMoveToPoint dict p =
    MapDrawer.hasActionOnPoint p (Types.MoveTo p) dict


getPathTo : Vector.Point -> Vector.Point -> Map.Map -> Maybe Pathfinder.Path
getPathTo from to map =
    if canMoveToPoint (Map.drawMap map) to then
        Pathfinder.getPath
            from
            (Pathfinder.PathInfo (MapGenerator.getNav map) to)

    else
        Nothing


moveLordOnPath : Map.Map -> Entities.Lord -> Vector.Point -> Entities.Lord
moveLordOnPath map l target =
    case getPathTo l.entity.position target map of
        Nothing ->
            l

        Just path ->
            let
                ( usedMove, point ) =
                    PathAgent.moveAlongPath path l.entity.position l.agent
            in
            { l
                | agent = PathAgent.setUsedMovement usedMove l.agent
                , entity = Entities.setPosition l.entity point
            }
