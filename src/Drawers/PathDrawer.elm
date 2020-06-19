module PathDrawer exposing (..)

import BasicDrawing
import MapData
import MapDrawer
import PathAgent
import Pathfinder
import Svg
import Svg.Attributes
import Svg.Events
import Types
import Vector


drawPath : PathAgent.Agent -> Pathfinder.Path -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawPath agent path dict =
    List.foldl
        (\( t, turns ) newDict ->
            drawPathPart
                (min 9 turns)
                t.indices
                newDict
        )
        dict
        (PathAgent.pathPartsToTime agent path.path)


drawPathPart : Int -> Vector.Point -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawPathPart i p =
    MapDrawer.addToMap
        (MapData.hashMapPoint p)
        (MapDrawer.InteractableSvg (showPathPart i p) (getPathPartAction p))


getPathPartAction : Vector.Point -> List Types.MapTileMsg
getPathPartAction _ =
    []


showPathPart : Int -> Vector.Point -> MapDrawer.SvgItem
showPathPart i p =
    MapDrawer.SvgItem MapData.pathZIndex (getSvgForPathPart i p)


getSvgForPathPart : Int -> Vector.Point -> Svg.Svg Types.Msg
getSvgForPathPart i p =
    BasicDrawing.getImage
        ("letters/" ++ String.fromInt i ++ ".png")
        p
        1
