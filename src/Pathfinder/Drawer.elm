module Pathfinder.Drawer exposing (..)

import BasicDrawing
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapData
import Msg
import PathAgent
import PathAgent.Model
import Pathfinder.Model exposing (..)
import Svg
import Svg.Attributes
import Svg.Events
import Vector


drawPath : PathAgent.Model.Agent -> Path -> MapAction.Model.MapClickAction -> MapAction.Model.MapClickAction
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


drawPathPart : Int -> Vector.Point -> MapAction.Model.MapClickAction -> MapAction.Model.MapClickAction
drawPathPart i p =
    MapAction.addToMap
        (MapData.hashMapPoint p)
        (MapAction.Model.InteractableSvg (showPathPart i p) (getPathPartAction p))


getPathPartAction : Vector.Point -> List MapAction.SubModel.MapTileMsg
getPathPartAction _ =
    []


showPathPart : Int -> Vector.Point -> MapAction.Model.SvgItem
showPathPart i p =
    MapAction.Model.SvgItem MapData.pathZIndex (getSvgForPathPart i p)


getSvgForPathPart : Int -> Vector.Point -> Svg.Svg Msg.Msg
getSvgForPathPart i p =
    BasicDrawing.getImage
        ("./assets/images/letters/" ++ String.fromInt i ++ ".png")
        p
        1
