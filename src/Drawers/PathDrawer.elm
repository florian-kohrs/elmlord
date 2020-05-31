module PathDrawer exposing (..)

import BasicDrawing
import MapData
import MapDrawer
import Pathfinder
import Svg
import Svg.Attributes
import Svg.Events
import Types
import Vector


drawPath : Float -> Pathfinder.Path -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawPath moveSpeed path dict =
    Tuple.first
        (List.foldl
            (\tile ( newDict, dist ) ->
                ( drawPathPart
                    (min 9 (max 1 (ceiling (dist / moveSpeed))))
                    tile
                    newDict
                , dist + tile.timeLoss
                )
            )
            ( dict, 0 )
            path.path
        )


drawPathPart : Int -> Pathfinder.PathTile -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawPathPart i tile =
    MapDrawer.addToMap
        (MapData.hashMapPoint tile.indices)
        (MapDrawer.InteractableSvg (showPathPart i tile) (getPathPartAction tile))


getPathPartAction : Pathfinder.PathTile -> Maybe MapDrawer.SvgAction
getPathPartAction _ =
    Nothing


showPathPart : Int -> Pathfinder.PathTile -> MapDrawer.SvgItem
showPathPart i tile =
    MapDrawer.SvgItem pathZAxis (getSvgForPathPart i tile.indices)


getSvgForPathPart : Int -> Vector.Point -> Svg.Svg Types.Msg
getSvgForPathPart i p =
    BasicDrawing.getImage
        ("letters/" ++ String.fromInt i ++ ".png")
        p
        Vector.one


pathZAxis =
    1
