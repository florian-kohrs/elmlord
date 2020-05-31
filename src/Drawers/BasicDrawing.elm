module BasicDrawing exposing (..)

import MapData
import Svg
import Svg.Attributes
import Svg.Events
import Types
import Vector


getImage : String -> Vector.Point -> Vector.Vector -> Svg.Svg Types.Msg
getImage imgName indices scale =
    let
        pos =
            MapData.mapPositionForIndex indices

        size =
            Vector.Vector (MapData.hexRadius * 1.5) (MapData.hexRadius * 2)
    in
    Svg.image
        [ Svg.Events.onClick (Types.Click indices)
        , Svg.Attributes.x (String.fromFloat (pos.x - size.x / 2))
        , Svg.Attributes.y (String.fromFloat (pos.y - size.y / 2))
        , Svg.Attributes.width (String.fromFloat size.x)
        , Svg.Attributes.height (String.fromFloat size.y)
        , Svg.Attributes.xlinkHref ("assets/images/" ++ imgName)

        --, Svg.Attributes.src "../Images/Background.png"
        --, Svg.Attributes.overflow "visible"
        --, Svg.Attributes.fill "red"
        ]
        []
