module BasicDrawing exposing (..)

import MapData
import Msg
import Svg
import Svg.Attributes
import Svg.Events
import Vector


getImage : String -> Vector.Point -> Float -> Svg.Svg Msg.Msg
getImage imgName indices scale =
    let
        pos =
            MapData.mapPositionForIndex indices

        size =
            Vector.scale (Vector.Vector (MapData.hexRadius * 1.5) (MapData.hexRadius * 2)) scale
    in
    Svg.image
        [ Svg.Events.onClick (Msg.Click indices)
        , Svg.Attributes.x (String.fromFloat (pos.x - size.x / 2))
        , Svg.Attributes.y (String.fromFloat (pos.y - size.y / 2))
        , Svg.Attributes.width (String.fromFloat size.x)
        , Svg.Attributes.height (String.fromFloat size.y)
        , Svg.Attributes.xlinkHref imgName

        --, Svg.Attributes.src "../Images/Background.png"
        --, Svg.Attributes.overflow "visible"
        --, Svg.Attributes.fill "red"
        ]
        []


calculateHexagonPoints : Vector.Vector -> Float -> List Vector.Vector
calculateHexagonPoints v r =
    let
        topLeft =
            Vector.pointOnCircle r MapData.rad

        middleLeft =
            Vector.pointOnCircle r 0

        bottomLeft =
            Vector.flipOnY topLeft

        tl =
            Vector.add topLeft v

        ml =
            Vector.add middleLeft v

        bl =
            Vector.add bottomLeft v

        tr =
            Vector.add (Vector.flipOnX topLeft) v

        mr =
            Vector.add (Vector.flipOnX middleLeft) v

        br =
            Vector.add (Vector.flipOnX bottomLeft) v
    in
    [ tl, ml, bl, br, mr, tr ]


pointsToHexagonPoints : List Vector.Vector -> String
pointsToHexagonPoints =
    List.foldl (\v r -> r ++ String.fromFloat v.x ++ "," ++ String.fromFloat v.y ++ " ") ""
