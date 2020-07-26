module MapData exposing (..)

import Bitwise
import Vector


hexRadius : Float
hexRadius =
    15


spaceBetweenHexes : Float
spaceBetweenHexes =
    3


mapHeight : Int
mapHeight =
    mapSize


mapWidth : Int
mapWidth =
    mapSize


tileRowXOffset : Int
tileRowXOffset =
    round (hexRadius + (spaceBetweenHexes / 2))


mapSize : Int
mapSize =
    15


seed : Int
seed =
    3


noiseScale : Float
noiseScale =
    0.1


rad : Float
rad =
    0.35


lordZIndex : Int
lordZIndex =
    9


settlementZIndex : Int
settlementZIndex =
    7


settlementBorderZIndex : Int
settlementBorderZIndex =
    6


pathZIndex : Int
pathZIndex =
    5


settlementStrokeWidth : Int
settlementStrokeWidth =
    5


imageTileZIndex : Int
imageTileZIndex =
    2


defaultTileZIndex : Int
defaultTileZIndex =
    0


hashMapPoint : Vector.Point -> Int
hashMapPoint p =
    let
        shiftedP =
            Vector.addPoints p (Vector.Point mapWidth mapHeight)
    in
    Bitwise.shiftLeftBy 16 shiftedP.x + shiftedP.y



{-
   hashToPoint : Int -> Vector.Point
   hashToPoint i =
       let
           x =
               Bitwise.shiftRightBy 16 i
       in
       Vector.addPoints
           (Vector.Point x (i - Bitwise.shiftLeftBy 16 x))
           (Vector.Point -mapWidth -mapHeight)
-}


mapPositionForIndex : Vector.Point -> Vector.Vector
mapPositionForIndex p =
    let
        offset =
            rowXOffset p.x
    in
    Vector.Vector (getXPosForIndex p.x + offset.x) (getYPosForIndex p.y + offset.y)


rowXOffset : Int -> Vector.Vector
rowXOffset x =
    let
        shiftedX =
            x + mapHeight
    in
    Vector.Vector
        -(toFloat shiftedX * (Vector.pointOnCircle hexRadius rad).x)
        (toFloat (modBy 2 shiftedX * tileRowXOffset))


getXPosForIndex : Int -> Float
getXPosForIndex i =
    let
        absI =
            i + mapWidth
    in
    (toFloat absI + 1)
        * hexRadius
        * 2
        + (spaceBetweenHexes * toFloat (absI - 1))


getYPosForIndex : Int -> Float
getYPosForIndex i =
    let
        absI =
            i + mapHeight
    in
    (toFloat absI + 1)
        * Vector.y (Vector.pointOnCircle hexRadius rad)
        * 2
        + (spaceBetweenHexes * toFloat (absI - 1))
