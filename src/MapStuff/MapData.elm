module MapData exposing (..)

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
    20


mapScale : Float
mapScale =
    1


seed : Int
seed =
    9


noiseScale : Float
noiseScale =
    0.1


rad : Float
rad =
    0.35


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
