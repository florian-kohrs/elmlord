module MapGenerator exposing (createMap, getXPosForIndex, getYPosForIndex, hexRadius)

import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lists exposing (indexedMap, repeat)
import MapModel exposing (MapTile, rad)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


hexRadius : Float
hexRadius =
    15


spaceBetweenHexes : Float
spaceBetweenHexes =
    3


hexHeight : Int
hexHeight =
    6


tileRowXOffset : Int
tileRowXOffset =
    round (hexRadius + (spaceBetweenHexes / 2))


hexWidth : Int
hexWidth =
    6


mapScale : Float
mapScale =
    1


getXPosForIndex : Int -> Float
getXPosForIndex i =
    let
        absI =
            i + hexWidth
    in
    (toFloat absI + 1)
        * hexRadius
        * 2
        + (spaceBetweenHexes * toFloat (absI - 1))


getYPosForIndex : Int -> Float
getYPosForIndex i =
    let
        absI =
            i + hexHeight
    in
    (toFloat absI + 1)
        * Vector.y (Vector.pointOnCircle hexRadius rad)
        * 2
        + (spaceBetweenHexes * toFloat (absI - 1))


createMap : List MapTile
createMap =
    createMap_ (hexHeight * 2)


createMap_ : Int -> List MapTile
createMap_ i =
    if i >= 0 then
        buildHexagonRow Vector.zero (i - hexHeight) ++ createMap_ (i - 1)

    else
        []


buildHexagonRow : Vector -> Int -> {- Noise -> -} List MapTile
buildHexagonRow offset i =
    buildHexagons offset
        i
        (hexWidth * 2 - abs i)


buildHexagons : Vector -> Int -> Int -> {- Noise -> -} List MapTile
buildHexagons offset height i =
    let
        indexOffset =
            hexHeight - (abs height // 2)

        rowXOffset =
            Vector.Vector (toFloat (modBy 2 height * tileRowXOffset)) 0
    in
    if i >= 0 then
        buildHexagon (Vector.add offset rowXOffset) height (i - indexOffset)
            :: buildHexagons offset height (i - 1)

    else
        []


buildHexagon : Vector -> Int -> Int -> {- Noise -> -} MapTile
buildHexagon offset h w =
    MapTile
        (Point h w)
        (Vector (getXPosForIndex w + offset.xF) (getYPosForIndex h + offset.yF))
        MapModel.Grass
        Nothing
        []
        Faction.Faction1
