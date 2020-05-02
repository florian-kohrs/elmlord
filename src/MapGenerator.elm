module MapGenerator exposing (..)

import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lists exposing (indexedMap, repeat)
import MapModel exposing (Hexagon(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


hexRadius : Float
hexRadius =
    10


spaceBetweenHexes : Float
spaceBetweenHexes =
    5


totalIndices : Float -> Float
totalIndices f =
    2 * f + 1


hexHeight : Int
hexHeight =
    6


tileHeightOffset : Int
tileHeightOffset =
    hexRadius + round (spaceBetweenHexes / 2)


hexWidth : Int
hexWidth =
    6


mapScale : Float
mapScale =
    1


getXPosForIndex : Int -> Int
getXPosForIndex i =
    let
        absI =
            i + hexWidth
    in
    (absI + 0.5)
        * hexRadius
        * 2
        + (spaceBetweenHexes * (absI - 1))


getYPosForIndex : Int -> Int
getYPosForIndex i =
    let
        absI =
            i + hexHeight
    in
    (absI + 0.5)
        * hexRadius
        * 2
        + (spaceBetweenHexes * (absI - 1))


createMap : List MapTile
createMap =
    createMap (hexHeight * 2 + 1)


createMap_ : Int -> List MapTile
createMap_ i =
    if i > 0 then
        buildHexagonColumn (i - hexHeight) ++ createMap_ (i - 1)

    else
        []


buildHexagonColumn : Vector -> Int -> {- Noise -> -} List MapTile
buildHexagonColumn offset i =
    buildHexagons offset i (hexRange * 2 - abs i)


buildHexagons : Vector -> Int -> Int {- Noise -> -} -> List MapTile
buildHexagons offset height i =
    let
        rowXOffset =
            Vector.Vector (modBy (tileHeightOffset * 2) (tileHeightOffset * i)) 0
    in
    if i >= 0 then
        buildHexagon (Vector.add offset rowXOffset) height (i - hexWidth)
            :: buildHexagons height (i - 1)

    else
        []


buildHexagon : Vector -> Int -> Int -> MapTile
buildHexagon offset h w =
    MapTile
        (Point h w)
        (Vector (getXPosForIndex w + offset.xF) (getYPosForIndex h + offset.yF))
        MapModel.Grass
        Nothing
        []
        Faction.Faction1
