module MapGenerator exposing (createMap, getNav, getXPosForIndex, getYPosForIndex, hexRadius, mapSize)

import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lists exposing (indexedMap, repeat)
import MapModel exposing (MapTile, heighProgressToTerrain, rad)
import Noise exposing (noise2d, permutationTable)
import Pathfinder exposing (NavigatableMap)
import Random exposing (initialSeed)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


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
    5


getNav : NavigatableMap
getNav =
    { getCircumjacentFields =
        \p -> [ Point 1 1 ]

    --List.filter (\point -> abs point.x <= mapSize && abs point.y <= mapSize)
    --List.foldl (\c r -> List.foldl (\c2 r2 -> Point (p.x + c) (p.y + c2) :: r2) r (List.range -1 1)) [] (List.range -1 1)
    , getMinDistanceBetween = \p1 p2 -> abs (p1.x - p2.x) + abs (p1.y - p2.y)
    }


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


createMap : List MapTile
createMap =
    let
        perm =
            Tuple.first (Noise.permutationTable (Random.initialSeed seed))
    in
    createMap_ (mapHeight * 2) perm


createMap_ : Int -> Noise.PermutationTable -> List MapTile
createMap_ i n =
    if i >= 0 then
        buildHexagonRow Vector.zero (i - mapHeight) n ++ createMap_ (i - 1) n

    else
        []


buildHexagonRow : Vector -> Int -> Noise.PermutationTable -> List MapTile
buildHexagonRow offset i =
    buildHexagons offset
        i
        (mapWidth * 2
         {--abs i-}
        )


buildHexagons : Vector -> Int -> Int -> Noise.PermutationTable -> List MapTile
buildHexagons offset height i n =
    let
        indexOffset =
            mapHeight

        -- - (abs height // 2)
        rowXOffset =
            Vector.Vector (toFloat (modBy 2 height * tileRowXOffset)) 0
    in
    if i >= 0 then
        buildHexagon (Vector.add offset rowXOffset) height (i - indexOffset) n
            :: buildHexagons offset height (i - 1) n

    else
        []


buildHexagon : Vector -> Int -> Int -> Noise.PermutationTable -> MapTile
buildHexagon offset h w n =
    MapTile
        (Point h w)
        (Vector (getXPosForIndex w + offset.xF) (getYPosForIndex h + offset.yF))
        (getTerrainFor (Point h w) n)
        Nothing
        []
        Faction.Faction1


getTerrainFor : Vector.Point -> Noise.PermutationTable -> MapModel.Terrain
getTerrainFor p n =
    let
        noiseP =
            Vector.scale (Vector.toVector p) noiseScale

        height =
            (Noise.noise2d n noiseP.xF noiseP.yF + 1) / 2
    in
    MapModel.heighProgressToTerrain height


noiseScale : Float
noiseScale =
    0.1
