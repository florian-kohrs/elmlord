module MapGenerator exposing (createMap, getNav, getXPosForIndex, getYPosForIndex, hexRadius, mapSize)

import Dict
import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lists exposing (indexedMap, repeat)
import MapModel exposing (Map, MapTile, heighProgressToTerrain, rad)
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
    9


noiseScale : Float
noiseScale =
    0.1


getNav : MapModel.Map -> NavigatableMap
getNav map =
    { timeToCrossField =
        \p ->
            case Dict.get (Vector.showPoint p) map of
                Nothing ->
                    9000

                Just t ->
                    case MapModel.terrainToMove t.terrain of
                        MapModel.CantWalkOn ->
                            9000

                        MapModel.CanWalkOn speedFactor ->
                            1 / speedFactor
    , getCircumjacentFields =
        \p ->
            let
                sign =
                    if modBy 2 p.y == 0 then
                        -1

                    else
                        1

                canUseTile point =
                    case Dict.get (Vector.showPoint point) map of
                        Nothing ->
                            False

                        Just t ->
                            MapModel.canMoveOnTile t
            in
            List.filter (\point -> abs point.x <= mapSize && abs point.y <= mapSize && canUseTile point)
                [ Vector.Point p.x (p.y + 1)
                , Vector.Point p.x (p.y - 1)
                , Vector.Point (p.x + sign) (p.y + 1)
                , Vector.Point (p.x + sign) (p.y - 1)

                --, { p | x = p.x - 1, y = p.y + 1 }
                --, { p | x = p.x - 1, y = p.y - 1 }
                , Vector.Point (p.x + 1) p.y
                , Vector.Point (p.x - 1) p.y
                ]

    {- List.filter (\point -> abs point.x <= mapSize && abs point.y <= mapSize)
       (List.foldl
           (\c r -> List.foldl (\c2 r2 -> Point (p.x + c) (p.y + c2) :: r2) r (List.range -1 1))
           []
           (List.range -1 1)
       )
    -}
    , getMinDistanceBetween =
        \p1 p2 ->
            let
                yDiff =
                    toFloat (abs (p1.y - p2.y))

                xDiff =
                    Basics.max 0
                        (toFloat (abs (p1.x - p2.x)) - yDiff / 2)
            in
            xDiff + yDiff
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


createMap : MapModel.Map
createMap =
    let
        perm =
            Tuple.first (Noise.permutationTable (Random.initialSeed seed))
    in
    createMap_ (mapHeight * 2) perm


createMap_ : Int -> Noise.PermutationTable -> MapModel.Map
createMap_ i n =
    if i >= 0 then
        Dict.union (buildHexagonRow Vector.zero (i - mapHeight) n) (createMap_ (i - 1) n)

    else
        Dict.empty


buildHexagonRow : Vector -> Int -> Noise.PermutationTable -> MapModel.Map
buildHexagonRow offset i =
    buildHexagons offset
        i
        (mapWidth * 2
         {--abs i-}
        )


buildHexagons : Vector -> Int -> Int -> Noise.PermutationTable -> MapModel.Map
buildHexagons offset height i n =
    let
        indexOffset =
            mapHeight

        -- - (abs height // 2)
        rowXOffset =
            Vector.Vector (toFloat (modBy 2 height * tileRowXOffset)) 0
    in
    if i >= 0 then
        let
            point =
                Vector.Point (i - indexOffset) height
        in
        Dict.insert (Vector.showPoint point)
            (buildHexagon (Vector.add offset rowXOffset) point n)
            (buildHexagons offset height (i - 1) n)

    else
        Dict.empty


buildHexagon : Vector.Vector -> Vector.Point -> Noise.PermutationTable -> MapTile
buildHexagon offset p n =
    MapTile
        p
        (Vector (getXPosForIndex p.x + offset.xF) (getYPosForIndex p.y + offset.yF))
        (getTerrainFor p n)
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
