module MapGenerator exposing (createMap, getNav)

--import Lists exposing (indexedMap, repeat)

import Dict
import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map exposing (Map, MapTile, heighProgressToTerrain)
import MapData exposing (..)
import Noise exposing (noise2d, permutationTable)
import Pathfinder exposing (NavigatableMap)
import Random exposing (initialSeed)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


getNav : Map.Map -> NavigatableMap
getNav map =
    { timeToCrossField =
        \p ->
            case Dict.get (MapData.hashMapPoint p) map of
                Nothing ->
                    Nothing

                Just t ->
                    case Map.terrainToMove t.terrain of
                        Map.CantWalkOn ->
                            Nothing

                        Map.CanWalkOn speedFactor ->
                            Just (1 / speedFactor)
    , getCircumjacentFields =
        \p useEveryTile ->
            let
                sign =
                    if modBy 2 p.x == 0 then
                        1

                    else
                        -1

                canUseTile point =
                    case Dict.get (MapData.hashMapPoint point) map of
                        Nothing ->
                            False

                        Just t ->
                            useEveryTile || Map.canMoveOnTile t
            in
            List.filter (\point -> abs point.x <= mapSize && abs point.y <= mapSize && canUseTile point)
                [ Vector.Point p.x (p.y + 1)
                , Vector.Point p.x (p.y - 1)
                , Vector.Point (p.x + 1) (p.y + sign)
                , Vector.Point (p.x - 1) (p.y + sign)

                --, { p | x = p.x - 1, y = p.y + 1 }
                --, { p | x = p.x - 1, y = p.y - 1 }
                , Vector.Point (p.x + 1) p.y
                , Vector.Point (p.x - 1) p.y
                ]
    , getMinDistanceBetween =
        \p1 p2 ->
            let
                xDiff =
                    toFloat (abs (p1.x - p2.x))

                yDiff =
                    Basics.max 0
                        ((toFloat (abs (p1.y - p2.y)) - xDiff / 2)
                            - toFloat (modBy 2 (round xDiff))
                        )
            in
            xDiff + yDiff
    }


createMap : Map.Map
createMap =
    let
        perm =
            Tuple.first (Noise.permutationTable (Random.initialSeed MapData.seed))
    in
    createMap_ (mapHeight * 2) perm


createMap_ : Int -> Noise.PermutationTable -> Map.Map
createMap_ i n =
    if i >= 0 then
        Dict.union (buildHexagonRow (i - mapHeight) n) (createMap_ (i - 1) n)

    else
        Dict.empty


buildHexagonRow : Int -> Noise.PermutationTable -> Map.Map
buildHexagonRow i =
    buildHexagons i (mapWidth * 2)


buildHexagons : Int -> Int -> Noise.PermutationTable -> Map.Map
buildHexagons height i n =
    let
        indexOffset =
            mapHeight
    in
    if i >= 0 then
        let
            point =
                Vector.Point (i - indexOffset) height
        in
        Dict.insert (MapData.hashMapPoint point)
            (buildHexagon point n)
            (buildHexagons height (i - 1) n)

    else
        Dict.empty


buildHexagon : Vector.Point -> Noise.PermutationTable -> MapTile
buildHexagon p n =
    MapTile
        p
        (MapData.mapPositionForIndex p)
        (getTerrainFor p n)
        Nothing
        []
        Faction.Faction1


getTerrainFor : Vector.Point -> Noise.PermutationTable -> Map.Terrain
getTerrainFor p n =
    let
        xDiff =
            1 - toFloat (abs p.x) / toFloat MapData.mapSize

        yDiff =
            1 - toFloat (abs p.y) / toFloat MapData.mapSize

        noiseP =
            Vector.scale (Vector.toVector p) noiseScale

        height =
            ((Noise.noise2d n noiseP.x noiseP.y + 1) / 2) * sin (Basics.min 1 (Basics.min xDiff yDiff * 2) * pi / 2)
    in
    Map.heighProgressToTerrain height
