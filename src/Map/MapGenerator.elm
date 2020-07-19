module MapGenerator exposing (createMap)

--import Lists exposing (indexedMap, repeat)

import Dict
import Faction exposing (Faction(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map
import Map.Model
import MapData exposing (..)
import Noise exposing (noise2d, permutationTable)
import Random exposing (initialSeed)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


createMap : Map.Model.Map
createMap =
    let
        perm =
            Tuple.first (Noise.permutationTable (Random.initialSeed MapData.seed))
    in
    createMap_ (mapHeight * 2) perm


createMap_ : Int -> Noise.PermutationTable -> Map.Model.Map
createMap_ i n =
    if i >= 0 then
        Dict.union (buildHexagonRow (i - mapHeight) n) (createMap_ (i - 1) n)

    else
        Dict.empty


buildHexagonRow : Int -> Noise.PermutationTable -> Map.Model.Map
buildHexagonRow i =
    buildHexagons i (mapWidth * 2)


buildHexagons : Int -> Int -> Noise.PermutationTable -> Map.Model.Map
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


buildHexagon : Vector.Point -> Noise.PermutationTable -> Map.Model.MapTile
buildHexagon p n =
    Map.Model.MapTile
        p
        (MapData.mapPositionForIndex p)
        (getTerrainFor p n)
        Nothing
        []
        Faction.Faction1


getTerrainFor : Vector.Point -> Noise.PermutationTable -> Map.Model.Terrain
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
