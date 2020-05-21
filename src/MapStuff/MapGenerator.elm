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
            case Dict.get (Vector.showPoint p) map of
                Nothing ->
                    9000

                Just t ->
                    case Map.terrainToMove t.terrain of
                        Map.CantWalkOn ->
                            9000

                        Map.CanWalkOn speedFactor ->
                            1 / speedFactor
    , getCircumjacentFields =
        \p ->
            let
                sign =
                    if modBy 2 p.x == 0 then
                        -1

                    else
                        1

                canUseTile point =
                    case Dict.get (Vector.showPoint point) map of
                        Nothing ->
                            False

                        Just t ->
                            Map.canMoveOnTile t
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
        Dict.union (buildHexagonRow Vector.zero (i - mapHeight) n) (createMap_ (i - 1) n)

    else
        Dict.empty


buildHexagonRow : Vector -> Int -> Noise.PermutationTable -> Map.Map
buildHexagonRow offset i =
    buildHexagons offset
        i
        (mapWidth * 2
         {--abs i-}
        )


buildHexagons : Vector -> Int -> Int -> Noise.PermutationTable -> Map.Map
buildHexagons offset height i n =
    let
        indexOffset =
            mapHeight

        -- - (abs height // 2)
        rowXOffset =
            Vector.Vector -(((toFloat i) * (Vector.pointOnCircle hexRadius rad).x))  (toFloat (modBy 2 i * tileRowXOffset)) 
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
        (Vector (getXPosForIndex p.x + offset.x) (getYPosForIndex p.y + offset.y))
        (getTerrainFor p n)
        Nothing
        []
        Faction.Faction1


getTerrainFor : Vector.Point -> Noise.PermutationTable -> Map.Terrain 
getTerrainFor p n = 
    let 
        xDiff = 1 - (toFloat (abs p.x) )/ (toFloat MapData.mapSize )
        yDiff = 1 - (toFloat (abs p.y) )/ (toFloat MapData.mapSize )
        noiseP = Vector.scale (Vector.toVector p) noiseScale 
        height = ((Noise.noise2d n noiseP.x noiseP.y + 1) / 2) * sin ( Basics.min 1 (Basics.min xDiff yDiff * 2) * pi / 2)
    
    in 
    Map.heighProgressToTerrain height
