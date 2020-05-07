module MapModel exposing (..)

import Browser
import Dict
import Entities exposing (Lord, Settlement)
import Faction exposing (Faction(..))
import Html.Events exposing (onClick)
import List exposing (..)
import Pathfinder exposing (NavigatableMap)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


type alias Map =
    Dict.Dict String MapTile


type alias MapTile =
    { indices : Vector.Point
    , point : Vector.Vector
    , terrain : Terrain
    , settlement : Maybe Entities.Settlement
    , lords : List Entities.Lord
    , faction : Faction
    }


setSettlement : MapTile -> Maybe Settlement -> MapTile
setSettlement t s =
    { t | settlement = s }


heighProgressToTerrain : Float -> Terrain
heighProgressToTerrain f =
    if f < 0.25 then
        Water

    else if f < 0.5 then
        Grass

    else if f < 0.75 then
        Forest

    else
        Mountain


type Terrain
    = Grass
    | Water
    | Forest
    | Mountain


type TerrainMoveType
    = CantWalkOn
    | CanWalkOn Float


terrainToMove : Terrain -> TerrainMoveType
terrainToMove t =
    case t of
        Grass ->
            CanWalkOn 1

        Water ->
            CantWalkOn

        Forest ->
            CanWalkOn 0.8

        Mountain ->
            CanWalkOn 0.5


rad : Float
rad =
    0.35


terrainToColor : Terrain -> String
terrainToColor t =
    case t of
        Grass ->
            "Green"

        Water ->
            "Blue"

        Forest ->
            "DarkGreen"

        Mountain ->
            "DarkKhaki"


terrainToName : Terrain -> String
terrainToName t =
    case t of
        Grass ->
            "Grass"

        Water ->
            "Water"

        Forest ->
            "Forest"

        Mountain ->
            "Mountain"


mapToSvg : Map -> Float -> (Point -> a) -> List (Svg a)
mapToSvg =
    mapWithPathToSvg []


mapWithPathToSvg : List Vector.Point -> Map -> Float -> (Point -> a) -> List (Svg a)
mapWithPathToSvg ps m r f =
    List.map (showMapTile ps r f) (Dict.values m)


showMapTile : List Vector.Point -> Float -> (Point -> a) -> MapTile -> Svg a
showMapTile ps tileRadius f tile =
    let
        colorString =
            if List.any (Vector.pointEqual tile.indices) ps then
                "Orange"

            else
                terrainToColor tile.terrain
    in
    polygon
        [ onClick (f tile.indices)
        , fill colorString
        , stroke "black"
        , points (pointsToHexagonPoints (generateHexagonPoints tile.point tileRadius))
        ]
        []


pointsToHexagonPoints : List Vector.Vector -> String
pointsToHexagonPoints =
    List.foldl (\v r -> r ++ String.fromFloat v.xF ++ "," ++ String.fromFloat v.yF ++ " ") ""



{-
   intialSettlementMapSetup : List Entities.Settlement -> Map -> Map
   intialSettlementMapSetup settlements =
       List.map (\( k, tile ) ->  setSettlement tile (List.head (List.filter (\s -> Vector.pointEqual tile.indices s.entity.position) settlements)))
-}


generateHexagonPoints : Vector -> Float -> List Vector.Vector
generateHexagonPoints v r =
    let
        topLeft =
            Vector.pointOnCircle r rad

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
