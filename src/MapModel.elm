module MapModel exposing (..)

import Browser
import Entities exposing (Lord, Settlement)
import Faction exposing (Faction(..))
import Html.Events exposing (onClick)
import List exposing (..)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


type alias Map =
    List MapTile


type alias MapTile =
    { indices : Vector.Point
    , point : Vector.Vector
    , terrain : Terrain
    , settlement : Maybe Entities.Settlement
    , lords : List Entities.Lord
    , faction : Faction
    }


type Terrain
    = Grass
    | Water
    | Forest


rad : Float
rad =
    0.35



{-
   terrainToColor : Terrain -> String
   terrainToColor t =
       case t of
           Grass ->
               "Green"
-}


terrainToName : Terrain -> String
terrainToName t =
    case t of
        Grass ->
            "Grass"

        Water ->
            "Water"

        Forest ->
            "Forest"


mapToSvg : Map -> Float -> (Point -> a) -> List (Svg a)
mapToSvg m r f =
    List.map (showMapTile r f) m


showMapTile : Float -> (Point -> a) -> MapTile -> Svg a
showMapTile tileRadius f tile =
    polygon
        [ onClick (f tile.indices)
        , fill "green"
        , stroke "black"
        , points (pointsToHexagonPoints (generateHexagonPoints tile.point tileRadius))
        ]
        []


pointsToHexagonPoints : List Vector.Vector -> String
pointsToHexagonPoints =
    List.foldl (\v r -> r ++ String.fromFloat v.xF ++ "," ++ String.fromFloat v.yF ++ " ") ""


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
