module Map exposing (..)

import BasicDrawing
import Browser
import Dict
import Entities exposing (Lord, Settlement)
import Faction exposing (Faction(..))
import Html.Events exposing (onClick)
import List exposing (..)
import ListExt
import MapData exposing (rad)
import MapDrawer
import MaybeExt
<<<<<<< Updated upstream
import Pathfinder exposing (NavigatableMap)
=======
>>>>>>> Stashed changes
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Troops
import Types
import Vector exposing (..)


type alias Map =
    Dict.Dict Int MapTile


type alias MapTile =
    { indices : Vector.Point
    , point : Vector.Vector
    , terrain : Terrain
    , settlement : Maybe Entities.Settlement
    , lords : List Entities.Lord
    , faction : Faction
    }


type alias MapTileDesign =
    { backgroundColor : String
    , strokeColor : String
    , strokeWidth : String
    }


drawMap : Map -> MapDrawer.MapClickAction
drawMap m =
    Dict.map (\_ tile -> tileToClickAction tile) m


tileToClickAction : MapTile -> List MapDrawer.InteractableSvg
tileToClickAction tile =
    MapDrawer.InteractableSvg (showMapTile tile) (getMapTileAction tile)
        :: MaybeExt.foldMaybe (\img -> [ MapDrawer.InteractableSvg img [] ]) [] (getImageItemForTile tile)


setSettlement : MapTile -> Maybe Settlement -> MapTile
setSettlement t s =
    { t | settlement = s }


heighProgressToTerrain : Float -> Terrain
heighProgressToTerrain f =
    if f < 0.2 then
        Water

    else if f < 0.5 then
        Grass

    else if f < 0.85 then
        Forest

    else
        Mountain


type Terrain
    = Grass
    | Water
    | Forest
    | Mountain


getTerrainForPoint : Vector.Point -> Map -> Terrain
getTerrainForPoint p map =
    MaybeExt.foldMaybe .terrain Grass (Dict.get (MapData.hashMapPoint p) map)


type TerrainMoveType
    = CantWalkOn
    | CanWalkOn Float


canMoveOnTile : MapTile -> Bool
canMoveOnTile mapTile =
    case terrainToMove mapTile.terrain of
        CantWalkOn ->
            False

        CanWalkOn _ ->
            True


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


terrainToColor : Terrain -> String
terrainToColor t =
    case t of
        Grass ->
            "#00cd00"

        Water ->
            "#4ca1d2"

        Forest ->
            "#008000"

        Mountain ->
            "#ba8f30"


terrainToImageName : Terrain -> Maybe String
terrainToImageName t =
    case t of
        Grass ->
            Nothing

        Water ->
            Nothing

        Forest ->
            Just "./assets/images/map/tree.png"

        Mountain ->
            Just "./assets/images/map/mountain_icon.png"


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


styleMapTile : MapTile -> MapTileDesign
styleMapTile tile =
    { backgroundColor = terrainToColor tile.terrain
    , strokeColor = Faction.factionColor tile.faction
    , strokeWidth = "2px"
    }


getMapTileAction : MapTile -> List Types.MapTileMsg
getMapTileAction tile =
    if canMoveOnTile tile then
        [ Types.MoveTo tile.indices ]

    else
        []


showMapTile : MapTile -> MapDrawer.SvgItem
showMapTile tile =
    let
        tileDesign =
            styleMapTile tile
    in
    MapDrawer.SvgItem MapData.defaultTileZIndex
        (polygon
            [ onClick (Types.Click tile.indices)
            , Svg.Attributes.overflow "visible"
            , fill tileDesign.backgroundColor
            , stroke tileDesign.strokeColor
            , strokeWidth tileDesign.strokeWidth
            , points (pointsToHexagonPoints (generateHexagonPoints tile.point MapData.hexRadius))
            , opacity "0.7"
            ]
            (MaybeExt.foldMaybe List.singleton [] (getImageForTile tile))
        )


getImageItemForTile : MapTile -> Maybe MapDrawer.SvgItem
getImageItemForTile t =
    Maybe.andThen
        (\img ->
            Just (MapDrawer.SvgItem MapData.imageTileZIndex img)
        )
        (getImageForTile t)


getImageForTile : MapTile -> Maybe (Svg.Svg Types.Msg)
getImageForTile t =
    Maybe.andThen
        (\imageName ->
            Just
                (BasicDrawing.getImage
                    imageName
                    t.indices
                    0.9
                )
        )
        (terrainToImageName t.terrain)


pointsToHexagonPoints : List Vector.Vector -> String
pointsToHexagonPoints =
    List.foldl (\v r -> r ++ String.fromFloat v.x ++ "," ++ String.fromFloat v.y ++ " ") ""


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


terrainToBonus : Terrain -> List Troops.TroopType
terrainToBonus ter =
    case ter of
        Grass ->
            [ Troops.Knight, Troops.Sword ]

        Forest ->
            [ Troops.Archer ]

        Mountain ->
            [ Troops.Spear ]

        Water ->
            []
