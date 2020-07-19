module Map exposing (..)

import Dict
import Entities.Model
import List exposing (..)
import Map.Model exposing (..)
import MapData
import MaybeExt
import Troops
import Vector


setSettlement : MapTile -> Maybe Entities.Model.Settlement -> MapTile
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


getTerrainForPoint : Vector.Point -> Map -> Terrain
getTerrainForPoint p map =
    MaybeExt.foldMaybe .terrain Grass (Dict.get (MapData.hashMapPoint p) map)


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
