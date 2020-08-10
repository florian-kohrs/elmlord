module Map.Drawer exposing (..)

import BasicDrawing
import Browser
import Dict
import Entities
import Faction exposing (Faction(..))
import Html.Events exposing (onClick)
import List exposing (..)
import ListExt
import Map
import Map.Model exposing (..)
import MapAction.Model
import MapAction.SubModel
import MapData exposing (rad)
import MaybeExt
import Msg
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Troops
import Vector exposing (..)


drawMap : Map -> MapAction.Model.InteractableMapSVG
drawMap m =
    Dict.map (\_ tile -> tileToClickAction tile) m


tileToClickAction : MapTile -> List MapAction.Model.InteractableSvg
tileToClickAction tile =
    MapAction.Model.InteractableSvg (showMapTile tile) (getMapTileAction tile)
        :: MaybeExt.foldMaybe (\img -> [ MapAction.Model.InteractableSvg img [] ]) [] (getImageItemForTile tile)


showMapTile : MapTile -> MapAction.Model.SvgItem
showMapTile tile =
    let
        tileDesign =
            styleMapTile tile
    in
    MapAction.Model.SvgItem MapData.defaultTileZIndex
        (polygon
            [ onClick (Msg.Click tile.indices)
            , Svg.Attributes.overflow "visible"
            , fill tileDesign.backgroundColor
            , stroke tileDesign.strokeColor
            , strokeWidth tileDesign.strokeWidth
            , points (BasicDrawing.pointsToHexagonPoints (BasicDrawing.calculateHexagonPoints tile.point MapData.hexRadius))
            , opacity "0.7"
            ]
            (MaybeExt.foldMaybe List.singleton [] (getImageForTile tile))
        )


getImageItemForTile : MapTile -> Maybe MapAction.Model.SvgItem
getImageItemForTile t =
    Maybe.andThen
        (\img ->
            Just (MapAction.Model.SvgItem MapData.imageTileZIndex img)
        )
        (getImageForTile t)


getImageForTile : MapTile -> Maybe (Svg.Svg Msg.Msg)
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
        (Map.terrainToImageName t.terrain)


styleMapTile : MapTile -> MapTileDesign
styleMapTile tile =
    { backgroundColor = Map.terrainToColor tile.terrain
    , strokeColor = Faction.factionColor tile.faction
    , strokeWidth = "2px"
    }


getMapTileAction : MapTile -> List MapAction.SubModel.MapTileMsg
getMapTileAction tile =
    if Map.canMoveOnTile tile then
        [ MapAction.SubModel.MoveTo tile.indices ]

    else
        []
