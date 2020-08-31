module Entities.Drawer exposing (..)

import BasicDrawing
import Dict
import Entities exposing (..)
import Entities.Model exposing (..)
import Faction
import ListExt
import Map
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapData
import MaybeExt
import Msg
import Svg
import Svg.Attributes exposing (..)
import Svg.Events
import Vector


drawLord : Lord -> Lord -> MapAction.Model.InteractableMapSVG -> MapAction.Model.InteractableMapSVG
drawLord player l dict =
    let
        drawnLord =
            MapAction.Model.InteractableSvg (showLord l) (getLordAction player l)

        drawnLordBorder =
            MapAction.Model.InteractableSvg (showEntityBorder l.entity) []
    in
    MapAction.addToMap (MapData.hashMapPoint l.entity.position) drawnLord dict
        |> MapAction.addToMap (MapData.hashMapPoint l.entity.position) drawnLordBorder


drawSettlement : Lord -> Settlement -> MapAction.Model.InteractableMapSVG -> MapAction.Model.InteractableMapSVG
drawSettlement player s dict =
    let
        drawnSettlement =
            MapAction.Model.InteractableSvg (showSettlement s) (getSettlementAction player s)

        drawnSettlementBorder =
            MapAction.Model.InteractableSvg (showEntityBorder s.entity) []
    in
    MapAction.addToMap (MapData.hashMapPoint s.entity.position) drawnSettlement dict
        |> MapAction.addToMap (MapData.hashMapPoint s.entity.position) drawnSettlementBorder


getLordAction : Lord -> Lord -> List MapAction.SubModel.MapTileMsg
getLordAction player lord =
    let
        action =
            if
                player.entity.position
                    == lord.entity.position
                    && lord.entity.name
                    /= player.entity.name
                    && not (Entities.isLordInOwnSettlement lord)
            then
                Just MapAction.SubModel.EngageLord

            else
                Nothing
    in
    MapAction.SubModel.LordMsg MapAction.SubModel.ViewLord lord
        :: MaybeExt.foldMaybe (\a -> [ MapAction.SubModel.LordMsg a lord ])
            []
            action


getSettlementAction : Lord -> Settlement -> List MapAction.SubModel.MapTileMsg
getSettlementAction player s =
    let
        action =
            if player.entity.position == s.entity.position then
                if List.member s.entity.position (List.map (\l -> l.entity.position) player.land) then
                    Just MapAction.SubModel.EnterSettlement

                else
                    Just MapAction.SubModel.SiegeSettlement

            else
                Nothing
    in
    MapAction.SubModel.SettlementMsg MapAction.SubModel.ViewSettlement s
        :: MaybeExt.foldMaybe (\a -> [ MapAction.SubModel.SettlementMsg a s ]) [] action


showLord : Lord -> MapAction.Model.SvgItem
showLord lord =
    MapAction.Model.SvgItem MapData.lordZIndex (getSvgForLord lord)


showSettlement : Settlement -> MapAction.Model.SvgItem
showSettlement s =
    MapAction.Model.SvgItem MapData.settlementZIndex (getSvgForSettlement s)


showEntityBorder : WorldEntity -> MapAction.Model.SvgItem
showEntityBorder e =
    MapAction.Model.SvgItem
        MapData.settlementBorderZIndex
        (getSvgBorderFor e.faction e.position)


getSvgForLord : Lord -> Svg.Svg Msg.Msg
getSvgForLord l =
    BasicDrawing.getImage
        (Entities.lordToMapIcon l)
        l.entity.position
        1


getSvgBorderFor : Faction.Faction -> Vector.Point -> Svg.Svg Msg.Msg
getSvgBorderFor f p =
    Svg.polygon
        [ Svg.Events.onClick (Msg.Click p)
        , overflow "visible"
        , stroke (Faction.factionColor f)
        , strokeWidth (String.fromInt MapData.settlementStrokeWidth ++ "px")
        , points (BasicDrawing.pointsToHexagonPoints (BasicDrawing.calculateHexagonPoints (MapData.mapPositionForIndex p) MapData.hexRadius))
        , opacity "0.8"
        ]
        []


getSvgForSettlement : Settlement -> Svg.Svg Msg.Msg
getSvgForSettlement s =
    BasicDrawing.getImage
        (Entities.getSettlementImage s)
        s.entity.position
        1
