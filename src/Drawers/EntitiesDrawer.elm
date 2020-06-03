module EntitiesDrawer exposing (..)

import BasicDrawing
import Dict
import Entities
import ListExt
import Map
import MapData
import MapDrawer
import MaybeExt
import Svg
import Svg.Attributes
import Svg.Events
import Types
import Vector


drawLord : Entities.Lord -> Entities.Lord -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawLord player l =
    let
        drawnLord =
            MapDrawer.InteractableSvg (showLord l) (getLordAction player l)
    in
    MapDrawer.addToMap (MapData.hashMapPoint l.entity.position) drawnLord


drawSettlement : Entities.Lord -> Entities.Settlement -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawSettlement player s =
    let
        drawnSettlement =
            MapDrawer.InteractableSvg (showSettlement s) (getSettlementAction player s)
    in
    MapDrawer.addToMap (MapData.hashMapPoint s.entity.position) drawnSettlement


getLordAction : Entities.Lord -> Entities.Lord -> List Types.MapTileMsg
getLordAction player lord =
    let
        action =
            if
                player.entity.position
                    == lord.entity.position
                    && lord.entity.name
                    /= player.entity.name
            then
                Just Types.EngageLord

            else
                Nothing
    in
    Types.LordMsg Types.ViewLord lord
        :: MaybeExt.foldMaybe (\a -> [ Types.LordMsg a lord ])
            []
            action


getSettlementAction : Entities.Lord -> Entities.Settlement -> List Types.MapTileMsg
getSettlementAction player s =
    let
        action =
            if player.entity.position == s.entity.position then
                if List.member s.entity.position (List.map (\l -> l.entity.position) player.land) then
                    Just Types.EnterSettlement

                else
                    Just Types.SiegeSettlement

            else
                Nothing
    in
    Types.SettlementMsg Types.ViewSettlement s
        :: MaybeExt.foldMaybe (\a -> [ Types.SettlementMsg a s ]) [] action


showLord : Entities.Lord -> MapDrawer.SvgItem
showLord lord =
    MapDrawer.SvgItem MapData.lordZIndex (getSvgForLord lord)


showSettlement : Entities.Settlement -> MapDrawer.SvgItem
showSettlement s =
    MapDrawer.SvgItem MapData.settlementZIndex (getSvgForSettlement s)


getSvgForLord : Entities.Lord -> Svg.Svg Types.Msg
getSvgForLord l =
    BasicDrawing.getImage
        "Lord1.png"
        l.entity.position
        1


getSvgForSettlement : Entities.Settlement -> Svg.Svg Types.Msg
getSvgForSettlement s =
    BasicDrawing.getImage
        (Entities.settlementImageName s.settlementType)
        s.entity.position
        1
