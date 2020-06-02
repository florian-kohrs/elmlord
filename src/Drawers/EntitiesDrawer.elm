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


drawLord : Entities.Lord -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawLord l =
    let
        drawnLord =
            MapDrawer.InteractableSvg (showLord l) (getLordAction l)
    in
    MapDrawer.addToMap (MapData.hashMapPoint l.entity.position) drawnLord


drawSettlement : Entities.Settlement -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
drawSettlement s =
    let
        drawnSettlement =
            MapDrawer.InteractableSvg (showSettlement s) (getSettlementAction s)
    in
    MapDrawer.addToMap (MapData.hashMapPoint s.entity.position) drawnSettlement


getLordAction : Entities.Lord -> Maybe MapDrawer.SvgAction
getLordAction lord =
    Just (MapDrawer.SvgAction ("View " ++ lord.entity.name) (Types.ViewLord lord))


getSettlementAction : Entities.Settlement -> Maybe MapDrawer.SvgAction
getSettlementAction s =
    Just (MapDrawer.SvgAction ("View " ++ s.entity.name) (Types.ViewSettlement s))


showLord : Entities.Lord -> MapDrawer.SvgItem
showLord lord =
    MapDrawer.SvgItem lordZAxis (getSvgForLord lord)


showSettlement : Entities.Settlement -> MapDrawer.SvgItem
showSettlement s =
    MapDrawer.SvgItem settlementZAxis (getSvgForSettlement s)


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


lordZAxis : Int
lordZAxis =
    6


settlementZAxis : Int
settlementZAxis =
    5
