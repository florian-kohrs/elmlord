module EntitiesDrawer exposing (..)

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
    getImage
        "Lord1.png"
        l.entity.position
        Vector.one


getSvgForSettlement : Entities.Settlement -> Svg.Svg Types.Msg
getSvgForSettlement s =
    getImage
        (Entities.settlementImageName s.settlementType)
        s.entity.position
        Vector.one


getImage : String -> Vector.Point -> Vector.Vector -> Svg.Svg Types.Msg
getImage imgName indices scale =
    let
        pos =
            MapData.mapPositionForIndex indices

        size =
            Vector.Vector (MapData.hexRadius * 1.5) (MapData.hexRadius * 2)
    in
    Svg.image
        [ Svg.Events.onClick (Types.Click indices)
        , Svg.Attributes.x (String.fromFloat (pos.x - size.x / 2))
        , Svg.Attributes.y (String.fromFloat (pos.y - size.y / 2))
        , Svg.Attributes.width (String.fromFloat size.x)
        , Svg.Attributes.height (String.fromFloat size.y)
        , Svg.Attributes.xlinkHref ("../Images/" ++ imgName)

        --, Svg.Attributes.src "../Images/Background.png"
        --, Svg.Attributes.overflow "visible"
        --, Svg.Attributes.fill "red"
        ]
        []


lordZAxis : Int
lordZAxis =
    5


settlementZAxis : Int
settlementZAxis =
    4
