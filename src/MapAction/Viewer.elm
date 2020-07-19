module MapAction.Viewer exposing (..)

import Entities.Model
import MapAction.SubModel
import Vector



-- Resolve map actions types that are displayed on the left
-- to the map
----------------------------------------------------------


showLordTileMsg : MapAction.SubModel.LordTileMsg -> String
showLordTileMsg lordTileMsg =
    case lordTileMsg of
        MapAction.SubModel.ViewLord ->
            "View"

        MapAction.SubModel.EngageLord ->
            "Engage"


mapTileMsgToToolTip : MapAction.SubModel.MapTileMsg -> String
mapTileMsgToToolTip m =
    case m of
        MapAction.SubModel.LordMsg msg l ->
            showLordTileMsg msg ++ " " ++ l.entity.name

        MapAction.SubModel.SettlementMsg msg s ->
            showSettlementTileMsg msg ++ " " ++ s.entity.name

        MapAction.SubModel.MoveTo _ ->
            "Move here"


showSettlementTileMsg : MapAction.SubModel.SettlementTileMsg -> String
showSettlementTileMsg msg =
    case msg of
        MapAction.SubModel.ViewSettlement ->
            "View"

        MapAction.SubModel.EnterSettlement ->
            "Enter"

        MapAction.SubModel.SiegeSettlement ->
            "Siege"
