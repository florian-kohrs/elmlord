module Types exposing (..)

import Entities
import Troops exposing (..)
import Vector



-- TODO: Combine ShowSettlement, ShowTroopRecruiting, ShowTroopStationing in 1 Msg


type Msg
    = EndRound
    | EndGame Bool
    | CloseModal
    | ShowBattleView
    | SettlementAction SettlementMsg
    | MapTileAction MapTileMsg
    | Click Vector.Point


type MapTileMsg
    = ViewLord Entities.Lord
    | ViewSettlement Entities.Settlement
    | MoveTo Vector.Point


mapTileMsgToToolTip : MapTileMsg -> String
mapTileMsgToToolTip m =
    case m of
        ViewLord _ ->
            "Inspect Lord"

        ViewSettlement _ ->
            "Inspect Settlement"

        MoveTo _ ->
            "Move to"


type SettlementMsg
    = BuyTroops Troops.TroopType Entities.Settlement Entities.Lord
    | StationTroops Troops.TroopType
    | TakeTroops Troops.TroopType
    | ShowSettlement Entities.Settlement
    | ShowBuyTroops Entities.Settlement
    | ShowStationTroops Entities.Settlement


type UiSettlementState
    = StandardView
    | RecruitView
    | StationView
    | BuildingView

 