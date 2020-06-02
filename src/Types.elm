module Types exposing (..)

import Entities
import Troops exposing (..)
import Vector



-- TODO: Combine ShowSettlement, ShowTroopRecruiting, ShowTroopStationing in 1 Msg


type Msg
    = EndRound
    | EndGame Bool
    | CloseModal
    | ShowSettlement
    | ShowTroopRecruiting
    | ShowTroopStationing
    | ShowBattleView
    | SettlementAction SettlementMsg TroopType
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
            "View Lord"

        ViewSettlement _ ->
            "ViewSettlement"

        MoveTo _ ->
            "Move to"


type SettlementMsg
    = BuyTroops
    | StationTroops
    | TakeTroops


type UiSettlementState
    = StandardView
    | RecruitView
    | StationView
    | BuildingView
