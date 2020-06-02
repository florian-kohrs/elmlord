module Types exposing (..)

import Entities
import Troops exposing (..)
import Vector


type Msg
    = EndRound
    | CloseModal
    | ShowSettlement
    | ShowTroopRecruiting
    | ShowTroopStationing
    | SettlementAction SettlementMsg TroopType
    | MapTileAction MapTileMsg
    | Click Vector.Point


type MapTileMsg
    = ViewLord Entities.Lord
    | ViewSettlement Entities.Settlement
    | MoveTo Vector.Point


type SettlementMsg
    = BuyTroops
    | StationTroops
    | TakeTroops


type UiSettlementState
    = StandardView
    | RecruitView
    | StationView
    | BuildingView
