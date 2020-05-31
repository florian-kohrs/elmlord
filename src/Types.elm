module Types exposing (..)

import Entities
import Vector


type Msg
    = EndRound
    | CloseModal
    | ShowSettlement
    | ShowTroopRecruiting
    | ShowTroopStationing
    | Click Vector.Point


type MapTileMsg
    = ViewLord Entities.Lord
    | ViewSettlement Entities.Settlement
    | MoveTo Vector.Point


type UiSettlementState 
    = StandardView
    | RecruitView
    | StationView
    | BuildingView