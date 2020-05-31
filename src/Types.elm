module Types exposing (..)

import Entities
import Troops exposing (..)
import Vector

-- TODO: Combine ShowSettlement, ShowTroopRecruiting, ShowTroopStationing in 1 Msg

type Msg
    = EndRound
    | CloseModal
    | ShowSettlement 
    | ShowTroopRecruiting
    | ShowTroopStationing
    | ShowBattleView
    | SettlementAction SettlementMsg TroopType
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