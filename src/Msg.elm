module Msg exposing (..)

import Battle.Model
import Building
import Entities
import Entities.Model
import MapAction.SubModel
import Troops
import Vector



-- all msg types that are used for the different states
----------------------------------------------------------


type Msg
    = EndRound
    | EndGame Bool
    | CloseModal
    | BattleAction BattleMsg
    | SettlementAction SettlementMsg
    | MapTileAction MapAction.SubModel.MapTileMsg
    | EventAction EventMsg
    | Click Vector.Point


type SettlementMsg
    = UIMsg SettlementUIMsg
    | TroopMsg SettlementArmyMsg


type SettlementUIMsg
    = ShowSettlement Entities.Model.Settlement
    | ShowBuyTroops Entities.Model.Settlement
    | ShowStationTroops Entities.Model.Settlement
    | ShowBuildings Entities.Model.Settlement


type SettlementArmyMsg
    = BuyTroops Troops.TroopType Entities.Model.Settlement
    | StationTroops Troops.TroopType Entities.Model.Settlement
    | TakeTroops Troops.TroopType Entities.Model.Settlement
    | UpgradeBuilding Building.Building Entities.Model.Settlement


type UiSettlementState
    = StandardView
    | RestrictedView
    | RecruitView
    | StationView
    | BuildingView


type EventMsg
    = DeleteEvent Int
    | SwitchEventView
    | ClearEvents


type BattleMsg
    = StartSkirmish Battle.Model.BattleStats
    | SkipSkirmishes Battle.Model.BattleStats
    | FleeBattle Battle.Model.BattleStats
    | EndBattle Battle.Model.BattleStats
