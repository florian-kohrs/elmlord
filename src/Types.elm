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
    = LordMsg LordTileMsg Entities.Lord
    | SettlementMsg SettlementTileMsg Entities.Settlement
    | MoveTo Vector.Point


mapTileMsgToToolTip : MapTileMsg -> String
mapTileMsgToToolTip m =
    "Unnamed Action"



{- case m of
   ViewLord _ ->
       "Inspect Lord"

   ViewSettlement _ ->
       "Inspect Settlement"

   MoveTo _ ->
       "Move to"
-}


type LordTileMsg
    = ViewLord
    | EngageLord


type SettlementTileMsg
    = ViewSettlement
    | EnterSettlement
    | SiegeSettlement


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
