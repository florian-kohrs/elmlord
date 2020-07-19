module MapAction.SubModel exposing (..)

import Entities.Model
import Vector


type MapTileMsg
    = LordMsg LordTileMsg Entities.Model.Lord
    | SettlementMsg SettlementTileMsg Entities.Model.Settlement
    | MoveTo Vector.Point


type LordTileMsg
    = ViewLord
    | EngageLord


type SettlementTileMsg
    = ViewSettlement
    | EnterSettlement
    | SiegeSettlement
