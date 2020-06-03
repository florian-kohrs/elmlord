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
    case m of
        LordMsg msg l ->
            showLordTileMsg msg ++ " " ++ l.entity.name

        SettlementMsg msg s ->
            showSettlementTileMsg msg ++ " " ++ s.entity.name

        MoveTo _ ->
            "Move here"


type LordTileMsg
    = ViewLord
    | EngageLord


showLordTileMsg : LordTileMsg -> String
showLordTileMsg lordTileMsg =
    case lordTileMsg of
        ViewLord ->
            "View"

        EngageLord ->
            "Engage"


type SettlementTileMsg
    = ViewSettlement
    | EnterSettlement
    | SiegeSettlement


showSettlementTileMsg : SettlementTileMsg -> String
showSettlementTileMsg msg =
    case msg of
        ViewSettlement ->
            "View"

        EnterSettlement ->
            "Enter"

        SiegeSettlement ->
            "Siege"


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
