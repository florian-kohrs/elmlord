module Types exposing (..)

import Entities
import Troops exposing (..)
import Vector
import Entities exposing (BattleStats)




type Msg
    = EndRound
    | EndGame Bool
    | CloseModal
    | BattleAction BattleMsg
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
    = UIMsg SettlementUIMsg
    | TroopMsg SettlementArmyMsg


type SettlementUIMsg =
    ShowSettlement Entities.Settlement
    | ShowBuyTroops Entities.Settlement
    | ShowStationTroops Entities.Settlement

type SettlementArmyMsg =
    BuyTroops Troops.TroopType Entities.Settlement
    | StationTroops Troops.TroopType Entities.Settlement
    | TakeTroops Troops.TroopType Entities.Settlement

type UiSettlementState
    = StandardView
    | RecruitView
    | StationView
    | BuildingView

type BattleMsg 
    = StartBattle String
    | StartSkirmish Entities.BattleStats
    | SkipSkirmishes Entities.BattleStats
    | FleeBattle 
    | EndBattle