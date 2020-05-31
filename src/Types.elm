module Types exposing (..)

import Entities
import Vector


type Msg
    = EndRound
    | CloseModal
    | ShowSettlement
    | Click Vector.Point


type MapTileMsg
    = ViewLord Entities.Lord
    | ViewSettlement Entities.Settlement
    | MoveTo Vector.Point
