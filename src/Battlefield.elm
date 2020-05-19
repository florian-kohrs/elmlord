module Battlefield exposing (..)

import Entities exposing (..)
import Troops exposing (..)


type MapTile
    = Ground
    | Water
    | Forest


type alias BattleField =
    { map : List List MapTile }


type alias Battle =
    { attacking : Lord, defending : Lord, ground : BattleField }
