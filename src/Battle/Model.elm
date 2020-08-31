module Battle.Model exposing (..)

import Entities.Model
import Troops


type alias BattleStats =
    { attacker : Entities.Model.Lord
    , defender : Entities.Model.Lord
    , round : Int
    , attackerCasualties : Troops.Army
    , defenderCasualties : Troops.Army
    , settlement : Maybe Entities.Model.Settlement
    , finished : Bool
    }
