module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)

test : Int
test = 5

{- evaluateBattle : (Lord, Lord) -> (Lord, Lord)
evaluateBattle (a, d) =  -}

sumTroopsDamage : List Troop -> Float
sumTroopsDamage t =
        List.foldr (\x y -> Troops.troopDamage x.troopType * toFloat x.amount + y ) 0 t