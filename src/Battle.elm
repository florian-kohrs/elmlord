module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)

test : Int
test = 5


-- needs refactoring
evaluateBattle : Lord -> Lord
evaluateBattle a = 
            evaluateLordCasualities a (sumTroopsDamage a.entity.army)

sumTroopsDamage : List Troop -> Float
sumTroopsDamage t =
        List.foldr (\x y -> Troops.troopDamage x.troopType * toFloat x.amount + y ) 0 t

evaluateLordCasualities : Lord -> Float -> Lord
evaluateLordCasualities lord d =
            { lord | entity = Entities.updateEntitiesArmy (temp lord.entity.army d) lord.entity }
            --{ lord | entity = Entities.updateEntitiesArmy (calcCasualties lord.entity.army d 1.0) lord.entity }

temp : List Troop -> Float -> List Troop
temp t d = 
    case t of 
        [] ->
            []

        (x :: xs) -> 
            calcCasualties x d (calcArmyShare t x) :: temp xs d


calcCasualties : Troop -> Float -> Float -> Troop
calcCasualties t d s=
        { t | amount = round ((calcDefense t - (d * s)) / calcDefense t) * t.amount }

calcDefense : Troop -> Float
calcDefense t =
        toFloat t.amount * Troops.troopDefense t.troopType

calcArmyShare : List Troop -> Troop -> Float
calcArmyShare l t =
        toFloat t.amount / toFloat (List.foldr (\x y -> x.amount + y) 0 l)