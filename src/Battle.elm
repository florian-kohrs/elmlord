module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)

test : Int
test = 5


-- needs refactoring
evaluateBattle : Lord -> List Troop -> Lord
evaluateBattle l t = 
            evaluateLordCasualities l (sumTroopsDamage t)

-- check
sumTroopsDamage : List Troop -> Float
sumTroopsDamage t =
        List.foldr (\x y -> Troops.troopDamage x.troopType * toFloat x.amount + y ) 0 t

evaluateLordCasualities : Lord -> Float -> Lord
evaluateLordCasualities lord d =
            { lord | entity = Entities.updateEntitiesArmy (temp lord.entity.army d (sumTroops lord.entity.army)) lord.entity }

temp : List Troop -> Float -> Float -> List Troop
temp t d a = 
    case t of 
        [] ->
            []

        (x :: xs) -> 
            calcCasualties x (d * (toFloat x.amount / a)) :: temp xs d a


calcCasualties : Troop -> Float -> Troop
calcCasualties t d =
        {t | amount = restrictNegativInt (t.amount - round ( d / Troops.troopDefense t.troopType))}


-- refactor: take this and change this also in other functions (exp. number of troops < 0)
restrictNegativInt : Int -> Int 
restrictNegativInt nmb = 
                ternary (nmb > 0) nmb 0

-- check
calcDefense : Troop -> Float
calcDefense t =
        toFloat t.amount * Troops.troopDefense t.troopType

-- check
calcArmyShare : List Troop -> Troop -> Float
calcArmyShare l t =
        toFloat t.amount / List.foldr (+) 0.0 (List.map (\x -> toFloat x.amount) l)

sumTroops : List Troop -> Float
sumTroops l = 
        List.foldr (+) 0.0 (List.map (\x -> toFloat x.amount) l)

