module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)


evaluateBattle : Lord -> List Troop -> Lord
evaluateBattle l t =
    evaluateLordCasualities l (sumTroopsDamage t)

evaluateBattle2 : WorldEntity -> List Troop -> WorldEntity
evaluateBattle2 w t =
    evaluateLordCasualities2 w (sumTroopsDamage t)


evaluateLordCasualities : Lord -> Float -> Lord
evaluateLordCasualities lord d =
    { lord | entity = Entities.updateEntitiesArmy (calcTroopCasualties lord.entity.army d (sumTroops lord.entity.army)) lord.entity }

evaluateLordCasualities2 : WorldEntity -> Float -> WorldEntity
evaluateLordCasualities2 w d =
    { w | army = calcTroopCasualties w.army d (sumTroops w.army) }

calcTroopCasualties : List Troop -> Float -> Float -> List Troop
calcTroopCasualties t d a =
    case t of
        [] ->
            []

        x :: xs ->
            calcCasualties x (d * (toFloat x.amount / a)) :: calcTroopCasualties xs d a


calcCasualties : Troop -> Float -> Troop
calcCasualties t d =
    { t | amount = restrictNegativInt (t.amount - round (d / Troops.troopDefense t.troopType)) }


restrictNegativInt : Int -> Int
restrictNegativInt nmb =
    ternary (nmb > 0) nmb 0


sumTroops : List Troop -> Float
sumTroops l =
    List.foldr (+) 0.0 (List.map (\x -> toFloat x.amount) l)



-- check


sumTroopsDamage : List Troop -> Float
sumTroopsDamage t =
    List.foldr (\x y -> Troops.troopDamage x.troopType * toFloat x.amount + y) 0 t
