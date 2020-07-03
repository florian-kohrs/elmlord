module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)

evaluateBattleResult : BattleStats -> BattleStats
evaluateBattleResult bS =
    if bS.siege then
        case bS.settlement of
            Nothing -> 
                bS
            
            Just settle -> 
                evaluateSiegeBattle bS settle
    else 
        evaluateLordBattle bS


evaluateSiegeBattle : BattleStats -> Settlement -> BattleStats 
evaluateSiegeBattle bS settle =
        if Entities.isLordOnSettlement bS.enemy settle then
            let
                (tEnemy, tSettle) = transferTroops bS.enemy settle
                tempPlayer = bS.player
                newPlayer = { tempPlayer | entity = evaluateBattle2 tempPlayer.entity tSettle.entity.army}
                newSettle = { tSettle | entity = evaluateBattle2 tSettle.entity bS.player.entity.army}
            in
                    { bS
                        | round = bS.round + 1
                        , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
                        , enemyCasualties = List.map2 Troops.troopDifferences tSettle.entity.army newSettle.entity.army
                        , player = newPlayer
                        , enemy = tEnemy
                        , settlement = Just newSettle
                        , finished = sumTroops newPlayer.entity.army == 0 || sumTroops newSettle.entity.army == 0
                    }
        else   
            let
                tempPlayer = bS.player
                newPlayer = {tempPlayer | entity = evaluateBattle2 tempPlayer.entity settle.entity.army}
                newSettle = {settle | entity = evaluateBattle2 settle.entity bS.player.entity.army}
            in
                { bS
                        | round = bS.round + 1
                        , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
                        , enemyCasualties = List.map2 Troops.troopDifferences settle.entity.army newSettle.entity.army
                        , player = newPlayer
                        , enemy = bS.enemy
                        , settlement = Just newSettle
                        , finished = sumTroops newPlayer.entity.army == 0 || sumTroops newSettle.entity.army == 0
                }



calcSiegeBattle : Lord -> Lord -> Settlement -> (Lord, Lord, Settlement)
calcSiegeBattle attacker defender settle = 
    let
        (dLord, dSettle) = transferTroops defender settle
        newAttacker = { attacker | entity = evaluateBattle2 attacker.entity settle.entity.army}
        newSettle = { dSettle | entity = evaluateBattle2 dSettle.entity attacker.entity.army}
    in
        (newAttacker, dLord, newSettle)
    



evaluateLordBattle : BattleStats -> BattleStats
evaluateLordBattle bS =
    let
        tempPlayer = bS.player
        tempEnemy = bS.enemy
        newPlayer = {tempPlayer | entity = evaluateBattle2 tempPlayer.entity bS.enemy.entity.army}
        newEnemy = {tempEnemy | entity = evaluateBattle2 tempEnemy.entity bS.player.entity.army}
    in
        { bS
            | round = bS.round + 1
            , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
            , enemyCasualties = List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army
            , player = newPlayer
            , enemy = newEnemy
            , settlement = bS.settlement
            , finished = sumTroops newPlayer.entity.army == 0 || sumTroops newEnemy.entity.army == 0
        }



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


transferTroops : Lord -> Settlement -> (Lord, Settlement)
transferTroops l s =
    let
        newArmy = List.map2 (\x y -> {amount = x.amount + y.amount, troopType = y.troopType}) l.entity.army s.entity.army
        newLord = { l | entity = Entities.updateEntitiesArmy (List.map (\x -> { x | amount = 0, troopType = x.troopType}) l.entity.army) l.entity }
        newSettlement = { s | entity = Entities.updateEntitiesArmy newArmy s.entity}
    in
        (newLord, newSettlement)
        

siegeBattleAftermath : BattleStats -> Settlement -> (Lord, Lord) 
siegeBattleAftermath bS s = 
        let
            tempPlayer = bS.player
            tempEnemy = bS.enemy
            tempSettle = {s | entity = Entities.updateEntitiesArmy s.entity.army (Entities.updateEntityFaction tempPlayer.entity.faction s.entity)}
        in
            if sumTroops tempPlayer.entity.army > 0 then
                (
                    {tempPlayer | land = tempSettle :: tempPlayer.land}
                    , {tempEnemy | land = List.filter (\x -> x.entity.name /= s.entity.name) tempEnemy.land}
                )
            else 
                case getLordCapital tempPlayer.land of
                    Nothing -> 
                        (
                            tempPlayer
                            , {tempEnemy | land = OperatorExt.mapFilter (\v -> {v | entity = Entities.updateEntitiesArmy s.entity.army v.entity}) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                        )
                    Just settle -> 
                        (
                            { tempPlayer | entity = Entities.setPosition tempPlayer.entity settle.entity.position}
                            , {tempEnemy | land = OperatorExt.mapFilter (\v -> {v | entity = Entities.updateEntitiesArmy s.entity.army v.entity}) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                        )

        
