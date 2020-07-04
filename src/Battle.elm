module Battle exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import OperatorExt exposing (..)
import Troops exposing (..)
import Map exposing (Terrain)

evaluateBattleResult : BattleStats -> Terrain -> BattleStats
evaluateBattleResult bS t =
    if bS.siege then
        case bS.settlement of
            Nothing -> 
                bS
            
            Just settle -> 
                evaluateSiegeBattle bS settle t
    else 
        evaluateLordBattle bS t


evaluateSiegeBattle : BattleStats -> Settlement -> Terrain -> BattleStats 
evaluateSiegeBattle bS settle ter =
        if Entities.isLordOnSettlement bS.enemy settle then
            let
                (tEnemy, tSettle) = transferTroops bS.enemy settle
                tempPlayer = bS.player
                newPlayer = { tempPlayer | entity = evaluateBattle tempPlayer.entity tSettle.entity.army ter (getSettlementBonus settle bS.enemy.land)}
                newSettle = { tSettle | entity = evaluateBattle tSettle.entity bS.player.entity.army ter 1}
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
                newPlayer = {tempPlayer | entity = evaluateBattle tempPlayer.entity settle.entity.army ter (getSettlementBonus settle bS.enemy.land)}
                newSettle = {settle | entity = evaluateBattle settle.entity bS.player.entity.army ter 1}
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

    
evaluateLordBattle : BattleStats -> Terrain -> BattleStats
evaluateLordBattle bS ter =
    let
        tempPlayer = bS.player
        tempEnemy = bS.enemy
        newPlayer = {tempPlayer | entity = evaluateBattle tempPlayer.entity bS.enemy.entity.army ter 1}
        newEnemy = {tempEnemy | entity = evaluateBattle tempEnemy.entity bS.player.entity.army ter 1}
    in
        { bS
            | round = bS.round + 1
            , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
            , enemyCasualties = List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army
            , player = lordBattleAftermath newPlayer newPlayer.land
            , enemy = lordBattleAftermath newEnemy newEnemy.land
            , settlement = bS.settlement
            , finished = sumTroops newPlayer.entity.army == 0 || sumTroops newEnemy.entity.army == 0
        }

lordBattleAftermath : Lord -> List Settlement -> Lord
lordBattleAftermath lord settlements = 
        if sumTroops lord.entity.army == 0 then
            case getLordCapital settlements of
                Nothing -> 
                    lord
                
                Just settle ->
                    {lord | entity = Entities.setPosition lord.entity settle.entity.position}

        else 
            lord

    
siegeBattleAftermath : BattleStats -> Settlement -> (Lord, Lord, Bool) 
siegeBattleAftermath bS s = 
        let
            tempPlayer = bS.player
            tempEnemy = bS.enemy
            tempSettle = {s | entity = Entities.updateEntitiesArmy s.entity.army (Entities.updateEntityFaction tempPlayer.entity.faction s.entity)}
        in
            if sumTroops tempPlayer.entity.army > 0 then
                if tempSettle.settlementType == Entities.Castle then
                    handleSettlementTransfer tempPlayer tempEnemy
                else 
                (
                    {tempPlayer | land = tempSettle :: tempPlayer.land}
                    , {tempEnemy | land = List.filter (\x -> x.entity.name /= s.entity.name) tempEnemy.land}
                    , False
                )
            else 
                case getLordCapital tempPlayer.land of
                    Nothing -> 
                        (
                            tempPlayer
                            , {tempEnemy | land = OperatorExt.mapFilter (\v -> {v | entity = Entities.updateEntitiesArmy s.entity.army v.entity}) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                            , False
                        )
                    Just settle -> 
                        (
                            { tempPlayer | entity = Entities.setPosition tempPlayer.entity settle.entity.position}
                            , {tempEnemy | land = OperatorExt.mapFilter (\v -> {v | entity = Entities.updateEntitiesArmy s.entity.army v.entity}) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                            , False
                        )


handleSettlementTransfer : Lord -> Lord -> (Lord, Lord, Bool)
handleSettlementTransfer player enemy = 
            (
                {player | land = List.map (\x -> {x | entity = Entities.updateEntityFaction player.entity.faction x.entity}) 
                              (List.filter (\y -> y.settlementType /= Entities.Castle) enemy.land)
                               ++ player.land}
                , {enemy | land = []}
                , True
            )


-- TODO: Combine the aftermath parts of normal and sieged battles

evaluateBattle : WorldEntity -> List Troop -> Terrain -> Float -> WorldEntity
evaluateBattle w t ter siegeBonus=
    evaluateLordCasualities w (sumTroopsDamage t ter siegeBonus)

evaluateLordCasualities : WorldEntity -> Float -> WorldEntity
evaluateLordCasualities w d =
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



-- checksumTroopsDamage


sumTroopsDamage : List Troop -> Terrain -> Float -> Float
sumTroopsDamage t ter siegeBonus=
    let
        bonusTroopTypes = Map.terrainToBonus ter
    in
    List.foldr (\x y -> siegeBonus * ternary (List.member x.troopType bonusTroopTypes) (Troops.battlefieldBonus x.troopType) 1 * Troops.troopDamage x.troopType * toFloat x.amount + y) 0 t


transferTroops : Lord -> Settlement -> (Lord, Settlement)
transferTroops l s =
    let
        newArmy = List.map2 (\x y -> {amount = x.amount + y.amount, troopType = y.troopType}) l.entity.army s.entity.army
        newLord = { l | entity = Entities.updateEntitiesArmy (List.map (\x -> { x | amount = 0, troopType = x.troopType}) l.entity.army) l.entity }
        newSettlement = { s | entity = Entities.updateEntitiesArmy newArmy s.entity}
    in
        (newLord, newSettlement)


getSettlementBonus : Settlement -> List Settlement -> Float
getSettlementBonus s l = 
    if s.settlementType == Entities.Village then
        1.1
    else 
        List.foldr (\_ y -> 0.1 + y) 1 l

