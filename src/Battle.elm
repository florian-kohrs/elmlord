module Battle exposing (evaluateBattleResult, siegeBattleAftermath)

import Entities
import Map
import OperatorExt
import Troops



--TODO: Maybe implement autoresolve instead the way in the main.elm


{-| Resolves / Calculate a battle skirmish outcome between (lord vs lord or lord vs siege).
Notice that only one round will be calculated!

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
evaluateBattleResult : Entities.BattleStats -> Map.Terrain -> Entities.BattleStats
evaluateBattleResult bS t =
    if bS.siege then
        case bS.settlement of
            Nothing ->
                bS

            Just settle ->
                evaluateSiegeBattle bS settle t

    else
        evaluateLordBattle bS t


{-| Resolves / Calculate a battle skirmish outcome for sieges

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Settlement}: Takes the settlement that gets sieged (in BattleStats its a Maybe Settlement)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
evaluateSiegeBattle : Entities.BattleStats -> Entities.Settlement -> Map.Terrain -> Entities.BattleStats
evaluateSiegeBattle bS settle ter =
    if Entities.isLordOnSettlement bS.enemy settle then
        let
            ( tEnemy, tSettle ) =
                transferTroops bS.enemy settle

            tempPlayer =
                bS.player

            newPlayer =
                { tempPlayer | entity = evaluateBattle tempPlayer.entity tSettle.entity.army ter (Entities.getSettlementBonus settle bS.enemy.land) }

            newSettle =
                { tSettle | entity = evaluateBattle tSettle.entity bS.player.entity.army ter 1 }
        in
        { bS
            | round = bS.round + 1
            , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
            , enemyCasualties = List.map2 Troops.troopDifferences tSettle.entity.army newSettle.entity.army
            , player = newPlayer
            , enemy = tEnemy
            , settlement = Just newSettle
            , finished = Troops.sumTroops newPlayer.entity.army == 0 || Troops.sumTroops newSettle.entity.army == 0
        }

    else
        let
            tempPlayer =
                bS.player

            newPlayer =
                { tempPlayer | entity = evaluateBattle tempPlayer.entity settle.entity.army ter (Entities.getSettlementBonus settle bS.enemy.land) }

            newSettle =
                { settle | entity = evaluateBattle settle.entity bS.player.entity.army ter 1 }
        in
        { bS
            | round = bS.round + 1
            , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
            , enemyCasualties = List.map2 Troops.troopDifferences settle.entity.army newSettle.entity.army
            , player = newPlayer
            , enemy = bS.enemy
            , settlement = Just newSettle
            , finished = Troops.sumTroops newPlayer.entity.army == 0 || Troops.sumTroops newSettle.entity.army == 0
        }


{- constructBattleResult : Entities.Lord -> Entities.Lord -> Maybe Entities.Settlement -> Entities.WorldEntity -> Entities.WorldEntity -> Entities.BattleStats
constructBattleResult bS p e s pew cew =
    { bS
        | round = bS.round + 1
        , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army p.entity.army
        , enemyCasualties = List.map2 Troops.troopDifferences pew cew
        , player = p
        , enemy = e
        , settlement = s
        , finished = Troops.sumTroops newPlayer.entity.army == 0 || Troops.sumTroops newSettle.entity.army == 0
    } -}


evaluateLordBattle : Entities.BattleStats -> Map.Terrain -> Entities.BattleStats
evaluateLordBattle bS ter =
    let
        tempPlayer =
            bS.player

        tempEnemy =
            bS.enemy

        newPlayer =
            { tempPlayer | entity = evaluateBattle tempPlayer.entity bS.enemy.entity.army ter 1 }

        newEnemy =
            { tempEnemy | entity = evaluateBattle tempEnemy.entity bS.player.entity.army ter 1 }
    in
    { bS
        | round = bS.round + 1
        , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
        , enemyCasualties = List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army
        , player = lordBattleAftermath newPlayer newPlayer.land
        , enemy = lordBattleAftermath newEnemy newEnemy.land
        , settlement = bS.settlement
        , finished = Troops.sumTroops newPlayer.entity.army == 0 || Troops.sumTroops newEnemy.entity.army == 0
    }


lordBattleAftermath : Entities.Lord -> List Entities.Settlement -> Entities.Lord
lordBattleAftermath lord settlements =
    if Troops.sumTroops lord.entity.army == 0 then
        case Entities.getLordCapital settlements of
            Nothing ->
                lord

            Just settle ->
                { lord | entity = Entities.setPosition lord.entity settle.entity.position }

    else
        lord


siegeBattleAftermath : Entities.BattleStats -> Entities.Settlement -> ( Entities.Lord, Entities.Lord, Bool )
siegeBattleAftermath bS s =
    let
        tempPlayer =
            bS.player

        tempEnemy =
            bS.enemy

        tempSettle =
            { s | entity = Entities.updateEntitiesArmy s.entity.army (Entities.updateEntityFaction tempPlayer.entity.faction s.entity) }
    in
    if Troops.sumTroops tempPlayer.entity.army > 0 then
        if tempSettle.settlementType == Entities.Castle then
            handleSettlementTransfer tempPlayer tempEnemy

        else
            ( { tempPlayer | land = tempSettle :: tempPlayer.land }
            , { tempEnemy | land = List.filter (\x -> x.entity.name /= s.entity.name) tempEnemy.land }
            , False
            )

    else
        case Entities.getLordCapital tempPlayer.land of
            Nothing ->
                ( tempPlayer
                , { tempEnemy | land = OperatorExt.mapFilter (\v -> { v | entity = Entities.updateEntitiesArmy s.entity.army v.entity }) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                , False
                )

            Just settle ->
                ( { tempPlayer | entity = Entities.setPosition tempPlayer.entity settle.entity.position }
                , { tempEnemy | land = OperatorExt.mapFilter (\v -> { v | entity = Entities.updateEntitiesArmy s.entity.army v.entity }) identity (\x -> x.entity.name == s.entity.name) tempEnemy.land }
                , False
                )


handleSettlementTransfer : Entities.Lord -> Entities.Lord -> ( Entities.Lord, Entities.Lord, Bool )
handleSettlementTransfer player enemy =
    ( { player
        | land =
            List.map (\x -> { x | entity = Entities.updateEntityFaction player.entity.faction x.entity })
                (List.filter (\y -> y.settlementType /= Entities.Castle) enemy.land)
                ++ player.land
      }
    , { enemy | land = [] }
    , True
    )



-- TODO: Combine the aftermath parts of normal and sieged battles


evaluateBattle : Entities.WorldEntity -> List Troops.Troop -> Map.Terrain -> Float -> Entities.WorldEntity
evaluateBattle w t ter siegeBonus =
    evaluateLordCasualities w (sumTroopsDamage t ter siegeBonus)


evaluateLordCasualities : Entities.WorldEntity -> Float -> Entities.WorldEntity
evaluateLordCasualities w d =
    { w | army = calcTroopCasualties w.army d (Troops.sumTroops w.army) }


calcTroopCasualties : List Troops.Troop -> Float -> Float -> List Troops.Troop
calcTroopCasualties t d a =
    case t of
        [] ->
            []

        x :: xs ->
            calcCasualties x (d * (toFloat x.amount / a)) :: calcTroopCasualties xs d a


calcCasualties : Troops.Troop -> Float -> Troops.Troop
calcCasualties t d =
    { t | amount = restrictNegativInt (t.amount - round (d / Troops.troopDefense t.troopType)) }


restrictNegativInt : Int -> Int
restrictNegativInt nmb =
    OperatorExt.ternary (nmb > 0) nmb 0



-- checksumTroopsDamage


sumTroopsDamage : List Troops.Troop -> Map.Terrain -> Float -> Float
sumTroopsDamage t ter siegeBonus =
    let
        bonusTroopTypes =
            Map.terrainToBonus ter
    in
    List.foldr (\x y -> siegeBonus * OperatorExt.ternary (List.member x.troopType bonusTroopTypes) (Troops.battlefieldBonus x.troopType) 1 * Troops.troopDamage x.troopType * toFloat x.amount + y) 0 t


transferTroops : Entities.Lord -> Entities.Settlement -> ( Entities.Lord, Entities.Settlement )
transferTroops l s =
    let
        newArmy =
            List.map2 (\x y -> { amount = x.amount + y.amount, troopType = y.troopType }) l.entity.army s.entity.army

        newLord =
            { l | entity = Entities.updateEntitiesArmy (List.map (\x -> { x | amount = 0, troopType = x.troopType }) l.entity.army) l.entity }

        newSettlement =
            { s | entity = Entities.updateEntitiesArmy newArmy s.entity }
    in
    ( newLord, newSettlement )
