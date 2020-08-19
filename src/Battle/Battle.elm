module Battle exposing (applyBattleAftermath, evaluateBattleResult, fleeBattle, getBattleSiegeStats, getLordBattleStats, siegeBattleAftermath, siegeBattleSetDefender, skipBattle)

import Balancing
import Battle.Model exposing (..)
import Dict
import DictExt
import Entities
import Entities.Lords
import Entities.Model exposing (Gold)
import Map
import Map.Model
import MaybeExt
import OperatorExt
import Troops


battleFleeTroopLoss : Float
battleFleeTroopLoss =
    0.3


{-| Resolves / Calculate a battle skirmish outcome between (lord vs lord or lord vs siege).
Notice that only one round will be calculated!

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
evaluateBattleResult : BattleStats -> Map.Model.Terrain -> BattleStats
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
evaluateSiegeBattle : BattleStats -> Entities.Model.Settlement -> Map.Model.Terrain -> BattleStats
evaluateSiegeBattle bS settle ter =
    let
        ( transferedDefender, transferedSettle ) =
            siegeBattleSetDefender bS settle

        tempAttacker =
            bS.attacker

        newAttacker =
            { tempAttacker | entity = evaluateBattle tempAttacker.entity transferedSettle.entity.army ter 1 1 }

        newSettle =
            { transferedSettle | entity = evaluateBattle transferedSettle.entity bS.attacker.entity.army ter (Entities.getSettlementBonus settle bS.defender.land) (Entities.getAttackerBonus (Entities.getLordCapital bS.defender.land)) }

        attackerCasualties =
            calculateEntityCasualties bS.attacker.entity.army newAttacker.entity.army

        defenderCasualties =
            calculateEntityCasualties transferedSettle.entity.army newSettle.entity.army
    in
    constructBattleResult bS newAttacker transferedDefender (Just newSettle) attackerCasualties defenderCasualties


{-| Resolves / Calculate a battle skirmish outcome for lord battles

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
evaluateLordBattle : BattleStats -> Map.Model.Terrain -> BattleStats
evaluateLordBattle bS ter =
    let
        tempAttacker =
            bS.attacker

        tempDefender =
            bS.defender

        newAttacker =
            { tempAttacker | entity = evaluateBattle tempAttacker.entity bS.defender.entity.army ter 1 1 }

        newDefender =
            { tempDefender | entity = evaluateBattle tempDefender.entity bS.attacker.entity.army ter 1 1 }

        attackerCasualties =
            calculateEntityCasualties bS.attacker.entity.army newAttacker.entity.army

        defenderCasualties =
            calculateEntityCasualties bS.defender.entity.army newDefender.entity.army
    in
    constructBattleResult bS newAttacker newDefender bS.settlement attackerCasualties defenderCasualties


{-| Construct a generic battle result for both sieges and normal battles

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Lord}: Takes the attacker lord
    @param {Lord}: Takes the defender lord
    @param {Maybe Settlement}: Takes for sieges a settlement therefore only as a maybe data structure
    @param {(List Troop, List Troop)}: Takes the calculated casualties for both sides

-}
constructBattleResult : BattleStats -> Entities.Model.Lord -> Entities.Model.Lord -> Maybe Entities.Model.Settlement -> Troops.Army -> Troops.Army -> BattleStats
constructBattleResult bS attacker defender settle aCasu dCasu =
    { bS
        | round = bS.round + 1
        , attackerCasualties = aCasu
        , defenderCasualties = dCasu
        , attacker = lordBattleAftermath attacker bS.settlement
        , defender = lordBattleAftermath defender bS.settlement
        , settlement = settle
        , finished = Troops.sumTroops attacker.entity.army == 0 || checkDefenderArmy defender settle
    }



-- battle aftermath functions
-- like position resets, settlement transfers, etc.
-------------------------------------------------------------------------------------


{-| Check if the player lost the normal battle, if thats the case his position gets a reset to his
capital position

    @param {Lord}: Takes the attacker-/defender lord

-}
lordBattleAftermath : Entities.Model.Lord -> Maybe Entities.Model.Settlement -> Entities.Model.Lord
lordBattleAftermath l settlement =
    if
        (Troops.sumTroops l.entity.army == 0)
            && MaybeExt.foldMaybe
                (\s ->
                    not
                        (Entities.isLandlord s l
                            && (Entities.isLordOnSettlement l s
                                    || Troops.sumTroops s.entity.army
                                    > 0
                               )
                        )
                )
                True
                settlement
    then
        case Entities.getLordCapital l.land of
            Nothing ->
                l

            Just settle ->
                { l | entity = Entities.setPosition l.entity settle.entity.position }

    else
        l


siegeBattleAftermath : BattleStats -> Entities.Model.Settlement -> ( Entities.Model.Lord, Entities.Model.Lord )
siegeBattleAftermath bS s =
    let
        attacker =
            bS.attacker

        defender =
            bS.defender
    in
    if Troops.sumTroops s.entity.army <= 0 then
        if s.settlementType == Entities.Model.Castle then
            handleSettlementTransfer (getGoldBonus attacker) defender (\y -> y.settlementType /= Entities.Model.Castle) []

        else
            handleSettlementTransfer attacker defender (\y -> y.entity.name == s.entity.name) (List.filter (\y -> y.entity.name /= s.entity.name) defender.land)

    else
        ( attacker, { defender | land = updateSettlementBattleField s defender.land } )


fleeBattle : Entities.Lords.LordList -> BattleStats -> Entities.Lords.LordList
fleeBattle ls bS =
    let
        tempPlayer =
            bS.attacker

        tempEnemy =
            bS.defender

        playerAfterGoldLoss =
            if bS.siege then
                tempPlayer

            else
                { tempPlayer | gold = tempPlayer.gold / 2 }

        newPlayer =
            Entities.updatePlayerArmy
                playerAfterGoldLoss
                (Dict.map (\k v -> round (toFloat v * (1 - battleFleeTroopLoss))) bS.attacker.entity.army)

        newEnemy =
            { tempEnemy | gold = tempEnemy.gold + tempPlayer.gold / 2 }
    in
    Entities.Lords.updateLord newEnemy <| Entities.Lords.updateLord newPlayer ls


applyBattleAftermath : Entities.Lords.LordList -> BattleStats -> Entities.Lords.LordList
applyBattleAftermath ls bs =
    let
        ( tAL, tDL ) =
            if bs.siege then
                ( bs.attacker, bs.defender )

            else
                updatedBattleLordFunds bs.attacker bs.defender
    in
    case bs.settlement of
        Nothing ->
            Entities.Lords.updateLord (lordBattleAftermath tDL Nothing) <| Entities.Lords.updateLord (lordBattleAftermath tAL Nothing) ls

        Just s ->
            let
                ( newAttacker, newDefender ) =
                    siegeBattleAftermath bs s
            in
            Entities.Lords.updateLord newDefender <| Entities.Lords.updateLord newAttacker ls


getBattleSiegeStats : Entities.Model.Lord -> Entities.Lords.LordList -> Entities.Model.Settlement -> Maybe BattleStats
getBattleSiegeStats l ls s =
    Maybe.andThen
        (\defender ->
            Just
                { attacker = l
                , defender = defender
                , round = 1
                , attackerCasualties = Troops.emptyTroops
                , defenderCasualties = Troops.emptyTroops
                , settlement = Just s
                , siege = True
                , finished = False
                }
        )
        (Entities.findLordWithSettlement
            s
            (Entities.Lords.lordListToList ls)
        )


getLordBattleStats : Entities.Model.Lord -> Entities.Model.Lord -> BattleStats
getLordBattleStats attacker defender =
    { attacker = attacker
    , defender = defender
    , round = 1
    , attackerCasualties = Troops.emptyTroops
    , defenderCasualties = Troops.emptyTroops
    , settlement = Nothing
    , siege = False
    , finished = False
    }


skipBattle : Map.Model.Terrain -> BattleStats -> BattleStats
skipBattle ter bS =
    let
        newBattleStats =
            evaluateBattleResult bS ter
    in
    if newBattleStats.finished then
        newBattleStats

    else
        skipBattle ter newBattleStats


getGoldBonus : Entities.Model.Lord -> Entities.Model.Lord
getGoldBonus lord =
    { lord | gold = lord.gold + Balancing.addGoldCastle }


updatedBattleLordFunds : Entities.Model.Lord -> Entities.Model.Lord -> ( Entities.Model.Lord, Entities.Model.Lord )
updatedBattleLordFunds fl sl =
    let
        flDiff =
            transferLordFunds fl sl

        slDiff =
            transferLordFunds sl fl
    in
    ( { fl | gold = fl.gold + flDiff }, { sl | gold = sl.gold + slDiff } )


transferLordFunds : Entities.Model.Lord -> Entities.Model.Lord -> Float
transferLordFunds fl sl =
    OperatorExt.ternary (Troops.sumTroops sl.entity.army == 0) (sl.gold / 2) 0 - OperatorExt.ternary (Troops.sumTroops fl.entity.army == 0) (fl.gold / 2) 0



-- helper functions for the construction of the battle result
-------------------------------------------------------------------------------------


siegeBattleSetDefender : BattleStats -> Entities.Model.Settlement -> ( Entities.Model.Lord, Entities.Model.Settlement )
siegeBattleSetDefender bS settle =
    if Entities.isLordOnSettlement bS.defender settle then
        transferTroops bS.defender settle

    else
        ( bS.defender, settle )


checkDefenderArmy : Entities.Model.Lord -> Maybe Entities.Model.Settlement -> Bool
checkDefenderArmy defender settle =
    case settle of
        Nothing ->
            Troops.sumTroops defender.entity.army == 0

        Just s ->
            Troops.sumTroops s.entity.army == 0


handleSettlementTransfer : Entities.Model.Lord -> Entities.Model.Lord -> (Entities.Model.Settlement -> Bool) -> List Entities.Model.Settlement -> ( Entities.Model.Lord, Entities.Model.Lord )
handleSettlementTransfer attacker defender aFunc ndl =
    ( { attacker
        | land =
            List.map (\x -> { x | entity = Entities.updateEntityFaction attacker.entity.faction x.entity })
                (List.filter aFunc defender.land)
                ++ attacker.land
      }
    , { defender | land = ndl }
    )


updateSettlementBattleField : Entities.Model.Settlement -> List Entities.Model.Settlement -> List Entities.Model.Settlement
updateSettlementBattleField s l =
    case l of
        [] ->
            []

        x :: xs ->
            if x.entity.name == s.entity.name then
                s :: updateSettlementBattleField s xs

            else
                x :: updateSettlementBattleField s xs


{-| Transfers the troops of a lord to the besieged settlement (only if the player stands on this settlement)

    @param {Lord}: Takes the defender lord
    @param {Maybe Settlement}: Takes the sieged settlement
    @param {( Entities.Lord, Entities.Settlement )}: Returns the entities with the transfered armies

-}
transferTroops : Entities.Model.Lord -> Entities.Model.Settlement -> ( Entities.Model.Lord, Entities.Model.Settlement )
transferTroops l s =
    let
        newArmy =
            Troops.mergeTroops l.entity.army s.entity.army

        newLord =
            { l | entity = Entities.updateEntitiesArmy Dict.empty l.entity }

        newSettlement =
            { s | entity = Entities.updateEntitiesArmy newArmy s.entity }
    in
    ( newLord, newSettlement )



-- battle evaluation and helper functions
-------------------------------------------------------------------------------------


evaluateBattle : Entities.Model.WorldEntity -> Troops.Army -> Map.Model.Terrain -> Float -> Float -> Entities.Model.WorldEntity
evaluateBattle w army ter dF aF =
    evaluateLordCasualities w (sumTroopsDamage army ter aF) dF


calculateEntityCasualties : Troops.Army -> Troops.Army -> Troops.Army
calculateEntityCasualties armyBefore armyAfter =
    Dict.merge
        (\k v r -> Dict.insert k -v r)
        (\k v1 v2 r -> Dict.insert k (v2 - v1) r)
        (\k v2 r -> Dict.insert k 0 r)
        armyBefore
        armyAfter
        Dict.empty


evaluateLordCasualities : Entities.Model.WorldEntity -> Float -> Float -> Entities.Model.WorldEntity
evaluateLordCasualities w d dF =
    { w | army = calcTroopCasualties w.army d dF (Troops.sumTroops w.army) }


calcTroopCasualties : Troops.Army -> Float -> Float -> Int -> Troops.Army
calcTroopCasualties army d dF a =
    Dict.map (\k v -> calcCasualties (Troops.intToTroopType k) v ((d + 100.0) * (toFloat v / toFloat a)) dF) army


calcCasualties : Troops.TroopType -> Int -> Float -> Float -> Int
calcCasualties t amount d dF =
    max 0 (amount - round (d / (Troops.troopDefense t * dF)))


sumTroopsDamage : Troops.Army -> Map.Model.Terrain -> Float -> Float
sumTroopsDamage army ter aF =
    let
        bonusTroopTypes =
            Map.terrainToBonus ter
    in
    Dict.foldl
        (\k v dmg ->
            dmg
                + aF
                * OperatorExt.ternary
                    (List.member (Troops.intToTroopType k) bonusTroopTypes)
                    (Troops.battlefieldBonus (Troops.intToTroopType k))
                    1
                * Troops.troopDamage (Troops.intToTroopType k)
                * toFloat v
        )
        0
        army
