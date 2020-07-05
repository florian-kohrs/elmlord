module Battle exposing (evaluateBattleResult, siegeBattleAftermath, fleeBattle, skipBattle)

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
    let
        ( transferedDefender, transferedSettle ) =
            siegeBattleSetDefender bS settle

        tempAttacker =
            bS.attacker

        newAttacker =
            { tempAttacker | entity = evaluateBattle tempAttacker.entity transferedSettle.entity.army ter (Entities.getSettlementBonus settle bS.defender.land) }

        newSettle =
            { transferedSettle | entity = evaluateBattle transferedSettle.entity bS.attacker.entity.army ter 1 }

        casualties =
            calculateEntityCasualties ( bS.attacker.entity.army, newAttacker.entity.army ) ( transferedSettle.entity.army, newSettle.entity.army )
    in
    constructBattleResult bS newAttacker transferedDefender (Just newSettle) casualties


{-| Resolves / Calculate a battle skirmish outcome for lord battles

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
evaluateLordBattle : Entities.BattleStats -> Map.Terrain -> Entities.BattleStats
evaluateLordBattle bS ter =
    let
        tempAttacker =
            bS.attacker

        tempDefender =
            bS.defender

        newAttacker =
            { tempAttacker | entity = evaluateBattle tempAttacker.entity bS.defender.entity.army ter 1 }

        newDefender =
            { tempDefender | entity = evaluateBattle tempDefender.entity bS.attacker.entity.army ter 1 }

        casualties =
            calculateEntityCasualties ( bS.attacker.entity.army, newAttacker.entity.army ) ( bS.defender.entity.army, newDefender.entity.army )
    in
    constructBattleResult bS newAttacker newDefender bS.settlement casualties


{-| Construct a generic battle result for both sieges and normal battles

    @param {BattleStats}: Takes information about the battle (lords, settlement, troops, names, etc.)
    @param {Lord}: Takes the attacker lord
    @param {Lord}: Takes the defender lord
    @param {Maybe Settlement}: Takes for sieges a settlement therefore only as a maybe data structure
    @param {(List Troop, List Troop)}: Takes the calculated casualties for both sides

-}
constructBattleResult : Entities.BattleStats -> Entities.Lord -> Entities.Lord -> Maybe Entities.Settlement -> ( List Troops.Troop, List Troops.Troop ) -> Entities.BattleStats
constructBattleResult bS attacker defender settle ( aCasu, dCasu ) =
    let
        ( aAttacker, aDefender, aSettle ) =
            resolveBattleAftermaths bS.siege attacker defender settle
    in
    { bS
        | round = bS.round + 1
        , attackerCasualties = aCasu
        , defenderCasualties = dCasu
        , attacker = aAttacker
        , defender = aDefender
        , settlement = aSettle
        , finished = Troops.sumTroops attacker.entity.army == 0 || checkDefenderArmy defender settle
    }



-- battle aftermath functions
-- like position resets, settlement transfers, etc.
-------------------------------------------------------------------------------------


resolveBattleAftermaths : Bool -> Entities.Lord -> Entities.Lord -> Maybe Entities.Settlement -> ( Entities.Lord, Entities.Lord, Maybe Entities.Settlement )
resolveBattleAftermaths siege attacker defender settlement =
    if siege then
        ( attacker, defender, settlement )

    else
        ( lordBattleAftermath attacker, lordBattleAftermath defender, settlement )


{-| Check if the player lost the normal battle, if thats the case his position gets a reset to his
capital position

    @param {Lord}: Takes the attacker-/defender lord

-}
lordBattleAftermath : Entities.Lord -> Entities.Lord
lordBattleAftermath lord =
    if Troops.sumTroops lord.entity.army == 0 then
        case Entities.getLordCapital lord.land of
            Nothing ->
                lord

            Just settle ->
                { lord | entity = Entities.setPosition lord.entity settle.entity.position }

    else
        lord


siegeBattleAftermath : Entities.BattleStats -> Entities.Settlement -> ( Entities.Lord, Entities.Lord, Bool )
siegeBattleAftermath bS s =
    let
        attacker =
            bS.attacker

        defender =
            bS.defender
    in
    if Troops.sumTroops s.entity.army <= 0 then
        if s.settlementType == Entities.Castle then
            handleSettlementTransfer attacker defender (\y -> y.settlementType /= Entities.Castle) []

        else
            handleSettlementTransfer attacker defender (\y -> y.entity.name == s.entity.name) (List.filter (\y -> y.entity.name /= s.entity.name) defender.land)

    else
        ( attacker, defender, False )


fleeBattle : Entities.BattleStats -> Entities.Lord
fleeBattle bS =
    Entities.updatePlayerArmy bS.attacker (List.map (\x -> { x | amount = round (toFloat x.amount * 0.6) }) bS.attacker.entity.army)


skipBattle : Entities.BattleStats -> Map.Terrain -> Entities.BattleStats
skipBattle bS ter =
    let
        newBattleStats =
            evaluateBattleResult bS ter
    in
    if newBattleStats.finished then
        newBattleStats

    else
        skipBattle newBattleStats ter


-- helper functions for the construction of the battle result
-------------------------------------------------------------------------------------


siegeBattleSetDefender : Entities.BattleStats -> Entities.Settlement -> ( Entities.Lord, Entities.Settlement )
siegeBattleSetDefender bS settle =
    if Entities.isLordOnSettlement bS.defender settle then
        transferTroops bS.defender settle

    else
        ( bS.defender, settle )


checkDefenderArmy : Entities.Lord -> Maybe Entities.Settlement -> Bool
checkDefenderArmy defender settle =
    case settle of
        Nothing ->
            Troops.sumTroops defender.entity.army == 0

        Just s ->
            Troops.sumTroops s.entity.army == 0


handleSettlementTransfer : Entities.Lord -> Entities.Lord -> (Entities.Settlement -> Bool) -> List Entities.Settlement -> ( Entities.Lord, Entities.Lord, Bool )
handleSettlementTransfer attacker defender aFunc ndl =
    ( { attacker
        | land =
            List.map (\x -> { x | entity = Entities.updateEntityFaction attacker.entity.faction x.entity })
                (List.filter aFunc defender.land)
                ++ attacker.land
      }
    , { defender | land = ndl }
    , List.length ndl == 0
    )


{-| Transfers the troops of a lord to the besieged settlement (only if the player stands on this settlement)

    @param {Lord}: Takes the defender lord
    @param {Maybe Settlement}: Takes the sieged settlement
    @param {( Entities.Lord, Entities.Settlement )}: Returns the entities with the transfered armies

-}
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



-- battle evaluation and helper functions
-------------------------------------------------------------------------------------


evaluateBattle : Entities.WorldEntity -> List Troops.Troop -> Map.Terrain -> Float -> Entities.WorldEntity
evaluateBattle w t ter siegeBonus =
    evaluateLordCasualities w (sumTroopsDamage t ter siegeBonus)


calculateEntityCasualties : ( List Troops.Troop, List Troops.Troop ) -> ( List Troops.Troop, List Troops.Troop ) -> ( List Troops.Troop, List Troops.Troop )
calculateEntityCasualties ( ffWe, fsWe ) ( sfWe, ssWe ) =
    ( List.map2 Troops.troopDifferences ffWe fsWe, List.map2 Troops.troopDifferences sfWe ssWe )


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


sumTroopsDamage : List Troops.Troop -> Map.Terrain -> Float -> Float
sumTroopsDamage t ter siegeBonus =
    let
        bonusTroopTypes =
            Map.terrainToBonus ter
    in
    List.foldr (\x y -> siegeBonus * OperatorExt.ternary (List.member x.troopType bonusTroopTypes) (Troops.battlefieldBonus x.troopType) 1 * Troops.troopDamage x.troopType * toFloat x.amount + y) 0 t
