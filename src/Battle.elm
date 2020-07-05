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
    if Entities.isLordOnSettlement bS.defender settle then
        let
            ( transferedDefender, transferedSettle ) =
                transferTroops bS.defender settle

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

    else
        let
            tempAttacker =
                bS.attacker

            newAttacker =
                { tempAttacker | entity = evaluateBattle tempAttacker.entity settle.entity.army ter (Entities.getSettlementBonus settle bS.defender.land) }

            newSettle =
                { settle | entity = evaluateBattle settle.entity bS.attacker.entity.army ter 1 }

            casualties =
                calculateEntityCasualties ( bS.attacker.entity.army, newAttacker.entity.army ) (settle.entity.army, newSettle.entity.army)
        in
        constructBattleResult bS newAttacker bS.defender (Just newSettle) casualties


siegeBattleSetDefender : Entities.BattleStats -> Entities.Settlement -> (Entities.Lord, Entities.Settlement)
siegeBattleSetDefender bS settle = 
        if Entities.isLordOnSettlement bS.defender settle then
            transferTroops bS.defender settle            
        else     
            ( bS.defender, settle )

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


constructBattleResult : Entities.BattleStats -> Entities.Lord -> Entities.Lord -> Maybe Entities.Settlement -> ( List Troops.Troop, List Troops.Troop ) -> Entities.BattleStats
constructBattleResult bS attacker defender settle ( aCasu, dCasu ) =
    { bS
        | round = bS.round + 1
        , attackerCasualties = aCasu
        , defenderCasualties = dCasu
        , attacker = attacker
        , defender = defender
        , settlement = settle
        , finished = Troops.sumTroops attacker.entity.army == 0 || checkDefenderArmy defender settle
    }


calculateEntityCasualties : ( List Troops.Troop, List Troops.Troop ) -> ( List Troops.Troop, List Troops.Troop ) -> ( List Troops.Troop, List Troops.Troop )
calculateEntityCasualties ( ffWe, fsWe ) ( sfWe, ssWe ) =
    ( List.map2 Troops.troopDifferences ffWe fsWe, List.map2 Troops.troopDifferences sfWe ssWe )


checkDefenderArmy : Entities.Lord -> Maybe Entities.Settlement -> Bool
checkDefenderArmy defender settle =
    case settle of
        Nothing ->
            Troops.sumTroops defender.entity.army == 0

        Just s ->
            Troops.sumTroops s.entity.army == 0


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
            bS.attacker

        tempEnemy =
            bS.defender

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
handleSettlementTransfer player defender =
    ( { player
        | land =
            List.map (\x -> { x | entity = Entities.updateEntityFaction player.entity.faction x.entity })
                (List.filter (\y -> y.settlementType /= Entities.Castle) defender.land)
                ++ player.land
      }
    , { defender | land = [] }
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
