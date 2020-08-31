module AI.AIOffsensiveActionHandler exposing (..)

import AI.AISettlementHandling
import AI.AITroopHandling
import AI.Model exposing (..)
import Entities
import Entities.Model
import ListExt
import MaybeExt
import Troops


getSettlementAttackActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getSettlementAttackActions ai enemies =
    ListExt.justList <|
        List.foldl
            (\s r -> evaluateSettlementSiegeAction ai s enemies :: r)
            []
            (List.concat <| List.map (\l -> l.land) enemies)


getAttackLordsActions :
    AI
    -> List Entities.Model.Lord
    -> List AiRoundActionPreference
getAttackLordsActions ai =
    List.foldl
        (\l actions ->
            let
                strengthFactor =
                    lordStrengthDiff ai.lord l

                preference =
                    min (2 + ai.strategy.battleMultiplier) <| ((strengthFactor + min 1 (l.gold / 2000)) * ai.strategy.battleMultiplier)
            in
            if strengthFactor >= 1 && not (Entities.isLordInOwnSettlement l) then
                AiRoundActionPreference (DoSomething (AttackLord l)) preference :: actions

            else
                actions
        )
        []


evaluateSettlementSiegeAction : AI -> Entities.Model.Settlement -> List Entities.Model.Lord -> Maybe AiRoundActionPreference
evaluateSettlementSiegeAction ai s ls =
    let
        siegeStrengthDiff =
            toFloat (Troops.sumArmyStats ai.lord.entity.army)
                / max
                    (300 * ai.strategy.defendMultiplier)
                    (toFloat (AI.AISettlementHandling.settlementDefenseStrength s (Entities.landlordOnSettlement s ls))
                        * MaybeExt.foldMaybe
                            (\l ->
                                Entities.getSettlementBonus s ai.lord.land
                            )
                            1
                            (Entities.findLordWithSettlement s ls)
                    )
    in
    if
        siegeStrengthDiff
            >= 1
    then
        Just
            (AiRoundActionPreference
                (DoSomething (SiegeSettlement s))
                (min (2 + ai.strategy.siegeMultiplier)
                    (ai.strategy.siegeMultiplier
                        + settlementSiegeBoni ai s
                        + max 0 (logBase 10 (siegeStrengthDiff * siegeStrengthDiff))
                    )
                    * settlementSiegeMultiplier ai s
                )
            )

    else
        Nothing


settlementSiegeBoni : AI -> Entities.Model.Settlement -> Float
settlementSiegeBoni ai s =
    case s.settlementType of
        Entities.Model.Castle ->
            1 + ai.strategy.siegeMultiplier * 2 - 2

        Entities.Model.Village ->
            max 0.9 ai.strategy.siegeMultiplier - 1


settlementSiegeMultiplier : AI -> Entities.Model.Settlement -> Float
settlementSiegeMultiplier ai s =
    case s.settlementType of
        Entities.Model.Castle ->
            1

        Entities.Model.Village ->
            clamp
                0.5
                1
                (toFloat (Troops.sumArmyStats ai.lord.entity.army) / toFloat (AI.AITroopHandling.estimatedNormalPlayerTroopStrength ai))


lordStrengthDiff : Entities.Model.Lord -> Entities.Model.Lord -> Float
lordStrengthDiff attacker defender =
    toFloat (Troops.sumArmyStats attacker.entity.army) / (max 300 <| toFloat <| Troops.sumArmyStats defender.entity.army)
