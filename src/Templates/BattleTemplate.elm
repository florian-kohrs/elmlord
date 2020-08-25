module Templates.BattleTemplate exposing (generateBattleTemplate)

import Battle
import Battle.Model
import Dict
import DictExt
import Entities
import Entities.Model
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map
import Map.Model
import MaybeExt
import Msg
import OperatorExt
import Templates.HelperTemplate as Helper
import Troops


{-| Returns the layout for the battle modal (Engage [Lord] / Siege [Settlement])

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
generateBattleTemplate : Battle.Model.BattleStats -> Map.Model.Terrain -> Html Msg.Msg
generateBattleTemplate bS t =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "battle-modal" ]
            [ div [ Html.Attributes.class "battle-modal-main" ] (determineBattleMap bS t) ]
        ]


{-| Determines which kind of battle this is (player vs player or player vs settlement)
and with this information displays different layouts / elements

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
determineBattleMap : Battle.Model.BattleStats -> Map.Model.Terrain -> List (Html Msg.Msg)
determineBattleMap bS t =
    case bS.settlement of
        Nothing ->
            [ generateArmyOverview bS.attacker.entity (Entities.getPlayerImage bS.attacker) bS.attackerCasualties
            , generateActionOverview bS t
            , generateArmyOverview bS.defender.entity (Entities.getPlayerImage bS.defender) bS.defenderCasualties
            ]

        Just settle ->
            [ generateArmyOverview bS.attacker.entity (Entities.getPlayerImage bS.attacker) bS.attackerCasualties
            , generateActionOverview bS t
            , generateArmyOverview (Tuple.second (Battle.siegeBattleSetDefender bS settle)).entity (Entities.getSettlementImage settle) bS.defenderCasualties
            ]


{-| Displays the army (all troops) of an entity

    @param {WorldEntity}: Takes the entity to which the troops belong (lord or settlement)
    @param {String}: Takes the url for the image that have to be displayed
    @param {List Troop}: Takes the current casualities of this entity

-}
generateArmyOverview : Entities.Model.WorldEntity -> String -> Troops.Army -> Html Msg.Msg
generateArmyOverview we image casu =
    div [ Html.Attributes.class "battle-army-overview" ]
        [ img [ src image ] []
        , span [] [ Html.text we.name ]
        , div []
            (DictExt.mergeKeys
                (\k v1 r -> generateTroopOverview (Troops.intToTroopType k) v1 0 :: r)
                (\k v1 v2 r -> generateTroopOverview (Troops.intToTroopType k) v1 v2 :: r)
                (\k v2 r -> generateTroopOverview (Troops.intToTroopType k) 0 v2 :: r)
                (\k r -> generateTroopOverview (Troops.intToTroopType k) 0 0 :: r)
                we.army
                casu
                Troops.troopKeyList
                []
            )
        ]


{-| Displays the current troops and casualties of an entity

    @param {Troop}: Takes the current army troop of the entity
    @param {Troop}: Takes the current army troop casualties of the entity

-}
generateTroopOverview : Troops.TroopType -> Int -> Int -> Html Msg.Msg
generateTroopOverview troop amount casuAmount =
    div [ Html.Attributes.class "battle-troop-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName troop) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt amount ++ "  " ++ Troops.troopName troop) ]
        , span [ Html.Attributes.class "battle-troop-casualties" ]
            [ Html.text
                (OperatorExt.ternary (casuAmount < 0)
                    ("( " ++ String.fromInt casuAmount ++ "  " ++ Troops.troopName troop ++ ")")
                    " "
                )
            ]
        ]


{-| Displays the terrian premiums (bonuses) and possible player actions

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on

-}
generateActionOverview : Battle.Model.BattleStats -> Map.Model.Terrain -> Html Msg.Msg
generateActionOverview bS ter =
    div [ Html.Attributes.class "battle-action-container" ]
        [ div [ Html.Attributes.class "battle-terrain-info" ]
            ([ span [] [ Html.text "Battlefield-Terrain" ]
             , div []
                [ img [ src ("./assets/images/map/" ++ String.toLower (Map.terrainToName ter) ++ "_icon_color.png") ] []
                , span [] [ Html.text (Map.terrainToName ter) ]
                ]
             ]
                ++ List.map generateTerrainBonuses (Map.terrainToBonus ter)
                ++ generateSettlementBonus bS ter
            )
        , span [ Html.Attributes.class "battle-versus-text" ] [ Html.text "VS." ]
        , generateStatusText bS
        , div [] (generateActionButtonsByState bS)
        ]


{-| Displays the terrain bonus for a troop type (archer, swordsman, etc.)

    @param {TroopType}: Takes the troop type for which the bonus has to be determined

-}
generateTerrainBonuses : Troops.TroopType -> Html Msg.Msg
generateTerrainBonuses t =
    div [ Html.Attributes.class "battle-terrain-bonus" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text ("+" ++ Helper.roundDigits (toFloat (round (Troops.battlefieldBonus t * 100 - 100))) 0 ++ "%") ]
        ]


{-| Displays the terrain bonus for the settlement

    @param {BattleStats}: Takes information about the battle to get the settlement

-}
generateSettlementBonus : Battle.Model.BattleStats -> Map.Model.Terrain -> List (Html Msg.Msg)
generateSettlementBonus bS ter =
    case bS.settlement of
        Nothing ->
            [ div [] [] ]

        Just settle ->
            OperatorExt.ternary (settle.settlementType == Entities.Model.Castle && ter /= Map.Model.Forest)
                (generateTerrainBonuses Troops.Archer)
                (div
                    []
                    []
                )
                :: [ div [ Html.Attributes.class "battle-terrain-bonus" ]
                        [ img [ src (Entities.getSettlementImage settle) ] []
                        , span [] [ Html.text ("+" ++ Helper.roundDigits (toFloat (round (Entities.getSettlementBonus settle bS.defender.land * 100 - 100))) 0 ++ "%") ]
                        ]
                   ]


{-| Displays the status text about the ongoing battle

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)

-}
generateStatusText : Battle.Model.BattleStats -> Html Msg.Msg
generateStatusText bS =
    if bS.finished then
        span [ Html.Attributes.class (OperatorExt.ternary (Troops.sumTroops bS.attacker.entity.army == 0) "negative-income battle-skirmish-text" "positive-income battle-skirmish-text") ]
            [ Html.text (OperatorExt.ternary (Troops.sumTroops bS.attacker.entity.army == 0) "My lord, we have lost, we will return to our castle!" "My lord, we were victorious, we destroyed them!")
            ]

    else
        span [ Html.Attributes.class "battle-skirmish-text" ] [ Html.text ("Skirmish-Round: " ++ String.fromInt bS.round) ]


{-| Displays the possible actions for the player (buttons)

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)

-}
generateActionButtonsByState : Battle.Model.BattleStats -> List (Html Msg.Msg)
generateActionButtonsByState bS =
    if bS.finished then
        [ button [ onClick (Msg.BattleAction (Msg.EndBattle bS)) ] [ span [] [ Html.text "Leave battlefield" ] ]
        ]

    else
        [ button [ onClick (Msg.BattleAction (Msg.StartSkirmish bS)) ] [ span [] [ Html.text "Start skirmish" ] ]
        , button [ onClick (Msg.BattleAction (Msg.SkipSkirmishes bS)) ] [ span [] [ Html.text "Skip skirmishes" ] ]
        , button [ onClick (Msg.BattleAction (Msg.FleeBattle bS)) ]
            [ span [] [ Html.text "Flee battle" ] ]
        ]
