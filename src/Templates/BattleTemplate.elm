module Templates.BattleTemplate exposing (generateBattleTemplate)

import Battle
import Entities
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map
import OperatorExt
import Troops
import Types
import Templates.HelperTemplate as Helper


{-| Returns the layout for the battle modal (Engage [Lord] / Siege [Settlement])

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on
-}

generateBattleTemplate : Entities.BattleStats -> Map.Terrain -> Html Types.Msg
generateBattleTemplate bS t =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "battle-modal" ]
            [ div [ Html.Attributes.class "battle-modal-main" ] (determineBattleMap bS t)] 
        ]


{-| Determines which kind of battle this is (player vs player or player vs settlement)
    and with this information displays different layouts / elements

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on
-}

determineBattleMap : Entities.BattleStats -> Map.Terrain -> List (Html Types.Msg)
determineBattleMap bS t =
        if bS.siege then
            case bS.settlement of
                Nothing ->
                        []
                
                Just settle -> 
                    [ generateArmyOverview bS.attacker.entity (Entities.getPlayerImage bS.attacker) bS.attackerCasualties
                    , generateActionOverview bS t
                    , generateArmyOverview settle.entity (Entities.getSettlementImage settle) bS.defenderCasualties
                    ]

        else 
                [ generateArmyOverview bS.attacker.entity (Entities.getPlayerImage bS.attacker) bS.attackerCasualties
                , generateActionOverview bS t
                , generateArmyOverview bS.defender.entity (Entities.getPlayerImage bS.defender) bS.defenderCasualties
                ]


{-| Displays the army (all troops) of an entity

    @param {WorldEntity}: Takes the entity to which the troops belong (lord or settlement)
    @param {String}: Takes the url for the image that have to be displayed
    @param {List Troop}: Takes the current casualities of this entity
-}

generateArmyOverview : Entities.WorldEntity -> String -> List Troops.Troop -> Html Types.Msg
generateArmyOverview we image troops =
    div [ Html.Attributes.class "battle-army-overview" ]
        [ img [ src image ] []
        , span [] [ Html.text we.name ]
        , div [] (List.map2 generateTroopOverview we.army troops)
        ]


{-| Displays the current troops and casualties of an entity

    @param {Troop}: Takes the current army troop of the entity
    @param {Troop}: Takes the current army troop casualties of the entity
-}

generateTroopOverview : Troops.Troop -> Troops.Troop -> Html Types.Msg
generateTroopOverview troop casu =
    div [ Html.Attributes.class "battle-troop-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName troop.troopType) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        , span [ Html.Attributes.class "battle-troop-casualties" ]
            [ Html.text
                (OperatorExt.ternary (casu.amount < 0)
                    ("( " ++ String.fromInt casu.amount ++ "  " ++ Troops.troopName casu.troopType ++ ")")
                    " "
                )
            ]
        ]

{-| Displays the terrian premiums (bonuses) and possible player actions

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
    @param {Terrain}: Takes terrain on which the battle takes place on
-}

generateActionOverview : Entities.BattleStats -> Map.Terrain -> Html Types.Msg
generateActionOverview bS ter =
    div [ Html.Attributes.class "battle-action-container" ]
        [ div [ Html.Attributes.class "battle-terrain-info" ]
            ([ span [] [ Html.text "Battlefield-Terrain" ]
             , div []
                [ img [ src "./assets/images/map/tree_image_color.png" ] []
                , span [] [ Html.text (Map.terrainToName ter) ]
                ]
             ]
                ++ List.map generateTerrainBonuses (Map.terrainToBonus ter)
                ++ [generateSettlementBonus bS]
            )
        , span [ Html.Attributes.class "battle-versus-text" ] [ Html.text "VS." ]
        , generateStatusText bS
        , div [] (generateActionButtonsByState bS)
        ]



{-| Displays the terrain bonus for a troop type (archer, swordsman, etc.)

    @param {TroopType}: Takes the troop type for which the bonus has to be determined
-}

generateTerrainBonuses : Troops.TroopType -> Html Types.Msg
generateTerrainBonuses t =
    div [ Html.Attributes.class "battle-terrain-bonus" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text ("+" ++ Helper.roundDigits (Troops.battlefieldBonus t * 100 - 100) ++ "%") ]
        ]

{-| Displays the terrain bonus for the settlement

    @param {BattleStats}: Takes information about the battle to get the settlement
-}

generateSettlementBonus : Entities.BattleStats -> Html Types.Msg
generateSettlementBonus bS =
    case bS.settlement of 
        Nothing -> 
            div [] []
        
        Just settle -> 
            div [ Html.Attributes.class "battle-terrain-bonus" ]
                [ img [ src (Entities.getSettlementImage settle) ] []
                    , span [] [ Html.text ("+" ++ Helper.roundDigits (Entities.getSettlementBonus settle bS.defender.land * 100 - 100) ++ "%") ]
                ]


{-| Displays the status text about the ongoing battle

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
-}

generateStatusText : Entities.BattleStats -> Html Types.Msg
generateStatusText bS =
    if bS.finished then
        span [ Html.Attributes.class (OperatorExt.ternary (Troops.sumTroops bS.attacker.entity.army == 0) "negative-income battle-skirmish-text" "positive-income battle-skirmish-text") ]
            [ Html.text (OperatorExt.ternary (Troops.sumTroops bS.attacker.entity.army == 0) "My lord, we have lost, we will return to our castle!" "My lord, we were victorious, we repeled them!")
            ]

    else
        span [ Html.Attributes.class "battle-skirmish-text" ] [ Html.text ("Skirmish-Round: " ++ String.fromInt bS.round) ]


{-| Displays the possible actions for the player (buttons)

    @param {BattleStats}: Takes information about the battle (troops, names, states, etc.)
-}
generateActionButtonsByState : Entities.BattleStats -> List (Html Types.Msg)
generateActionButtonsByState bS =
    if bS.finished then
        [ button [ onClick (Types.BattleAction (Types.EndBattle bS)) ] [ span [] [ Html.text "Leave battlefield" ] ]
        ]

    else
        [ button [ onClick (Types.BattleAction (Types.StartSkirmish bS)) ] [ span [] [ Html.text "Start skirmish" ] ]
        , button [ onClick (Types.BattleAction (Types.SkipSkirmishes bS)) ] [ span [] [ Html.text "Skip skirmishes" ] ]
        , button [ onClick (Types.BattleAction (Types.FleeBattle bS))] 
                 [ span [] [ Html.text "Flee battle" ] ]
        ]
