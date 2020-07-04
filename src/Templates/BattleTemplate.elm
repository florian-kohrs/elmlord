module Templates.BattleTemplate exposing (..)

import Battle
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map exposing (Map, Terrain)
import OperatorExt
import Troops exposing (..)
import Types exposing (Msg(..))


generateBattleTemplate : BattleStats -> Terrain -> Html Msg
generateBattleTemplate bS t =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "battle-modal" ]
            [ div [ Html.Attributes.class "battle-modal-main" ] (determineBattleMap bS t)] 
        ]

determineBattleMap : BattleStats -> Terrain -> List (Html Msg)
determineBattleMap bS t =
        if bS.siege then
            case bS.settlement of
                Nothing ->
                        []
                
                Just settle -> 
                    [ generateArmyOverview bS.player.entity (Entities.getPlayerImage bS.player) bS.playerCasualties
                    , generateActionOverview bS t
                    , generateArmyOverview settle.entity (Entities.getSettlementImage settle) bS.enemyCasualties
                    ]

        else 
                [ generateArmyOverview bS.player.entity (Entities.getPlayerImage bS.player) bS.playerCasualties
                , generateActionOverview bS t
                , generateArmyOverview bS.enemy.entity (Entities.getPlayerImage bS.enemy) bS.enemyCasualties
                ]


generateArmyOverview : WorldEntity -> String -> List Troop -> Html Msg
generateArmyOverview we image troops =
    div [ Html.Attributes.class "battle-army-overview" ]
        [ img [ src image ] []
        , span [] [ Html.text we.name ]
        , div [] (List.map2 generateTroopOverview we.army troops)
        ]

generateTroopOverview : Troop -> Troop -> Html Msg
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


generateActionOverview : BattleStats -> Terrain -> Html Msg
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


generateTerrainBonuses : TroopType -> Html Msg
generateTerrainBonuses t =
    div [ Html.Attributes.class "battle-terrain-bonus" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text ("+" ++ String.fromFloat (Troops.battlefieldBonus t * 100 - 100) ++ "%") ]
        ]


generateSettlementBonus : BattleStats -> Html Msg
generateSettlementBonus bS =
    case bS.settlement of 
        Nothing -> 
            div [] []
        
        Just settle -> 
            div [ Html.Attributes.class "battle-terrain-bonus" ]
                [ img [ src (Entities.getSettlementImage settle) ] []
                , span [] [ Html.text (roundPercentage (String.fromFloat (Battle.getSettlementBonus settle bS.enemy.land * 100 - 100)) ++ "%") ]
                ]

generateStatusText : BattleStats -> Html Msg
generateStatusText bS =
    if bS.finished then
        span [ Html.Attributes.class (OperatorExt.ternary (Battle.sumTroops bS.player.entity.army == 0) "negative-income battle-skirmish-text" "positive-income battle-skirmish-text") ]
            [ Html.text (OperatorExt.ternary (Battle.sumTroops bS.player.entity.army == 0) "My lord, we have lost, we will return to our castle!" "My lord, we were victorious, we repeled them!")
            ]

    else
        span [ Html.Attributes.class "battle-skirmish-text" ] [ Html.text ("Skirmish-Round: " ++ String.fromInt bS.round) ]


generateActionButtonsByState : BattleStats -> List (Html Msg)
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


roundPercentage : String -> String
roundPercentage v =
    let 
        parts = String.split "." v
    in
        case parts of 
            [] -> 
                "0.00"
            
            (x :: []) -> 
                x ++ ".00"

            (x :: (xs :: _)) -> 
                x ++ "." ++ String.left 2 xs