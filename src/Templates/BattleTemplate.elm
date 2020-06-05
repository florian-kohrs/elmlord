module Templates.BattleTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Troops exposing (..)
import Map exposing (Terrain)
import Battle
import OperatorExt


generateBattleTemplate : BattleStats -> Html Msg
generateBattleTemplate bS =
    div [Html.Attributes.class "modal-background"] [
        div [Html.Attributes.class "battle-modal"] [
            div [Html.Attributes.class "battle-modal-main"] [
                generateArmyOverview bS.player bS.playerCasualties
                , generateActionOverview bS Map.Forest
                , generateArmyOverview bS.enemy bS.enemyCasualties
            ]
        ]
    ]


generateArmyOverview : Lord -> List Troop -> Html Msg
generateArmyOverview lord troops =
        div [Html.Attributes.class "battle-army-overview"] [
            img [src  ("./assets/images/profiles/" ++ factionToImage lord.entity.faction)] []
            , span [] [Html.text lord.entity.name]
            , div [] (List.map2 generateTroopOverview lord.entity.army troops)
        ]

generateTroopOverview : Troop -> Troop -> Html Msg
generateTroopOverview troop casu = 
        div [Html.Attributes.class "battle-troop-container"] [
            img [src  ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] []
            , span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
            , span [Html.Attributes.class "battle-troop-casualties"] 
                [ Html.text 
                    (OperatorExt.ternary (casu.amount < 0)
                        ("( " ++ String.fromInt casu.amount ++ "  " ++ Troops.troopName casu.troopType ++ ")") 
                         " ")
                ]
        ]




generateActionOverview : BattleStats -> Terrain -> Html Msg
generateActionOverview bS ter = 
        div [Html.Attributes.class "battle-action-container"] [
            div [Html.Attributes.class "battle-terrain-info"] [
                span [] [Html.text "Battlefield-Terrain"]
                , div [] [
                    img [src  "./assets/images/map/tree_image_color.png"] []
                    , span [] [Html.text (Map.terrainToName ter)]
                ]
                , div [Html.Attributes.class "battle-terrain-bonus"] [
                    img [src  "./assets/images/troops/bow_icon_color.png"] []
                    , span [] [Html.text "+15%"]
                ] 
            ]
            , span [Html.Attributes.class "battle-versus-text"] [Html.text "VS."]
            , generateStatusText bS
            , div [] (generateActionButtonsByState bS)
        ]


generateStatusText : BattleStats -> Html Msg
generateStatusText bS = 
        if bS.finished then
            span [Html.Attributes.class (OperatorExt.ternary (Battle.sumTroops bS.player.entity.army == 0) "negative-income battle-skirmish-text" "positive-income battle-skirmish-text")] 
            [
                Html.text (OperatorExt.ternary (Battle.sumTroops bS.player.entity.army == 0) "My lord, we have lost!" "My lord, we were victorious!")
            ]
        else
            span [Html.Attributes.class "battle-skirmish-text"] [ Html.text ("Skirmish-Round: " ++ String.fromInt bS.round)]


generateActionButtonsByState : BattleStats -> List (Html Msg)
generateActionButtonsByState bS =
        if bS.finished then
            [
                button [onClick (Types.BattleAction (Types.EndBattle bS))] [span [] [Html.text  "Leave battlefield"]]
            ]
        else 
            [
                button [onClick (Types.BattleAction (Types.StartSkirmish bS))] [span [] [Html.text "Start skirmish"]]
                , button [onClick (Types.BattleAction (Types.SkipSkirmishes bS))] [span [] [Html.text "Skip skirmishes"]]
                , button [onClick (Types.BattleAction (Types.FleeBattle bS))] [span [] [Html.text  "Flee battle"]]
            ]

