module Templates.BattleTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Troops exposing (..)
import Map exposing (Terrain)


generateBattleTemplate : Lord -> Lord -> Html Msg
generateBattleTemplate player enemy =
    div [Html.Attributes.class "modal-background"] [
        div [Html.Attributes.class "battle-modal"] [
            div [Html.Attributes.class "battle-modal-main"] [
                generateArmyOverview player
                , generateActionOverview Map.Forest
                , generateArmyOverview enemy
            ]
        ]
    ]


generateArmyOverview : Lord -> Html Msg
generateArmyOverview lord =
        div [Html.Attributes.class "battle-army-overview"] [
            img [src  "./assets/images/profiles/profile_lord.png"] []
            , span [] [Html.text lord.entity.name]
            , div [] (List.map generateTroopOverview lord.entity.army)
        ]

generateTroopOverview : Troop -> Html Msg
generateTroopOverview troop = 
        div [Html.Attributes.class "battle-troop-container"] [
            img [src  ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]


generateActionOverview : Terrain -> Html Msg
generateActionOverview ter = 
        div [Html.Attributes.class "battle-action-container"] [
            div [Html.Attributes.class "battle-terrain-info"] [
                span [] [Html.text "Battlefield-terrain"]
                , div [] [
                    span [] [Html.text "+1.5%"]
                    , span [] [Html.text (Map.terrainToName ter)]
                    , span [] [Html.text "-1.5%"]
                ] 
            ]
            , span [Html.Attributes.class "battle-versus-text"] [Html.text "VS."]
            , span [Html.Attributes.class "battle-skirmish-text"] [ Html.text "Skirmish-Round: 1"]
            , div [] [
                button [] [span [] [Html.text "Start skirmish"]]
                , button [onClick Types.CloseModal] [span [] [Html.text  "Flee battle"]]
            ]
        ]