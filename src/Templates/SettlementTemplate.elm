module Templates.SettlementTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Troops exposing (..)

generateSettlementModalTemplate : Settlement -> Html Msg
generateSettlementModalTemplate settlement =
    div [Html.Attributes.class "modal-background"] [
        div [Html.Attributes.class "settlement-modal"] [
            div [Html.Attributes.class "settlement-modal-close-container"] [
                div [onClick Types.CloseModal, Html.Attributes.class "settlement-modal-close-btn"] [
                    span [] [Html.text "X"]
                ]
            ]
            , div [Html.Attributes.class "settlement-modal-name"] [
                span [] [Html.text (Entities.combineSettlementName settlement)]
            ]
            , div [Html.Attributes.class "settlement-lordship"] [
                div [] [
                    img [src  "./assets/images/profiles/profile_lord.png", Html.Attributes.class "settlement-lord-icon"] []
                ]
                , div [] [
                    span [Html.Attributes.class "settlement-lord-text"] [Html.text "Sir Quicknuss"]
                ]
            ]
            , div [Html.Attributes.class "settlement-action-container"]
            (getSettlementActionsByType settlement.settlementType ++
            [button [] [ span [] [Html.text "Recruit troops"]]
                , button [] [ span [] [Html.text "Station troops"]]
                , div [Html.Attributes.class "settlement-info"] [
                    span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                    , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                    , div [Html.Attributes.class "stationed-troops-overview"]
                        (span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "] :: List.map troopToHtml settlement.entity.army)
                ]
            ])
            , div [Html.Attributes.class "settlement-illustration-container"] [
                img [src  "./assets/images/illustrations/example_ilustration.png"] []
            ]
        ]
    ]


getSettlementActionsByType : SettlementType -> List (Html Msg)
getSettlementActionsByType settle =
    if settle == Entities.Castle then
        [button [] [ span [] [Html.text "Upgrade Buildings"]]]
    else
        []

troopToHtml : Troop -> Html Msg
troopToHtml troop =
        div [Html.Attributes.class "troop-container"] [
            img [src  ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]