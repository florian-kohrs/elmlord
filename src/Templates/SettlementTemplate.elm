module Templates.SettlementTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing ( Msg(..), UiSettlementState(..))
import Troops exposing (..)


testTroopList : List Troop
testTroopList = [{amount = 50, troopType = Troops.Sword}, {amount = 30, troopType = Troops.Spear}, {amount = 30, troopType = Troops.Archer}, {amount = 11, troopType = Troops.Knight}]

generateSettlementModalTemplate : Lord -> Settlement -> UiSettlementState -> Html Msg
generateSettlementModalTemplate lord settlement uistate=
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
                    span [Html.Attributes.class "settlement-lord-text"] [Html.text lord.entity.name]
                ]
            ]
            , div [Html.Attributes.class "settlement-action-container"]
            (settlementStateToAction lord settlement uistate)
            , div [Html.Attributes.class "settlement-illustration-container"] [
                img [src  "./assets/images/illustrations/example_ilustration.png"] []
            ]
        ]
    ]

settlementStateToAction : Lord -> Settlement -> UiSettlementState -> List (Html Msg)
settlementStateToAction lord settlement uistate = 
    case uistate of 
        StandardView -> 
            (getSettlementActionsByType settlement.settlementType ++
                [button [onClick Types.ShowTroopRecruiting] [ span [] [Html.text "Recruit troops"]]
                , button [onClick Types.ShowTroopStationing] [ span [] [Html.text "Station troops"]]
                , div [Html.Attributes.class "settlement-info"] [
                    span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                    , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                    , div [Html.Attributes.class "stationed-troops-overview"] [
                        span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "]
                        , div [] (List.map troopToHtml settlement.entity.army)
                        
                    ]
                ]
            ])
        
        RecruitView -> 
            [div [Html.Attributes.class "settlement-troop-recruiting"] 
                    (span [] [Html.text "Recruit troops"] ::
                    List.map generateRecruitTroopContainer lord.entity.army ++
                    [button [onClick Types.ShowSettlement] [ span [] [Html.text "Back"]]])]

        StationView -> 
            [div [Html.Attributes.class "settlement-troop-stationing"] 
                    (span [] [Html.text "Station troops"] ::
                    List.map2 generateStationTroopContainer lord.entity.army settlement.entity.army ++
                    [button [onClick Types.ShowSettlement] [ span [] [Html.text "Back"]]])]
        _ ->
            []


generateStationTroopContainer : Troop ->  Troop -> Html Msg
generateStationTroopContainer lT sT = 
    div [Html.Attributes.class "troop-stationing-container"] [
            img [src "./assets/images/knight_icon.png"] []
            , span [] [Html.text ("[" ++ String.fromInt lT.amount ++ "]")]
            , div [] [
                span [] [Html.text ("[" ++ String.fromInt sT.amount ++ "]")]
            ]
            , button [onClick (SettlementAction Types.TakeTroops lT.troopType)] [ Html.text "O" ]
            , button [onClick (SettlementAction Types.StationTroops lT.troopType)] [ Html.text "I" ]
    ]




generateRecruitTroopContainer : Troop -> Html Msg
generateRecruitTroopContainer troop = 
    div [Html.Attributes.class "troop-recruiting-container"] [
            img [src ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] []
            , span [] [Html.text ("[" ++ String.fromInt troop.amount ++ "]")]
            , div [] [
                span [] [Html.text (String.fromFloat (Troops.troopCost troop.troopType))]
                , img [src  "./assets/images/ducats_icon.png"] []
            ]
            , button [onClick (SettlementAction Types.BuyTroops troop.troopType)] [ Html.text "+" ]
    ]

getSettlementActionsByType : SettlementType -> List (Html Msg)
getSettlementActionsByType settle =
    if settle == Entities.Castle then
        [button [] [ span [] [Html.text "Upgrade Buildings"]]]
    else
        []

troopToHtml : Troop -> Html Msg
troopToHtml troop =
        div [Html.Attributes.class "stationed-troop-container troop-container"] [
            img [src  ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]