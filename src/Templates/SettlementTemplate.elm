module Templates.SettlementTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing ( Msg(..), UiSettlementState(..))
import Troops exposing (..)
import OperatorExt exposing (..)


generateSettlementModalTemplate : Lord -> Settlement -> UiSettlementState -> Html Msg
generateSettlementModalTemplate lord settlement uistate =
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
                    img [src  ("./assets/images/profiles/" ++ factionToImage lord.entity.faction), Html.Attributes.class "settlement-lord-icon"] []
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
                [ button [onClick (SettlementAction (Types.UIMsg (Types.ShowBuyTroops settlement)))] 
                [ span [] [Html.text "Recruit troops"]], button [onClick (SettlementAction (Types.UIMsg (Types.ShowStationTroops settlement)))] [ span [] [Html.text "Station troops"]]
                    ,div [Html.Attributes.class "settlement-info"] [
                        span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                    , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                    , div [Html.Attributes.class "stationed-troops-overview"] [
                        span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "]
                        , div [] (List.map troopToHtml settlement.entity.army)
                        
                    ]
                ]
            ]
        
        RecruitView -> 
            [div [Html.Attributes.class "settlement-troop-recruiting"] 
                    (span [] [Html.text "Recruit troops"] ::
                    mapSettlement (List.map2 Tuple.pair lord.entity.army settlement.recruitLimits) settlement lord ++
                    [button [onClick (SettlementAction (Types.UIMsg (Types.ShowSettlement settlement)))] [ span [] [Html.text "Back"]]])]

        StationView -> 
            [div [Html.Attributes.class "settlement-troop-stationing"] 
                    (span [] [Html.text "Station troops"] ::
                    List.map2 generateStationTroopContainer lord.entity.army (List.map (\x -> (x, settlement)) settlement.entity.army) ++
                    [button [onClick (SettlementAction (Types.UIMsg (Types.ShowSettlement settlement)))] [ span [] [Html.text "Back"]]])]
                    
        RestrictedView ->
            (validateSettlement lord settlement ++
                [ div [Html.Attributes.class "settlement-info"] [
                    span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                    , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                    , div [Html.Attributes.class "stationed-troops-overview"] [
                        span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "]
                        , div [] (List.map troopToHtml settlement.entity.army)
                    ]
                ]
            ])

        _ -> 
            []
 


generateStationTroopContainer : Troop ->  (Troop, Settlement) -> Html Msg
generateStationTroopContainer lT (sT, sE) = 
    div [Html.Attributes.class "troop-stationing-container"] [
            img [src ("./assets/images/troops/" ++ String.toLower (Troops.troopName lT.troopType) ++ ".png")] []
            , span [] [Html.text ("[" ++ String.fromInt lT.amount ++ "]")]
            , div [] [
                span [] [Html.text ("[" ++ String.fromInt sT.amount ++ "]")]
            ]
            , button [onClick (SettlementAction (Types.TroopMsg (Types.TakeTroops lT.troopType sE)))] [ Html.text "O" ]
            , button [onClick (SettlementAction (Types.TroopMsg (Types.StationTroops lT.troopType sE)))] [ Html.text "I" ]
    ]


mapSettlement : List (Troop, Troop) -> Settlement -> Lord -> List (Html Msg)
mapSettlement li s l =
        case li of 
            [] -> 
                []

            (x :: xs) ->
                 generateRecruitTroopContainer x s l :: mapSettlement xs s l


generateRecruitTroopContainer : (Troop, Troop) -> Settlement -> Lord -> Html Msg
generateRecruitTroopContainer (aT,sT) s l = 
    div [Html.Attributes.class "troop-recruiting-container"] [
            img [src ("./assets/images/troops/" ++ String.toLower (Troops.troopName aT.troopType) ++ ".png")] []
            , span [] [Html.text ("[" ++ String.fromInt aT.amount ++ "]")]
            , span [] [Html.text ("[" ++ String.fromInt sT.amount ++ "]")]
            , div [] [
                span [] [Html.text (String.fromFloat (Troops.troopCost aT.troopType))]
                , img [src  "./assets/images/ducats_icon.png"] []
            ]
            , button [onClick (SettlementAction (Types.TroopMsg (Types.BuyTroops aT.troopType s))), 
                    Html.Attributes.class (OperatorExt.ternary (validateBuyTroops aT.troopType s l) "troop-disabled-button" "tooltip") , 
                    disabled (validateBuyTroops aT.troopType s l)]
                [span [] [Html.text "+"]
                , div [Html.Attributes.class "tooltiptext troop-recruiting-tooltip"] [
                        span [] [Html.text "Monthly wage"]
                        , span [Html.Attributes.class "negative-income"] [Html.text ("- " ++ String.fromFloat (Troops.troopWage aT.troopType) ++ " Ducats")]
                    ] 
                ]
    ]

validateBuyTroops : TroopType -> Settlement -> Lord -> Bool 
validateBuyTroops t s l = 
        not ((l.gold - Troops.troopCost t > 0) && (Maybe.withDefault {amount = 0, troopType = t} (List.head (List.filter (\x -> x.troopType == t) s.recruitLimits))).amount > 0)

troopToHtml : Troop -> Html Msg
troopToHtml troop =
        div [Html.Attributes.class "stationed-troop-container troop-container"] [
            img [src  ("./assets/images/troops/" ++ String.toLower (Troops.troopName troop.troopType) ++ ".png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]

validateSettlement : Lord -> Settlement -> List (Html Msg)
validateSettlement l s =
        [div [Html.Attributes.class "settlement-enemy-overview"] [
            span [] [Html.text (OperatorExt.ternary (l.entity.faction == s.entity.faction) "This is our settlement!" "This is an enemy settlement!")]
        ]]

