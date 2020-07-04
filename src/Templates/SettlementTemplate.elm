module Templates.SettlementTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing ( Msg(..), UiSettlementState(..))
import Troops exposing (..)
import Templates.HelperTemplate exposing (..)
import OperatorExt exposing (..)
import Templates.HelperTemplate as Helper


{-| Returns the layout for the settlement modal (Enter/View [Settlement-Name])

    @param {Lord}: Takes the lord of the settlement 
    @param {Settlement}: Takes the chosen settlement 
    @param {UiSettlementState}: Takes the state of the modal windows (exp. View for Recruiting, Stationing, etc.)
-}

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

{-| Returns the specific layout in dependence to the state

    @param {Lord}: Takes the lord of the settlement 
    @param {Settlement}: Takes the chosen settlement 
    @param {UiSettlementState}: Takes the state of the modal windows (exp. View for Recruiting, Stationing, etc.)
-}
settlementStateToAction : Lord -> Settlement -> UiSettlementState -> List (Html Msg)
settlementStateToAction lord settlement uistate = 
    case uistate of 
        StandardView -> 
                [   button [onClick (SettlementAction (Types.UIMsg (Types.ShowBuyTroops settlement)))] [ span [] [Html.text "Recruit troops"]]
                    , button [onClick (SettlementAction (Types.UIMsg (Types.ShowStationTroops settlement)))] [ span [] [Html.text "Station troops"]]
                    ,div [Html.Attributes.class "settlement-info"] [
                        span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                        , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                        , div [Html.Attributes.class "stationed-troops-overview"] [
                            span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "]
                            , div [] (List.map Helper.troopToHtml settlement.entity.army)   
                        ]
                    ]
                ]
        
        RecruitView -> 
            [div [Html.Attributes.class "settlement-troop-recruiting"] 
                    (span [] [Html.text "Recruit troops"] ::
                    div [Html.Attributes.class "troop-recruiting-header"] [
                        div [Html.Attributes.class "troop-settlement-header"] [
                            img [src "./assets/images/troop_icon.png"][]
                        ]
                        , div [Html.Attributes.class "troop-army-header"] [
                            img [src (getSettlementImage settlement)][]
                        ]
                    ] ::
                    mapSettlement (List.map2 Tuple.pair lord.entity.army settlement.recruitLimits) settlement lord ++
                    [button [onClick (SettlementAction (Types.UIMsg (Types.ShowSettlement settlement)))] [ span [] [Html.text "Back"]]])]

        StationView -> 
            [div [Html.Attributes.class "settlement-troop-stationing"] 
                    (span [] [Html.text "Station troops"] ::
                    div [Html.Attributes.class "troop-recruiting-header"] [
                        div [Html.Attributes.class "troop-settlement-header"] [
                            img [src "./assets/images/troop_icon.png"][]
                        ]
                        , div [Html.Attributes.class "troop-army-header"] [
                            img [src (getSettlementImage settlement)][]
                        ]
                    ] ::
                    List.map2 generateStationTroopContainer lord.entity.army (List.map (\x -> (x, settlement)) settlement.entity.army) ++
                    [button [onClick (SettlementAction (Types.UIMsg (Types.ShowSettlement settlement)))] [ span [] [Html.text "Back"]]])]
                    
        RestrictedView ->
            (validateSettlement lord settlement ++
                [ div [Html.Attributes.class "settlement-info"] [
                    span [Html.Attributes.class "header-span"] [Html.text "Settlement Info"]
                    , span [Html.Attributes.class "income-span"] [Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats")]
                    , div [Html.Attributes.class "stationed-troops-overview"] [
                        span [Html.Attributes.class "troop-span"] [Html.text "Stationed Troops: "]
                        , div [] (List.map Helper.troopToHtml settlement.entity.army)
                    ]
                ]
            ])

        _ -> 
            []
 

{-| Returns the listview with the stationed troops, the player can take units out or station new troops to the settlement.
    The function is used for the List.map2 function. 

    @param {Troop}: Current troop/unit of the lord (specifically the amount!)
    @param {(Troop, Settlement)}: Tuple with the current troop/unit and the settlement of this unit
-}

generateStationTroopContainer : Troop ->  (Troop, Settlement) -> Html Msg
generateStationTroopContainer lT (sT, sE) = 
    div [Html.Attributes.class "troop-stationing-container"] [
            img [src ("./assets/images/troops/" ++ String.toLower (Troops.troopName lT.troopType) ++ ".png")] []
            , span [] [Html.text ("[" ++ String.fromInt lT.amount ++ "]")]
            , div [] [
                span [] [Html.text ("[" ++ String.fromInt sT.amount ++ "]")]
            ]
            , button [onClick (SettlementAction (Types.TroopMsg (Types.TakeTroops lT.troopType sE)))
                     , Html.Attributes.class (OperatorExt.ternary (validateStationTroops sT.amount) "troop-disabled-button" "")
                     , disabled (validateStationTroops sT.amount)] [ 
                img [Html.Attributes.class "troop-station-icon", src "./assets/images/arrow_up.png"] []
            ]
            , button [onClick (SettlementAction (Types.TroopMsg (Types.StationTroops lT.troopType sE))),
                     Html.Attributes.class (OperatorExt.ternary (validateStationTroops lT.amount) "troop-disabled-button" "tooltip")
                     , disabled (validateStationTroops lT.amount)] [ 
                img [Html.Attributes.class "troop-station-icon", src "./assets/images/arrow_down.png"] []
            ]
    ]

{-| Custom map function that displays the stationed troops listview of the settlement.

    @param {List (Troop, Troop)}: Current troop/unit of the lord (specifically the amount!)
    @param {Settlement}: Takes the chosen settlement 
    @param {Lord}: Takes the current lord 
-}
mapSettlement : List (Troop, Troop) -> Settlement -> Lord -> List (Html Msg)
mapSettlement li s l =
        case li of 
            [] -> 
                []

            (x :: xs) ->
                 generateRecruitTroopContainer x s l :: mapSettlement xs s l


{-| Displays the listcomponent of the stationed troops list

    @param {(Troop, Troop)}: Current troop/unit of the lord (specifically the amount!)
    @param {Settlement}: Takes the chosen settlement 
    @param {Lord}: Takes the current lord 
-}
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


{-| Validates whether the player can buy new troops or not (if the player has the gold for the troops and the settlement has enough recruits)

    @param {TroopType}: Takes the troopType that the player wants to buy
    @param {Settlement}: Takes the chosen settlement 
    @param {Lord}: Takes the current lord (to determine if the player has the gold)
-}
validateBuyTroops : TroopType -> Settlement -> Lord -> Bool 
validateBuyTroops t s l = 
        not ((l.gold - Troops.troopCost t > 0) 
            && (Maybe.withDefault {amount = 0, troopType = t} (List.head (List.filter (\x -> x.troopType == t) s.recruitLimits))
        ).amount > 0)

{-| Validates whether the player can station or take troops out of the settlement

    @param {Int}: Takes the current number of troops that are stationed or in the army
-}

validateStationTroops : Int -> Bool 
validateStationTroops amount = 
        not (amount > 0)


{-| Validates whether the settlement belongs to the player or to another lord, in dependence to this return a message

    @param {Lord}: Takes the current lord
    @param {Settlement}: Takes the current settlement
-}
validateSettlement : Lord -> Settlement -> List (Html Msg)
validateSettlement l s =
        [div [Html.Attributes.class "settlement-enemy-overview"] [
            span [] [Html.text (OperatorExt.ternary (l.entity.faction == s.entity.faction) "This is our settlement!" "This is an enemy settlement!")]
        ]]

