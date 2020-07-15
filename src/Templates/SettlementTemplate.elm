module Templates.SettlementTemplate exposing (..)

import Building
import Dict
import Entities
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MaybeExt
import OperatorExt
import Templates.HelperTemplate as Helper
import Troops
import Types


{-| Returns the layout for the settlement modal (Enter/View [Settlement-Name])

    @param {Lord}: Takes the lord of the settlement
    @param {Settlement}: Takes the chosen settlement
    @param {UiSettlementState}: Takes the state of the modal windows (exp. View for Recruiting, Stationing, etc.)

-}
generateSettlementModalTemplate : Entities.Lord -> Entities.Settlement -> Types.UiSettlementState -> Html Types.Msg
generateSettlementModalTemplate lord settlement uistate =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "settlement-modal" ]
            [ div [ Html.Attributes.class "settlement-modal-close-container" ]
                [ div [ onClick Types.CloseModal, Html.Attributes.class "settlement-modal-close-btn" ]
                    [ span [] [ Html.text "X" ]
                    ]
                ]
            , div [ Html.Attributes.class "settlement-modal-name" ]
                [ span [] [ Html.text (Entities.combineSettlementName settlement) ]
                ]
            , div [ Html.Attributes.class "settlement-lordship box-shadow" ]
                [ div []
                    [ img [ src ("./assets/images/profiles/" ++ Entities.factionToImage lord.entity.faction), Html.Attributes.class "settlement-lord-icon" ] []
                    ]
                , div []
                    [ span [ Html.Attributes.class "settlement-lord-text" ] [ Html.text lord.entity.name ]
                    ]
                ]
            , div [ Html.Attributes.class "settlement-action-container" ]
                (settlementStateToAction lord settlement uistate)
            , div [ Html.Attributes.class "settlement-illustration-container box-shadow" ]
                [ img [ src "./assets/images/illustrations/example_ilustration.png" ] []
                ]
            ]
        ]


{-| Returns the specific layout in dependence to the state

    @param {Lord}: Takes the lord of the settlement
    @param {Settlement}: Takes the chosen settlement
    @param {UiSettlementState}: Takes the state of the modal windows (exp. View for Recruiting, Stationing, etc.)

-}
settlementStateToAction : Entities.Lord -> Entities.Settlement -> Types.UiSettlementState -> List (Html Types.Msg)
settlementStateToAction lord settlement uistate =
    case uistate of
        Types.StandardView ->
            [ button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowBuyTroops settlement))) ] [ span [] [ Html.text "Recruit troops" ] ]
            , button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowStationTroops settlement))) ] [ span [] [ Html.text "Station troops" ] ]
            , button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowBuildings settlement))) ] [ span [] [ Html.text "Upgrade buildings" ] ]
            , div [ Html.Attributes.class "settlement-info box-shadow" ]
                [ span [ Html.Attributes.class "header-span" ] [ Html.text "Settlement Info" ]
                , span [ Html.Attributes.class "income-span" ] [ Html.text ("Income: +" ++ Helper.roundDigits (settlement.income + Building.resolveBonusFromBuildings settlement.buildings Building.Marketplace) ++ " Ducats") ]
                , div [ Html.Attributes.class "stationed-troops-overview" ]
                    [ span [ Html.Attributes.class "troop-span" ] [ Html.text "Stationed Troops: " ]
                    , div [] (Dict.foldr (\k v r -> Helper.troopToHtml (Troops.intToTroopType k) v "stationed-troop-container troop-container" :: r) [] settlement.entity.army)
                    ]
                ]
            ]

        Types.RecruitView ->
            [ div [ Html.Attributes.class "settlement-troop-recruiting" ]
                (span [] [ Html.text "Recruit troops" ]
                    :: div [ Html.Attributes.class "troop-recruiting-header" ]
                        [ div [ Html.Attributes.class "troop-settlement-header" ]
                            [ img [ src "./assets/images/troops/troop_icon.png" ] []
                            ]
                        , div [ Html.Attributes.class "troop-army-header" ]
                            [ img [ src (Entities.getSettlementImage settlement) ] []
                            ]
                        ]
                    :: Dict.merge
                        (\k v1 r -> generateRecruitTroopContainer (Troops.intToTroopType k) v1 0 settlement lord :: r)
                        (\k v1 v2 r -> generateRecruitTroopContainer (Troops.intToTroopType k) v1 v2 settlement lord :: r)
                        (\k v2 r -> generateRecruitTroopContainer (Troops.intToTroopType k) 0 v2 settlement lord :: r)
                        lord.entity.army
                        settlement.recruitLimits
                        []
                    ++ [ button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowSettlement settlement))) ] [ span [] [ Html.text "Back" ] ] ]
                )
            ]

        Types.StationView ->
            [ div [ Html.Attributes.class "settlement-troop-stationing" ]
                (span [] [ Html.text "Station troops" ]
                    :: div [ Html.Attributes.class "troop-recruiting-header" ]
                        [ div [ Html.Attributes.class "troop-settlement-header" ]
                            [ img [ src "./assets/images/troops/troop_icon.png" ] []
                            ]
                        , div [ Html.Attributes.class "troop-army-header" ]
                            [ img [ src (Entities.getSettlementImage settlement) ] []
                            ]
                        ]
                    :: Dict.merge (\k v1 r -> generateStationTroopContainer (Troops.intToTroopType k) v1 0 settlement :: r) (\k v1 v2 r -> generateStationTroopContainer (Troops.intToTroopType k) v1 v2 settlement :: r) (\k v2 r -> generateStationTroopContainer (Troops.intToTroopType k) 0 v2 settlement :: r) lord.entity.army settlement.entity.army []
                    ++ [ button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowSettlement settlement))) ] [ span [] [ Html.text "Back" ] ] ]
                )
            ]

        Types.RestrictedView ->
            validateSettlement lord settlement
                ++ [ div [ Html.Attributes.class "settlement-info box-shadow" ]
                        [ span [ Html.Attributes.class "header-span" ] [ Html.text "Settlement Info" ]
                        , span [ Html.Attributes.class "income-span" ] [ Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats") ]
                        , div [ Html.Attributes.class "stationed-troops-overview" ]
                            [ span [ Html.Attributes.class "troop-span" ] [ Html.text "Stationed Troops: " ]
                            , div [] (Dict.foldr (\k v r -> Helper.troopToHtml (Troops.intToTroopType k) v "stationed-troop-container troop-container" :: r) [] settlement.entity.army)
                            ]
                        ]
                   ]

        Types.BuildingView ->
            [ div [ Html.Attributes.class "settlement-building-upgrading" ]
                [ span [] [ Html.text "Upgrade buildings" ]
                , div [] (List.map displayBuildingComponents (List.map (\x -> ( x, lord, settlement )) settlement.buildings))
                , div [] [ button [ onClick (Types.SettlementAction (Types.UIMsg (Types.ShowSettlement settlement))) ] [ span [] [ Html.text "Back" ] ] ]
                ]
            ]


{-| Returns the listview with the stationed troops, the player can take units out or station new troops to the settlement.
The function is used for the List.map2 function.

    @param {Troop}: Current troop/unit of the lord (specifically the amount!)
    @param {(Troop, Settlement)}: Tuple with the current troop/unit and the settlement of this unit

-}
generateStationTroopContainer : Troops.TroopType -> Int -> Int -> Entities.Settlement -> Html Types.Msg
generateStationTroopContainer lt ltAmount stAmount sE =
    div [ Html.Attributes.class "troop-stationing-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName lt) ++ ".png") ] []
        , span [] [ Html.text ("[" ++ String.fromInt ltAmount ++ "]") ]
        , div []
            [ span [] [ Html.text ("[" ++ String.fromInt stAmount ++ "]") ]
            ]
        , button
            [ onClick (Types.SettlementAction (Types.TroopMsg (Types.TakeTroops lt sE)))
            , Html.Attributes.class (OperatorExt.ternary (validateStationTroops stAmount) "troop-disabled-button" "")
            , disabled (validateStationTroops stAmount)
            ]
            [ img [ Html.Attributes.class "troop-station-icon", src "./assets/images/general/arrow_up.png" ] []
            ]
        , button
            [ onClick (Types.SettlementAction (Types.TroopMsg (Types.StationTroops lt sE)))
            , Html.Attributes.class (OperatorExt.ternary (validateStationTroops ltAmount) "troop-disabled-button" "tooltip")
            , disabled (validateStationTroops ltAmount)
            ]
            [ img [ Html.Attributes.class "troop-station-icon", src "./assets/images/general/arrow_down.png" ] []
            ]
        ]


{-| Displays the listcomponent of the stationed troops list

    @param {(Troop, Troop)}: Current troop/unit of the lord (specifically the amount!)
    @param {Settlement}: Takes the chosen settlement
    @param {Lord}: Takes the current lord

-}
generateRecruitTroopContainer : Troops.TroopType -> Int -> Int -> Entities.Settlement -> Entities.Lord -> Html Types.Msg
generateRecruitTroopContainer t aAmount sAmount s l =
    div [ Html.Attributes.class "troop-recruiting-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text ("[" ++ String.fromInt aAmount ++ "]") ]
        , span [] [ Html.text ("[" ++ String.fromInt sAmount ++ "]") ]
        , div []
            [ span [] [ Html.text (String.fromFloat (((100.0 - Building.resolveBonusFromBuildings s.buildings Building.Fortress) / 100) * Troops.troopCost t)) ]
            , img [ src "./assets/images/general/ducats_icon.png" ] []
            ]
        , button
            [ onClick (Types.SettlementAction (Types.TroopMsg (Types.BuyTroops t s)))
            , Html.Attributes.class (OperatorExt.ternary (validateBuyTroops t s l) "troop-disabled-button" "tooltip")
            , disabled (validateBuyTroops t s l)
            ]
            [ span [] [ Html.text "+" ]
            , div [ Html.Attributes.class "tooltiptext troop-recruiting-tooltip" ]
                [ span [] [ Html.text "Monthly wage" ]
                , span [ Html.Attributes.class "negative-income" ] [ Html.text ("- " ++ String.fromFloat (Troops.troopWage t) ++ " Ducats") ]
                ]
            ]
        ]


displayBuildingComponents : ( Building.Building, Entities.Lord, Entities.Settlement ) -> Html Types.Msg
displayBuildingComponents ( b, l, s ) =
    div [ Html.Attributes.class "settlement-building-component" ]
        [ div [] []
        , div [] [ span [] [ Html.text b.name ] ]
        , div [ Html.Attributes.class "tooltip" ]
            [ img [ Html.Attributes.class "info-icon", src "./assets/images/general/info.png" ] []
            , div [ Html.Attributes.class "tooltiptext building-level-tooltip" ]
                (span [] [ Html.text "Upgrade infos" ]
                    :: List.map displayBuildingBonus [ ( b, 0 ), ( b, 1 ), ( b, 2 ), ( b, 3 ) ]
                )
            ]
        , div []
            [ button
                [ onClick (Types.SettlementAction (Types.TroopMsg (Types.UpgradeBuilding b s)))
                , Html.Attributes.class (OperatorExt.ternary (validateBuildingUpgrade b l) "troop-disabled-button" "tooltip")
                , disabled (validateBuildingUpgrade b l)
                ]
                [ img [ Html.Attributes.class "troop-station-icon", src "./assets/images/general/arrow_up.png" ] []
                , div [ Html.Attributes.class "tooltiptext building-upgrade-tooltip" ]
                    [ span [] [ Html.text "Upgrade building" ]
                    , span [ Html.Attributes.class "positive-income" ] [ Html.text ("Upgrade " ++ b.name ++ " to level " ++ String.fromInt (b.level + 1)) ]
                    ]
                ]
            ]
        ]


displayBuildingBonus : ( Building.Building, Int ) -> Html Types.Msg
displayBuildingBonus ( b, i ) =
    div [ Html.Attributes.class "buildings-info-container" ]
        [ span [ Html.Attributes.class (OperatorExt.ternary (b.level >= i) "positive-income" "negative-income") ]
            [ Html.text ("Level " ++ String.fromInt i ++ ":")
            ]
        , span [ Html.Attributes.class (OperatorExt.ternary (b.level >= i) "positive-income" "negative-income") ]
            [ Html.text (Building.buildingToBonusInfo b.buildingType i)
            ]
        , span [ Html.Attributes.class (OperatorExt.ternary (b.level >= i) "positive-income" "negative-income") ]
            [ Html.text
                (OperatorExt.ternary (i >= 1)
                    ("Cost: " ++ String.fromFloat (Building.upgradeCostBase b.buildingType * Basics.toFloat i))
                    ""
                )
            ]
        ]


{-| Validates whether the player can buy new troops or not (if the player has the gold for the troops and the settlement has enough recruits)

    @param {TroopType}: Takes the troopType that the player wants to buy
    @param {Settlement}: Takes the chosen settlement
    @param {Lord}: Takes the current lord (to determine if the player has the gold)

-}
validateBuyTroops : Troops.TroopType -> Entities.Settlement -> Entities.Lord -> Bool
validateBuyTroops t s l =
    not
        ((l.gold - (Troops.troopCost t * (1 - Building.resolveBonusFromBuildings s.buildings Building.Fortress / 100)) >= 0)
            && MaybeExt.foldMaybe (\v -> v > 0) False (Dict.get (Troops.troopTypeToInt t) s.recruitLimits)
        )


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
validateSettlement : Entities.Lord -> Entities.Settlement -> List (Html Types.Msg)
validateSettlement l s =
    [ div [ Html.Attributes.class "settlement-enemy-overview" ]
        [ span [] [ Html.text (OperatorExt.ternary (l.entity.faction == s.entity.faction) "This is our settlement!" "This is an enemy settlement!") ]
        ]
    ]



--TODO: combine it with the other validate functions (troopcost)


validateBuildingUpgrade : Building.Building -> Entities.Lord -> Bool
validateBuildingUpgrade b l =
    not
        ((l.gold - (Building.upgradeCostBase b.buildingType * Basics.toFloat (b.level + 1)) > 0) && b.level <= 2)
