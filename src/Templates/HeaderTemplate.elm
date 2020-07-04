module Templates.HeaderTemplate exposing (..)

import DateExt
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Troops exposing (..)
import Types exposing (Msg(..))
import Templates.HelperTemplate as Helper


{-| Returns the layout for the header

    @param {Lord}: Takes the player lord
    @param {Date}: Takes the current (ingame)-date
-}

generateHeaderTemplate : Lord -> DateExt.Date -> Html Msg
generateHeaderTemplate lord date =
    div [ Html.Attributes.class "page-header" ]
        [ div [ Html.Attributes.class "page-turn-header" ] (headerTurnTemplate date)
        , div [ Html.Attributes.class "page-gold-header" ] (headerGoldTemplate lord)
        , div [ Html.Attributes.class "page-troop-header" ] (headerTroopTemplate lord)
        , div [ Html.Attributes.class "page-settings-header" ] headerSettingsTemplate
        ]




{-| Returns turn parts (end turn button and date display)

    @param {Date}: Takes the current (ingame)-date
-}
headerTurnTemplate : DateExt.Date -> List (Html Msg)
headerTurnTemplate date =
    [ div [ Html.Attributes.class "page-turn-handler-header" ]
        [ div [ Html.Attributes.class "page-turn-button", onClick EndRound ]
            [ span [ ] [ Html.text "End turn" ]]
        ]
    , div [ Html.Attributes.class "page-turn-date-header" ]
        [ span [ Html.Attributes.class "page-header-span" ] [ Html.text (DateExt.showDate date) ]
        ]
    ]


{-| Returns the player treasury (ducats overview) and the current expenses

    @param {Lord}: Takes the player lord
-}
headerGoldTemplate : Lord -> List (Html Msg)
headerGoldTemplate lord =
    [ img [ onClick (EndGame True), src "./assets/images/ducats_icon.png", Html.Attributes.class "page-header-images" ] []
    , div [ Html.Attributes.class "tooltip" ]
        [ span [ Html.Attributes.class "page-header-span" ]
            [ Html.text (Helper.roundDigits lord.gold ++ " Ducats")
            , revenueToSpan ( "", List.foldr (+) 0 (List.map Tuple.second (lordToRevenues lord)) )
            ]
        , div [ Html.Attributes.class "tooltiptext gold-tooltip" ]
            [ span [] [ Html.text "Monthly revenue" ]
            , div [] (List.map revenuesToTemplate (lordToRevenues lord))
            , div [ Html.Attributes.class "revenue-result-container" ]
                [ revenueToSpan ( "Revenue", List.foldr (+) 0 (List.map Tuple.second (lordToRevenues lord)) )
                ]
            ]
        ]
    ]

{-| Returns the army and all stationed troops of the player

    @param {Lord}: Takes the player lord
-}
headerTroopTemplate : Lord -> List (Html Msg)
headerTroopTemplate lord =
    [ img [ src "./assets/images/troop_icon.png", Html.Attributes.class "page-header-images" ] []
    , div [ Html.Attributes.class "tooltip" ]
        [ span [ Html.Attributes.class "page-header-span" ] [ Html.text (String.fromFloat (List.foldr (+) 0.0 (List.map (\x -> toFloat x.amount) lord.entity.army)) ++ " Troops") ]
        , div [ Html.Attributes.class "tooltiptext troop-tooltip" ]
            [ span [] [ Html.text "Current Troops" ]
            , div [ Html.Attributes.class "troop-container-header troop-container" ]
                [ div [] []
                , span [] [ Html.text "In the Army" ]
                , span [] [ Html.text "Stantioned" ]
                ]
            , div [] (List.map2 generateTroopTooltip lord.entity.army (Entities.flattenTroops (List.foldr (\x y -> x.entity.army ++ y) [] lord.land) troopTypeList))
            ]
        ]
    ]

{-| Returns the troopoverview inside the tooltip, that displays the army structure of the player

    @param {Troop}: Takes the player unit/troop
    @param {Troop}: Takes the stationed player unit/troop
-}
generateTroopTooltip : Troop -> Troop -> Html Msg
generateTroopTooltip aT sT =
    div [ Html.Attributes.class "troop-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName aT.troopType) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt aT.amount ++ "  " ++ Troops.troopName aT.troopType) ]
        , span [] [ Html.text (String.fromInt sT.amount ++ "  " ++ Troops.troopName sT.troopType) ]
        ]


{-| Returns possible settings (save game and set audio) insided the header

-}
headerSettingsTemplate : List (Html Msg)
headerSettingsTemplate =
    [ div [ Html.Attributes.class "page-setting-container tooltip" ]
        [ img [ src "./assets/images/audio_on_icon.png", Html.Attributes.class "page-image-settings" ] []
        , div [ Html.Attributes.class "tooltip" ]
            [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Mute or unmute the gamesounds" ]
            ]
        ]
    , div [ Html.Attributes.class "page-settings-grid" ]
        [ div [ Html.Attributes.class "page-setting-container tooltip" ]
            [ img [ src "./assets/images/save_icon.png", Html.Attributes.class "page-image-settings" ] []
            , div [ Html.Attributes.class "tooltip" ]
                [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Save the game as a file" ]
                ]
            ]
        ]
    ]



{-| Calculates and returns the revenues of a lord in form of tuple ([Revenue-Type], [Value])

    @param {Lord}: Takes the lord, whose revenue should be calculated
-}

lordToRevenues : Lord -> List (String, Float) 
lordToRevenues l =
        [("Settlements:", Entities.sumSettlementsIncome l.land), ("Armies:", Entities.sumTroopWages (Entities.sumLordTroops l) * -1)]

{-| Formats the revenue tuple to a template for the revenue tooltip

    @param {(String, Float)}: Revenue of a lord
-}
revenuesToTemplate : ( String, Float ) -> Html Msg
revenuesToTemplate rev =
    div [ Html.Attributes.class "revenue-container" ] [ revenueToSpan rev ]


{-| Returns (a part) revenue with different styling, depending if the revenue is positive or negative

    @param {(String, Float)}: Revenue of a lord
-}
revenueToSpan : ( String, Float ) -> Html Msg
revenueToSpan ( name, value ) =
    if value > 0 then
        span [ Html.Attributes.class "positive-income" ] [ Html.text (name ++ "  +" ++ Helper.roundDigits value ++ " Ducats") ]

    else
        span [ Html.Attributes.class "negative-income" ] [ Html.text (name ++ " " ++ Helper.roundDigits value ++ " Ducats") ]

