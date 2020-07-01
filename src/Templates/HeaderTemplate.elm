module Templates.HeaderTemplate exposing (..)

import DateExt
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Troops exposing (..)
import Types exposing (Msg(..))


generateHeaderTemplate : Lord -> DateExt.Date -> Html Msg
generateHeaderTemplate lord date =
    let
        value =
            revenueToSpan ( "", List.foldr (+) 0 (List.map Tuple.second (lordToRevenues lord)) )
    in
    div [ Html.Attributes.class "page-header" ]
        [ div [ Html.Attributes.class "page-turn-header" ] (headerTurnTemplate date)
        , div [ Html.Attributes.class "page-gold-header" ] (headerGoldTemplate lord value)
        , div [ Html.Attributes.class "page-troop-header" ] (headerTroopTemplate lord)
        , div [ Html.Attributes.class "page-settings-header" ] headerSettingsTemplate
        ]



-- HTML Parts
---------------------------------------------------------------------------------------------------------------------------------------------------


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


headerGoldTemplate : Lord -> Html Msg -> List (Html Msg)
headerGoldTemplate lord value =
    [ img [ onClick (EndGame True), src "./assets/images/ducats_icon.png", Html.Attributes.class "page-header-images" ] []
    , div [ Html.Attributes.class "tooltip" ]
        [ span [ Html.Attributes.class "page-header-span" ]
            [ Html.text (roundGold lord.gold ++ " Ducats")
            , value
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


generateTroopTooltip : Troop -> Troop -> Html Msg
generateTroopTooltip aT sT =
    div [ Html.Attributes.class "troop-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName aT.troopType) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt aT.amount ++ "  " ++ Troops.troopName aT.troopType) ]
        , span [] [ Html.text (String.fromInt sT.amount ++ "  " ++ Troops.troopName sT.troopType) ]
        ]

headerSettingsTemplate : List (Html Msg)
headerSettingsTemplate =
    [ div [ Html.Attributes.class "page-setting-container tooltip" ]
        [ img [ src "./assets/images/audio_on_icon.png", Html.Attributes.class "page-image-settings" ] []
        , div [ Html.Attributes.class "tooltip" ]
            [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Mute or unmute the gamesounds" ]
            ]
        ]
    , div [ Html.Attributes.class "page-settings-grid" ]
        [ div [ onClick (Types.BattleAction (Types.StartBattle "Lord 0")), Html.Attributes.class "page-setting-container tooltip" ]
            [ img [ src "./assets/images/save_icon.png", Html.Attributes.class "page-image-settings" ] []
            , div [ Html.Attributes.class "tooltip" ]
                [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Save the game as a file" ]
                ]
            ]
        ]
    ]

-- Logic
---------------------------------------------------------------------------------------------------------------------------------------------------


roundGold : Float -> String
roundGold v =
    let 
        parts = String.split "." (String.fromFloat v)
    in
        case parts of 
            [] -> 
                "0.00"
            
            (x :: []) -> 
                x ++ ".00"

            (x :: (xs :: _)) -> 
                x ++ "." ++ String.left 2 xs


lordToRevenues : Lord -> List (String, Float) 
lordToRevenues l =
        [("Settlements:", Entities.sumSettlementsIncome l.land), ("Armies:", Entities.sumTroopWages (Entities.sumLordTroops l) * -1)]

revenuesToTemplate : ( String, Float ) -> Html Msg
revenuesToTemplate rev =
    div [ Html.Attributes.class "revenue-container" ] [ revenueToSpan rev ]


revenueToSpan : ( String, Float ) -> Html Msg
revenueToSpan ( name, value ) =
    if value > 0 then
        span [ Html.Attributes.class "positive-income" ] [ Html.text (name ++ "  +" ++ roundGold value ++ " Ducats") ]

    else
        span [ Html.Attributes.class "negative-income" ] [ Html.text (name ++ " " ++ roundGold value ++ " Ducats") ]

