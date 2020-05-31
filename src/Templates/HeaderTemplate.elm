module Templates.HeaderTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Troops exposing (..)


type alias Revenue =
    { name : String
    , value : Float
    }


testRevenueList : List Revenue
testRevenueList =
    [ { name = "Castles", value = 2.5 }, { name = "Village", value = 1.9 }, { name = "Army", value = -3.3 } ]


testTroopList : List Troop
testTroopList = [{amount = 50, troopType = Troops.Sword}, {amount = 30, troopType = Troops.Spear}, {amount = 30, troopType = Troops.Archer}, {amount = 11, troopType = Troops.Knight}]

generateHeaderTemplate : Html Msg
generateHeaderTemplate =
    div [Html.Attributes.class "page-header"] [
        div [Html.Attributes.class "page-turn-header"] [
            div [Html.Attributes.class "page-turn-handler-header"] [
                div [Html.Attributes.class "page-turn-button"] [
                    span [ onClick EndRound ] [Html.text "End turn"]
                ]
            ]
            , div [Html.Attributes.class "page-turn-date-header"] [
                span [Html.Attributes.class "page-header-span"] [ Html.text "January 1077 AD" ]
            ]
        ]
        ,div [Html.Attributes.class "page-gold-header"] [
            img [src  "./assets/images/ducats_icon.png", Html.Attributes.class "page-header-images"] []
            , div [Html.Attributes.class "tooltip"] [
                span [Html.Attributes.class "page-header-span"] [ Html.text "207 Ducats" ]
                , div [Html.Attributes.class "tooltiptext gold-tooltip"] [
                    span [] [Html.text "Monthly revenue" ]
                    , div [] (List.map revenuesToTemplate testRevenueList)
                    , div [Html.Attributes.class "revenue-result-container"] [
                        revenueToString { name = "Revenue", value =  List.foldr (+) 0 (List.map revenueToIncomeList testRevenueList)}
                    ]
                ]
            ]
        ]
        , div [Html.Attributes.class "page-troop-header"] [
            img [src  "./assets/images/troop_icon.png", Html.Attributes.class "page-header-images"] []
            , div [Html.Attributes.class "tooltip"] [
                span [Html.Attributes.class "page-header-span"] [ Html.text "121 Troops" ]
                , div [Html.Attributes.class "tooltiptext troop-tooltip"] [
                    span [] [Html.text "Current Troops" ]
                    , div [] (List.map troopToHtml testTroopList)
                ]
            ]
        ]
        , div [Html.Attributes.class "page-settings-header"] [
            div [onClick ShowSettlement, Html.Attributes.class "page-setting-container tooltip"] [
                    img [src  "./assets/images/audio_on_icon.png", Html.Attributes.class "page-image-settings"] []
                    , div [Html.Attributes.class "tooltip"] [
                        span [Html.Attributes.class "tooltiptext settings-tooltip"] [ Html.text "Mute or unmute the gamesounds" ]
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
    ]

-- REVENUE WIRD AUSGELAGERT
------------------------------------------------------------------------------------------------------------------------------------

revenuesToTemplate : Revenue -> Html Msg
revenuesToTemplate rev =
            div [Html.Attributes.class "revenue-container"] [ revenueToString rev]

revenueToString : Revenue -> Html Msg
revenueToString rev =
    if rev.value > 0 then
        span [ Html.Attributes.class "positive-income" ] [ Html.text (rev.name ++ ":  +" ++ String.fromFloat rev.value ++ " Ducats") ]

    else
        span [ Html.Attributes.class "negative-income" ] [ Html.text (rev.name ++ ": " ++ String.fromFloat rev.value ++ " Ducats") ]


revenueToIncomeList : Revenue -> Float
revenueToIncomeList rev =
    rev.value


-- Troop WIRD AUSGELAGERT (Sobald MSG ausgelagert ist)
------------------------------------------------------------------------------------------------------------------------------------

troopToHtml : Troop -> Html Msg
troopToHtml troop =
        div [Html.Attributes.class "troop-container"] [
            img [src  ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]



-- auslagern, konnte nicht gemacht werden, weil Msg in Templates benötigt wird xd


addStylesheet : String -> String -> Html Msg
addStylesheet tag href =
    Html.node tag [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" href ] []