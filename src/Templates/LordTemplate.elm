module Templates.LordTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapDrawer
import OperatorExt exposing (..)
import Troops exposing (..)
import Types exposing (Msg(..), UiSettlementState(..))

generateLordTemplate : Lord -> Html Msg
generateLordTemplate l = 
        div [Html.Attributes.class "modal-background"] [
            div [Html.Attributes.class "lord-modal"] [
                div [Html.Attributes.class "settlement-modal-close-container"] [
                    div [onClick Types.CloseModal, Html.Attributes.class "settlement-modal-close-btn lord-modal-close-btn"] [
                        span [] [Html.text "X"]
                    ]
                ]
                , div [Html.Attributes.class "lord-title"] [
                        span [] [Html.text l.entity.name]
                    ]
                    , div [Html.Attributes.class "lord-image"] [
                        img [src  ("./assets/images/profiles/" ++ factionToImage l.entity.faction)] []
                    ]
                    , div [Html.Attributes.class "lord-stats"] [
                        div [Html.Attributes.class "lord-data"] [
                            img [src  "./assets/images/ducats_icon.png"] []
                            , span [] [Html.text ("Gold: " ++ String.fromFloat l.gold)]
                        ]
                        , div [Html.Attributes.class "lord-data"] [
                            img [src  "./assets/images/map/Castle.png"] []
                            , span [] [Html.text ("Settlements: " ++ String.fromInt (List.length l.land))]
                        ]
                        , div [Html.Attributes.class "lord-troops"] 
                        (div [Html.Attributes.class "lord-troop-header"] [span [] [Html.text "Current-Army"]] :: List.map troopToHtml l.entity.army)
                    ]
                    
            ]
        ]


troopToHtml : Troop -> Html Msg
troopToHtml troop =
        div [Html.Attributes.class "lord-troop-container"] [
            img [src  ("./assets/images/troops/" ++ String.toLower (Troops.troopName troop.troopType) ++ ".png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]
