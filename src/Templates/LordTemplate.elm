module Templates.LordTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import OperatorExt exposing (..)
import Troops exposing (..)
import Types exposing (Msg(..), UiSettlementState(..))
import Templates.HelperTemplate as Helper

{-| Returns the layout for the lord modal (View [Lord-Name])

    @param {Lord}: Takes the chosen Lord 
-}

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
                        (div [Html.Attributes.class "lord-troop-header"] [span [] [Html.text "Current-Army"]] :: List.map Helper.troopToHtml l.entity.army)
                    ]
                    
            ]
        ]

