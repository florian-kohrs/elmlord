module Templates.LordTemplate exposing (..)

import Entities
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types
import Templates.HelperTemplate as Helper

{-| Returns the layout for the lord modal (View [Lord-Name])

    @param {Lord}: Takes the chosen Lord 
-}

generateLordTemplate : Entities.Lord -> Html Types.Msg
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
                        img [src  ("./assets/images/profiles/" ++ Entities.factionToImage l.entity.faction)] []
                    ]
                    , div [Html.Attributes.class "lord-stats"] [
                        div [Html.Attributes.class "lord-data"] [
                            img [src  "./assets/images/general/ducats_icon.png"] []
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

