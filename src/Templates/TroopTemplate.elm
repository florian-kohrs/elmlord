module Templates.TroopTemplate exposing (..)

import DictExt
import Entities.Model
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msg
import OperatorExt
import Troops


generateTroopTemplate : Entities.Model.Lord -> Html Msg.Msg
generateTroopTemplate l =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "troop-modal" ]
            [ div [ Html.Attributes.class "troop-modal-close-container" ]
                [ div [ Html.Attributes.class "troop-modal-close", onClick Msg.CloseModal ]
                    [ span [] [ Html.text "X" ] ]
                ]
            , div []
                (div [ Html.Attributes.class "troop-header" ]
                    [ span [] [ Html.text "Current Army" ] ]
                    :: DictExt.foldlOverKeys
                        (\k v r -> troopToDisbandHtml (Troops.intToTroopType k) v :: r)
                        (\k r -> troopToDisbandHtml (Troops.intToTroopType k) 0 :: r)
                        []
                        l.entity.army
                        Troops.troopKeyList
                )
            ]
        ]


troopToDisbandHtml : Troops.TroopType -> Int -> Html Msg.Msg
troopToDisbandHtml t v =
    div [ Html.Attributes.class "disband-troop-container" ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt v ++ "  " ++ Troops.troopName t) ]
        , button
            [ onClick (Msg.TroopAction (Msg.TroopArmyMsg t))
            , Html.Attributes.class (OperatorExt.ternary (v > 0) "disband-button tooltip" "disband-button troop-disabled-button")
            , disabled (v <= 0)
            ]
            [ span [] [ Html.text "-" ]
            , div [ Html.Attributes.class "tooltiptext troop-recruiting-tooltip" ]
                [ span [] [ Html.text "Disband 5 units" ] ]
            ]
        ]
