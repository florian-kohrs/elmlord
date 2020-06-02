module Templates.MapActionTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapDrawer
import OperatorExt exposing (..)
import Troops exposing (..)
import Types exposing (Msg(..), UiSettlementState(..))
import Vector exposing (..)


generateMapActionTemplate : Maybe Point -> MapDrawer.MapClickAction -> Html Msg
generateMapActionTemplate p dict =
    case p of
        Nothing ->
            div [] []

        Just x ->
            let
                actions =
                    MapDrawer.actionsOnPoint x dict
            in
            div [ Html.Attributes.class "map-action-menu" ]
                (staticActionButton x ++ displayActionButtons x)


staticActionButton : Point -> List (Html Msg)
staticActionButton p =
    [ span [] [ Html.text "Map Actions" ]
    , button [ onClick (MapTileAction (Types.MoveTo p)) ] [ span [] [ Html.text "Move To" ] ]
    ]


displayActionButtons : Point -> List (Html Msg)
displayActionButtons p =
    [ ternary (checkPointForLord p) (button [] [ span [] [ Html.text "Inspect Lord" ] ]) (div [] [])
    , ternary (checkPointForSettlement p) (button [] [ span [] [ Html.text "Inspect Settlement" ] ]) (div [] [])
    ]



-- get deleted when Point -> Maybe Setllement / Lord gets implemented


checkPointForLord : Point -> Bool
checkPointForLord p =
    True


checkPointForSettlement : Point -> Bool
checkPointForSettlement p =
    True
