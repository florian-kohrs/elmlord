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
import MapDrawer exposing (..)


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
                (span [] [Html.text "Map Actions"] :: List.map generateMapActionButtons actions)


generateMapActionButtons : SvgAction -> Html Msg
generateMapActionButtons svga = 
                button [ onClick (MapTileAction svga.action) ] [ span [] [ Html.text (Types.mapTileMsgToToolTip svga.action) ] ]
