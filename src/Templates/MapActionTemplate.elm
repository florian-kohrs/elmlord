module Templates.MapActionTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapDrawer exposing (..)
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
                (span [] [ Html.text "Map Actions" ] :: List.map generateMapActionButtons actions)


generateMapActionButtons : Types.MapTileMsg -> Html Msg
generateMapActionButtons svga =
    button [ onClick (MapTileAction svga) ] [ span [] [ Html.text (Types.mapTileMsgToToolTip svga) ] ]
