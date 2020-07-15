module Templates.MapActionTemplate exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapDrawer
import Types
import Vector

{-| Returns the layout for the map actions, in dependence to the chosen point (in the model)

    @param {Maybe Point}: Takes the point that is currently chosen point (at the start no point is chosen, therefore Maybe)
    @param {MapDrawer.MapClickAction}: Takes a dict with all possible actions
-}

generateMapActionTemplate : Maybe Vector.Point -> MapDrawer.MapClickAction -> Html Types.Msg
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


{-| Function for map, that displays a button for each possible action

    @param {Types.MapTileMsg}: Takes the action type, that the button sends, when it gets clicked
-}

generateMapActionButtons : Types.MapTileMsg -> Html Types.Msg
generateMapActionButtons svga =
    button [ onClick (Types.MapTileAction svga) ] [ span [] [ Html.text (Types.mapTileMsgToToolTip svga) ] ]
