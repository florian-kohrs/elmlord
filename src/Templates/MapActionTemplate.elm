module Templates.MapActionTemplate exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapAction.Viewer
import Msg
import Vector


{-| Returns the layout for the map actions, in dependence to the chosen point (in the model)

    @param {Maybe Point}: Takes the point that is currently chosen point (at the start no point is chosen, therefore Maybe)
    @param {MapDrawer.InteractableMapSVG}: Takes a dict with all possible actions

-}
generateMapActionTemplate : Maybe Vector.Point -> MapAction.Model.InteractableMapSVG -> Html Msg.Msg
generateMapActionTemplate p dict =
    let
        actions =
            case p of
                Nothing ->
                    []

                Just x ->
                    MapAction.actionsOnPoint x dict
    in
    div [ Html.Attributes.class "map-action-menu" ]
        (span [] [ Html.text "Map Actions" ] :: List.map generateMapActionButtons actions)


{-| Function for map, that displays a button for each possible action

    @param {Msg.MapTileMsg}: Takes the action type, that the button sends, when it gets clicked

-}
generateMapActionButtons : MapAction.SubModel.MapTileMsg -> Html Msg.Msg
generateMapActionButtons svga =
    button [ onClick (Msg.MapTileAction svga) ] [ span [] [ Html.text (MapAction.Viewer.mapTileMsgToToolTip svga) ] ]
