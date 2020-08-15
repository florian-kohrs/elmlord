module Templates.MapActionTemplate exposing (..)

import Dict
import DictExt
import Entities.Model
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapAction.Viewer
import Msg
import Templates.HelperTemplate as Helper
import Troops
import Vector
import OperatorExt
import Faction exposing (Faction)
import Entities.Model

{-| Returns the layout for the map actions, in dependence to the chosen point (in the model)

    @param {Maybe Point}: Takes the point that is currently chosen point (at the start no point is chosen, therefore Maybe)
    @param {MapDrawer.InteractableMapSVG}: Takes a dict with all possible actions

-}
generateMapActionTemplate : Maybe Vector.Point -> Entities.Model.Lord -> MapAction.Model.InteractableMapSVG -> Html Msg.Msg
generateMapActionTemplate p player dict =
    let
        actions =
            case p of
                Nothing ->
                    []

                Just x ->
                    MapAction.actionsOnPoint x dict
    in
    div []
        [ div [ Html.Attributes.class "map-action-menu" ]
            (span [] [ Html.text "Map Actions" ] :: List.map generateMapActionButtons actions)
        , generateTroopOverview player.entity.faction actions
        ]


{-| Function for map, that displays a button for each possible action

    @param {Msg.MapTileMsg}: Takes the action type, that the button sends, when it gets clicked

-}
generateMapActionButtons : MapAction.SubModel.MapTileMsg -> Html Msg.Msg
generateMapActionButtons svga =
    button [ onClick (Msg.MapTileAction svga) ] [ span [] [ Html.text (MapAction.Viewer.mapTileMsgToToolTip svga) ] ]


generateTroopOverview : Faction -> List MapAction.SubModel.MapTileMsg -> Html Msg.Msg
generateTroopOverview pf actions =
    div [ Html.Attributes.class "map-troop-overview" ]
        ((span [] [ Html.text "Enemy troops on map tile" ]
            :: DictExt.foldlOverKeys
                (\k v r -> Helper.troopToHtml (Troops.intToTroopType k) v "mini-lord-troop-container" :: r)
                (\k r -> Helper.troopToHtml (Troops.intToTroopType k) 0 "mini-lord-troop-container" :: r)
                []
                (combineTroopDicts Troops.emptyTroops actions (\f -> f /= pf))
                Troops.troopKeyList
        )
        ++ (span [] [ Html.text "Your troops on map tile" ]
                :: DictExt.foldlOverKeys
                    (\k v r -> Helper.troopToHtml (Troops.intToTroopType k) v "mini-lord-troop-container" :: r)
                    (\k r -> Helper.troopToHtml (Troops.intToTroopType k) 0 "mini-lord-troop-container" :: r)
                    []
                    (combineTroopDicts Troops.emptyTroops actions (\f -> f == pf)) 
                    Troops.troopKeyList
           ))


combineTroopDicts : Troops.Army -> List MapAction.SubModel.MapTileMsg -> (Faction -> Bool) -> Troops.Army
combineTroopDicts a l ff =
    case l of
        [] ->
            a

        x :: xs ->
            combineTroopDicts (Troops.mergeTroops a (getEntityTroopsByMapTile x ff)) xs ff


getEntityTroopsByMapTile : MapAction.SubModel.MapTileMsg -> (Faction -> Bool) -> Troops.Army
getEntityTroopsByMapTile svga ff =
    case svga of
        MapAction.SubModel.LordMsg _ lord ->
            OperatorExt.ternary (ff lord.entity.faction) lord.entity.army Troops.emptyTroops

        MapAction.SubModel.SettlementMsg _ settlement ->
            OperatorExt.ternary (ff settlement.entity.faction) settlement.entity.army Troops.emptyTroops

        _ ->
            Troops.emptyTroops
