module Templates.MapActionTemplate exposing (..)

import Dict
import DictExt
import Entities.Model
import Faction exposing (Faction)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapAction.Viewer
import Msg
import OperatorExt
import Templates.HelperTemplate as Helper
import Troops
import Vector


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
            [ span [] [ Html.text "Map Actions" ]
            , div [ Html.Attributes.class "map-action-menu-body" ]
                (List.map generateMapActionButtons actions)
            ]
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
    let
        playerTroops =
            combineTroopDicts Troops.emptyTroops actions (\f -> f == pf)

        enemyTroops =
            combineTroopDicts Troops.emptyTroops actions (\f -> f /= pf)
    in
    if Troops.sumTroops playerTroops /= 0 || Troops.sumTroops enemyTroops /= 0 then
        div [ Html.Attributes.class "map-troop-overview" ]
            ((span [ Html.Attributes.class "map-troop-overview-header" ] [ Html.text "Enemy troops on map tile" ]
                :: getTroopOverviewData enemyTroops
             )
                ++ (span [ Html.Attributes.class "map-troop-overview-header" ] [ Html.text "Your troops on map tile" ]
                        :: getTroopOverviewData playerTroops
                   )
            )

    else
        div [] []


getTroopOverviewData : Troops.Army -> List (Html Msg.Msg)
getTroopOverviewData t =
    DictExt.foldlOverKeys
        (\k v r -> Helper.troopToHtml (Troops.intToTroopType k) v "mini-lord-troop-container" :: r)
        (\k r -> Helper.troopToHtml (Troops.intToTroopType k) 0 "mini-lord-troop-container" :: r)
        []
        t
        Troops.troopKeyList


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
        MapAction.SubModel.LordMsg lmsg lord ->
            case lmsg of
                MapAction.SubModel.ViewLord ->
                    OperatorExt.ternary (ff lord.entity.faction) lord.entity.army Troops.emptyTroops

                _ ->
                    Troops.emptyTroops

        MapAction.SubModel.SettlementMsg smsg settlement ->
            case smsg of
                MapAction.SubModel.ViewSettlement ->
                    OperatorExt.ternary (ff settlement.entity.faction) settlement.entity.army Troops.emptyTroops

                _ ->
                    Troops.emptyTroops

        _ ->
            Troops.emptyTroops
