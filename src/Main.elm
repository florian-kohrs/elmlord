module Main exposing (..)

import Browser
import Dict
import Entities exposing (..)
import EntitiesDrawer
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map exposing (Map, MapTile)
import MapData exposing (..)
import MapDrawer
import MapGenerator exposing (createMap)
import MaybeExt
import PathDrawer
import Pathfinder
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Troops exposing (..)
import Types exposing (MapTileMsg(..), Msg(..))
import Vector exposing (..)


type alias Model =
    { lords : List Lord
    , gameState : GameState
    , selectedPoint : Maybe Point
    , map : Map.Map --used for pathfinding
    , mapTileClickActions : MapDrawer.MapClickAction
    }


type alias Revenue =
    { name : String
    , value : Float
    }


type GameState
    = GameSetup UiState
    | InGame Int -- int = playerCount
    | GameOver Bool -- true = gewonnen, false = verloren


type UiState
    = MainMenue
    | SaveLoad
    | NewCampain
    | SettlementView



--todo : Modell überarbeiten, map generierung anschauen -> pathfinding?
--lordToDrawInfo : Entities.Lord -> MapDrawer.MapDrawInfo Msg MapTileMsg
--lordToDrawInfo l =
-- STATIC TEST DATA


getPlayer : Model -> Maybe Entities.Lord
getPlayer m =
    List.head m.lords


buildAllMapSvgs : Model -> MapDrawer.MapClickAction
buildAllMapSvgs m =
    buildPathSvgs m
        (List.foldl
            EntitiesDrawer.drawSettlement
            (List.foldl EntitiesDrawer.drawLord m.mapTileClickActions m.lords)
            (allSettlements m)
        )


buildPathSvgs : Model -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
buildPathSvgs m mapDict =
    case getSelectedPath m of
        Nothing ->
            mapDict

        Just path ->
            case getPlayer m of
                Nothing ->
                    mapDict

                Just player ->
                    PathDrawer.drawPath player.moveSpeed path mapDict


getSelectedPath : Model -> Maybe Pathfinder.Path
getSelectedPath m =
    case getPlayer m of
        Nothing ->
            Nothing

        Just player ->
            case m.selectedPoint of
                Nothing ->
                    Nothing

                Just point ->
                    Pathfinder.getPath
                        player.entity.position
                        (Pathfinder.PathInfo (MapGenerator.getNav m.map) point)


allSettlements : Model -> List Settlement
allSettlements m =
    List.concat (List.map .land m.lords)


testRevenueList : List Revenue
testRevenueList =
    [ { name = "Castles", value = 2.5 }, { name = "Village", value = 1.9 }, { name = "Army", value = -3.3 } ]


testTroopList : List Troop
testTroopList =
    [ { amount = 50, troopType = Troops.Sword }, { amount = 30, troopType = Troops.Spear }, { amount = 30, troopType = Troops.Archer }, { amount = 11, troopType = Troops.Knight } ]


testWorldEntity : WorldEntity
testWorldEntity =
    { army = testTroopList
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Malaca"
    }


testSetelement : Settlement
testSetelement =
    { entity = testWorldEntity
    , settlementType = Entities.Village
    , income = 3.19
    , isSieged = False
    }



-- STATIC TEST DATA --


initialModel : Model
initialModel =
    let
        map =
            MapGenerator.createMap

        drawnMap =
            Map.drawMap map
    in
    Model [] (GameSetup MainMenue) Nothing map drawnMap


startGame : Int -> Model
startGame playerCount =
    createMapClickActions (initPlayers initialModel playerCount)


initPlayers : Model -> Int -> Model
initPlayers m count =
    let
        lords =
            List.map
                (\i -> initPlayer i (2 * (toFloat i / toFloat count + 0.125)))
                (List.range 0 (count - 1))
    in
    { m | lords = lords }


createMapClickActions : Model -> Model
createMapClickActions m =
    { m | mapTileClickActions = Map.drawMap m.map }


initPlayer : Int -> Float -> Lord
initPlayer i rad =
    let
        entity =
            WorldEntity
                []
                (Faction.getFaction i)
                (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad))
                ("Lord " ++ String.fromInt i)
    in
    Lord
        entity
        0
        (Entities.Action Entities.Wait Entities.Defend)
        (initSettlementsFor entity)
        5


initSettlementsFor : Entities.WorldEntity -> List Entities.Settlement
initSettlementsFor e =
    Entities.createCapitalFor e :: []



--generateSettlementsFor : Lord ->
{-
   addSettlementsTo : Entities.Lord -> List Entities.SettlementInfo -> Model -> Model
   addSettlementsTo l sInfos m =
       let
           mLord =
               List.head (List.filter ((==) l) m.lords)
       in
       case mLord of
           Nothing ->
               m

           Just lord ->
               List.map (Entities.getSettlementFor lord) sInfos


   initField : Map.MapTile -> Map.MapTile
   initField t =
       t

-}


update : Msg -> Model -> Model
update msg model =
    case msg of
        EndRound ->
            model

        Click p ->
            { model | selectedPoint = Just p }


view : Model -> Html Msg
view model =
    let
        allClickActions =
            buildAllMapSvgs model
    in
    div [ Html.Attributes.class "page-container" ]
        ([ generateHeaderTemplate model
         , div [ Html.Attributes.style "height" "800", Html.Attributes.style "width" "1000px" ]
            [ addStylesheet "link" "./assets/styles/main_styles.css"
            , Svg.svg
                [ Svg.Attributes.viewBox "0 0 2000 1800"
                , Svg.Attributes.width "2000"
                , Svg.Attributes.height "1800"
                , Svg.Attributes.fill "none"
                ]
                (MapDrawer.allSvgs allClickActions)
            ]
         ]
            ++ MaybeExt.foldMaybe
                (\path ->
                    [ Html.text
                        (List.foldl
                            (\t r -> String.fromFloat t.timeLoss ++ "," ++ r)
                            ""
                            path.path
                        )
                    , Html.text (Vector.showPoint path.target)
                    ]
                )
                [ Html.text (MaybeExt.foldMaybe (\p -> Vector.showPoint p) "no point selected" model.selectedPoint) ]
                (getSelectedPath model)
        )


pointToMsg : Vector.Point -> Msg
pointToMsg p =
    Click p


main : Program () Model Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }



-- SETTLEMENT-TEMPLATE (ist auszulagern)


generateSettlementModalTemplate : Settlement -> Html Msg
generateSettlementModalTemplate settlement =
    div [ Html.Attributes.class "modal-background" ]
        [ div [ Html.Attributes.class "settlement-modal" ]
            [ div [ Html.Attributes.class "settlement-modal-close-container" ]
                [ div [ Html.Attributes.class "settlement-modal-close-btn" ]
                    [ span [] [ Html.text "X" ]
                    ]
                ]
            , div [ Html.Attributes.class "settlement-modal-name" ]
                [ span [] [ Html.text (Entities.combineSettlementName settlement) ]
                ]
            , div [ Html.Attributes.class "settlement-lordship" ]
                [ div []
                    [ img [ src "./assets/images/profiles/profile_lord.png", Html.Attributes.class "settlement-lord-icon" ] []
                    ]
                , div []
                    [ span [ Html.Attributes.class "settlement-lord-text" ] [ Html.text "Sir Quicknuss" ]
                    ]
                ]
            , div [ Html.Attributes.class "settlement-action-container" ]
                (getSettlementActionsByType settlement.settlementType
                    ++ [ button [] [ span [] [ Html.text "Recruit troops" ] ]
                       , button [] [ span [] [ Html.text "Station troops" ] ]
                       , div [ Html.Attributes.class "settlement-info" ]
                            [ span [ Html.Attributes.class "header-span" ] [ Html.text "Settlement Info" ]
                            , span [ Html.Attributes.class "income-span" ] [ Html.text ("Income: +" ++ String.fromFloat settlement.income ++ " Ducats") ]
                            , div [ Html.Attributes.class "stationed-troops-overview" ]
                                (span [ Html.Attributes.class "troop-span" ] [ Html.text "Stationed Troops: " ] :: List.map troopToHtml settlement.entity.army)
                            ]
                       ]
                )
            , div [ Html.Attributes.class "settlement-illustration-container" ]
                [ img [ src "./assets/images/illustrations/example_ilustration.png" ] []
                ]
            ]
        ]


getSettlementActionsByType : SettlementType -> List (Html Msg)
getSettlementActionsByType settle =
    if settle == Entities.Castle then
        [ button [] [ span [] [ Html.text "Upgrade Buildings" ] ] ]

    else
        []



-- HEADER-TEMPLATE (ist auszulagern)


generateHeaderTemplate : Model -> Html Msg
generateHeaderTemplate model =
    div [ Html.Attributes.class "page-header" ]
        [ div [ Html.Attributes.class "page-turn-header" ]
            [ div [ Html.Attributes.class "page-turn-handler-header" ]
                [ div [ Html.Attributes.class "page-turn-button" ]
                    [ span [ onClick EndRound ] [ Html.text "End turn" ]
                    ]
                ]
            , div [ Html.Attributes.class "page-turn-date-header" ]
                [ span [ Html.Attributes.class "page-header-span" ] [ Html.text "January 1077 AD" ]
                ]
            ]
        , div [ Html.Attributes.class "page-gold-header" ]
            [ img [ src "./assets/images/ducats_icon.png", Html.Attributes.class "page-header-images" ] []
            , div [ Html.Attributes.class "tooltip" ]
                [ span [ Html.Attributes.class "page-header-span" ] [ Html.text "207 Ducats" ]
                , div [ Html.Attributes.class "tooltiptext gold-tooltip" ]
                    [ span [] [ Html.text "Monthly revenue" ]
                    , div [] (List.map revenuesToTemplate testRevenueList)
                    , div [ Html.Attributes.class "revenue-result-container" ]
                        [ revenueToString { name = "Revenue", value = List.foldr (+) 0 (List.map revenueToIncomeList testRevenueList) }
                        ]
                    ]
                ]
            ]
        , div [ Html.Attributes.class "page-troop-header" ]
            [ img [ src "./assets/images/troop_icon.png", Html.Attributes.class "page-header-images" ] []
            , div [ Html.Attributes.class "tooltip" ]
                [ span [ Html.Attributes.class "page-header-span" ] [ Html.text "121 Troops" ]
                , div [ Html.Attributes.class "tooltiptext troop-tooltip" ]
                    [ span [] [ Html.text "Current Troops" ]
                    , div [] (List.map troopToHtml testTroopList)
                    ]
                ]
            ]
        , div [ Html.Attributes.class "page-settings-header" ]
            [ div [ Html.Attributes.class "page-setting-container tooltip" ]
                [ img [ src "./assets/images/audio_on_icon.png", Html.Attributes.class "page-image-settings" ] []
                , div [ Html.Attributes.class "tooltip" ]
                    [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Mute or unmute the gamesounds" ]
                    ]
                ]
            , div [ Html.Attributes.class "page-settings-grid" ]
                [ div [ Html.Attributes.class "page-setting-container tooltip" ]
                    [ img [ src "./assets/images/save_icon.png", Html.Attributes.class "page-image-settings" ] []
                    , div [ Html.Attributes.class "tooltip" ]
                        [ span [ Html.Attributes.class "tooltiptext settings-tooltip" ] [ Html.text "Save the game as a file" ]
                        ]
                    ]
                ]
            ]
        ]



-- REVENUE WIRD AUSGELAGERT
------------------------------------------------------------------------------------------------------------------------------------


revenuesToTemplate : Revenue -> Html Msg
revenuesToTemplate rev =
    div [ Html.Attributes.class "revenue-container" ] [ revenueToString rev ]


revenueToString : Revenue -> Html Msg
revenueToString rev =
    if rev.value > 0 then
        span [ Html.Attributes.class "positive-income" ] [ Html.text (rev.name ++ ":  +" ++ String.fromFloat rev.value ++ " Ducats") ]

    else
        span [ Html.Attributes.class "negative-income" ] [ Html.text (rev.name ++ ": " ++ String.fromFloat rev.value ++ " Ducats") ]


revenueToIncomeList : Revenue -> Float
revenueToIncomeList rev =
    rev.value



-- Troop WIRD AUSGELAGERT (Sobald MSG ausgelagert ist)
------------------------------------------------------------------------------------------------------------------------------------


troopToHtml : Troop -> Html Msg
troopToHtml troop =
    div [ Html.Attributes.class "troop-container" ]
        [ img [ src ("./assets/images/" ++ String.toLower (Troops.troopName troop.troopType) ++ "_icon.png") ] []
        , span [] [ Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]



-- auslagern, konnte nicht gemacht werden, weil Msg in Templates benötigt wird xd


addStylesheet : String -> String -> Html Msg
addStylesheet tag href =
    Html.node tag [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" href ] []
