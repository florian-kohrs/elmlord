module Main exposing (..)

import AI
import Battle
import Battle.Model
import Browser
import Building
import DateExt
import Dict
import Entities
import Entities.Drawer
import Entities.Model
import Event
import Faction
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (..)
import ListExt
import Map
import Map.Drawer
import Map.Model
import MapAction
import MapAction.Model
import MapAction.SubModel
import MapData
import MapGenerator
import Msg
import OperatorExt
import PathAgent
import Pathfinder
import Pathfinder.Drawer
import Pathfinder.Model
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Templates.BattleTemplate as BattleTemplate
import Templates.EndTemplate as EndTemplate
import Templates.EventTemplate as EventTemplate
import Templates.HeaderTemplate as HeaderTemplate
import Templates.LordTemplate as LordTemplate
import Templates.MapActionTemplate as MapActionTemplate
import Templates.SettlementTemplate as SettlementTemplate
import Troops
import Vector



-- Model and the states of the UI (show/hide specific layouts)
----------------------------------------------------------


type alias Model =
    { lords : Entities.LordList
    , gameState : GameState
    , selectedPoint : Maybe Vector.Point
    , date : DateExt.Date
    , map : Map.Model.Map
    , errorMsg : String
    , event : Event.EventState
    }


type GameState
    = GameSetup UiState
    | GameOver Bool -- true = gewonnen, false = verloren


type UiState
    = MainMenue
    | SaveLoad
    | NewCampain
    | GameMenue
    | BattleView Battle.Model.BattleStats
    | SettlementView Entities.Model.Lord Entities.Model.Settlement Msg.UiSettlementState
    | LordView Entities.Model.Lord


villagesPerLord : Int
villagesPerLord =
    3


villageCaptialDistance : Float
villageCaptialDistance =
    7


getPlayer : Model -> Entities.Model.Lord
getPlayer model =
    Entities.getPlayer model.lords



-- items and path drawing on the game map
----------------------------------------------------------


canMoveToPoint : MapAction.Model.MapClickAction -> Vector.Point -> Bool
canMoveToPoint dict p =
    MapAction.hasActionOnPoint p (MapAction.SubModel.MoveTo p) dict


buildAllMapSvgs : Model -> MapAction.Model.MapClickAction
buildAllMapSvgs m =
    filterMapSvgs
        (buildPathSvgs m
            (List.foldl
                (Entities.Drawer.drawSettlement (getPlayer m))
                (List.foldl (Entities.Drawer.drawLord (getPlayer m)) (drawnMap m.map) (Entities.flattenLordList m.lords))
                (allSettlements m)
            )
        )


filterMapSvgs : MapAction.Model.MapClickAction -> MapAction.Model.MapClickAction
filterMapSvgs =
    Dict.map (\_ v -> filterInteractables v)


filterInteractables : List MapAction.Model.InteractableSvg -> List MapAction.Model.InteractableSvg
filterInteractables =
    List.foldr
        (\svg r ->
            if MapAction.isSvgAllowedIn svg r then
                svg :: r

            else
                r
        )
        []


buildPathSvgs : Model -> MapAction.Model.MapClickAction -> MapAction.Model.MapClickAction
buildPathSvgs m mapDict =
    let
        player =
            Entities.getPlayer m.lords
    in
    case getSelectedPath m of
        Nothing ->
            mapDict

        Just path ->
            Pathfinder.Drawer.drawPath
                player.agent
                (Pathfinder.cutFirstStepFromPath path)
                mapDict


getSelectedPath : Model -> Maybe Pathfinder.Model.Path
getSelectedPath m =
    let
        player =
            Entities.getPlayer m.lords
    in
    case m.selectedPoint of
        Nothing ->
            Nothing

        Just point ->
            getPathTo player.entity.position point m.map


getPathTo : Vector.Point -> Vector.Point -> Map.Model.Map -> Maybe Pathfinder.Model.Path
getPathTo from to map =
    if canMoveToPoint (drawnMap map) to then
        Pathfinder.getPath
            from
            (Pathfinder.Model.PathInfo (Pathfinder.getNav map) to)

    else
        Nothing


allSettlements : Model -> List Entities.Model.Settlement
allSettlements m =
    List.concat (List.map .land (Entities.flattenLordList m.lords))


drawnMap : Map.Model.Map -> MapAction.Model.MapClickAction
drawnMap map =
    Map.Drawer.drawMap map



-- testLordData For Battlesimulation


testWorldEntity : Entities.Model.WorldEntity
testWorldEntity =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Malaca"
    }


testSetelement : Entities.Model.Settlement
testSetelement =
    { entity = testWorldEntity
    , settlementType = Entities.Model.Castle
    , recruitLimits = Troops.emptyTroops
    , income = 3.19
    , isSieged = False
    , buildings = Building.startBuildings
    }


testLordWorldEntity : Entities.Model.WorldEntity
testLordWorldEntity =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "RÜDIGER"
    }


testLordWorldEntity2 : Entities.Model.WorldEntity
testLordWorldEntity2 =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Peter von Haskell"
    }


testLord2 : Entities.Model.Lord
testLord2 =
    { entity = testLordWorldEntity2
    , gold = 450
    , land = [ testSetelement ]
    , agent = PathAgent.getAgent 6
    }


testLord : Entities.Model.Lord
testLord =
    { entity = testLordWorldEntity
    , gold = 10050
    , land = [ testSetelement ]
    , agent = PathAgent.getAgent 6
    }



-- init game data (set the model data)
-- add the player, lords, settlements, etc.
----------------------------------------------------------


startGame : Int -> Model
startGame playerCount =
    initPlayers initialModel playerCount


initialModel : Model
initialModel =
    let
        map =
            MapGenerator.createMap
    in
    Model (Entities.Cons testLord []) (GameSetup MainMenue) Nothing (DateExt.Date 1017 DateExt.Jan) map "" testEventState



--TODO: delete it after you ve added events


testEventState : Event.EventState
testEventState =
    { state = True, events = testEvents }


testEvents : List Event.Event
testEvents =
    [ { index = 0, header = "Hallo leude", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Important }
    , { index = 1, header = "Ich der zweite", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 2, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 3, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 4, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 5, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 6, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 7, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    , { index = 8, header = "WARUM?!??!", text = "lorem ipsum lorem ipsum lorem ipsum lorem ipsum", eventType = Event.Minor }
    ]


initPlayers : Model -> Int -> Model
initPlayers m count =
    let
        lords =
            List.map
                (\i -> initPlayer m.map i (2 * (toFloat i / toFloat count + 0.125)))
                (List.range 0 (count - 1))
    in
    { m | lords = Entities.Cons testLord lords }


initPlayer : Map.Model.Map -> Int -> Float -> Entities.Model.Lord
initPlayer m i rad =
    let
        entity =
            Entities.Model.WorldEntity
                Troops.startTroops
                (Faction.getFaction i)
                (Pathfinder.getClosestFreeFieldAt (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad)) (Pathfinder.getNav m) Dict.empty)
                ("Lord " ++ String.fromInt i)
    in
    Entities.Model.Lord
        entity
        250
        (initSettlementsFor m Dict.empty entity i)
        (PathAgent.getAgent 5)


initSettlementsFor : Map.Model.Map -> Dict.Dict Int () -> Entities.Model.WorldEntity -> Int -> List Entities.Model.Settlement
initSettlementsFor m usedFields e i =
    Entities.createCapitalFor e
        (Maybe.withDefault
            (e.name ++ "`s Capital`")
            (ListExt.getElementAt i Entities.Model.castleNames)
        )
        :: List.map
            Entities.getSettlementFor
            (getVillagesInQuadrant m e i villagesPerLord |> getSafeSettlementInfos m usedFields)


getVillagesInQuadrant : Map.Model.Map -> Entities.Model.WorldEntity -> Int -> Int -> List Entities.Model.SettlementInfo
getVillagesInQuadrant m e q i =
    List.map
        (\index ->
            Entities.Model.SettlementInfo
                Entities.Model.Village
                (getVillagesPosition i q index e.position)
                (Maybe.withDefault
                    (e.name ++ " " ++ String.fromInt i ++ "th Village`")
                    (ListExt.getElementAt (q * 4 + index) Entities.Model.villageNames)
                )
                e.faction
        )
        (List.range 1 i)


getVillagesPosition : Int -> Int -> Int -> Vector.Point -> Vector.Point
getVillagesPosition max q {- quadrant -} i p =
    let
        distanceFromCapital =
            villageCaptialDistance

        rad =
            0.5 * pi * (toFloat i / toFloat max + toFloat (-q + 2))

        x =
            sin rad * distanceFromCapital

        y =
            cos rad * distanceFromCapital

        -- should be divided by playerCount
    in
    Vector.addPoints p (Vector.toPoint (Vector.Vector x y))


getSafeSettlementInfos : Map.Model.Map -> Dict.Dict Int () -> List Entities.Model.SettlementInfo -> List Entities.Model.SettlementInfo
getSafeSettlementInfos m usedFields infos =
    Tuple.first
        (List.foldl
            (\info ( result, usedFields2 ) ->
                let
                    newInfo =
                        getSafeSettlementInfo info m usedFields2
                in
                ( newInfo :: result, Dict.insert (MapData.hashMapPoint newInfo.position) () usedFields2 )
            )
            ( [], usedFields )
            infos
        )


getSafeSettlementInfo : Entities.Model.SettlementInfo -> Map.Model.Map -> Dict.Dict Int () -> Entities.Model.SettlementInfo
getSafeSettlementInfo i m dict =
    Entities.editSettlmentInfoPosition (Pathfinder.getClosestFreeFieldAt i.position (Pathfinder.getNav m) dict) i



-- init the view (build the base layout of the page)
----------------------------------------------------------


view : Model -> Html Msg.Msg
view model =
    let
        allClickActions =
            buildAllMapSvgs model
    in
    div [ Html.Attributes.class "page-container" ]
        [ findModalWindow model
        , HeaderTemplate.generateHeaderTemplate (Entities.getPlayer model.lords) model.date
        , div [ Html.Attributes.class "page-map" ]
            (List.map addStylesheet stylessheets
                ++ [ MapActionTemplate.generateMapActionTemplate model.selectedPoint allClickActions
                   , div []
                        [ Svg.svg
                            [ Svg.Attributes.viewBox "0 0 850 1000"
                            , Svg.Attributes.fill "none"
                            ]
                            (MapAction.allSvgs allClickActions)
                        ]
                   , EventTemplate.generateEventOverview model.event
                   , span [] [ Html.text (Debug.toString (Entities.getPlayer model.lords).land) ]
                   ]
            )
        ]



-- load / show right modal-window by the current model menue state
----------------------------------------------------------


findModalWindow : Model -> Html Msg.Msg
findModalWindow model =
    case model.gameState of
        GameSetup uistate ->
            case uistate of
                SettlementView l s u ->
                    SettlementTemplate.generateSettlementModalTemplate l s u

                LordView l ->
                    LordTemplate.generateLordTemplate l

                BattleView bS ->
                    BattleTemplate.generateBattleTemplate bS (Map.getTerrainForPoint bS.attacker.entity.position model.map)

                _ ->
                    div [] []

        GameOver bool ->
            EndTemplate.generateEndTemplate bool


addStylesheet : String -> Html Msg.Msg
addStylesheet href =
    Html.node "link" [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" ("./assets/styles/" ++ href ++ ".css") ] []


stylessheets : List String
stylessheets =
    [ "main_styles", "battle_styles", "end_styles", "event_styles", "header_styles", "lord_styles", "mapaction_styles", "settlement_styles", "start_styles", "tooltip_styles" ]



-- update function
----------------------------------------------------------


update : Msg.Msg -> Model -> Model
update msg model =
    case msg of
        Msg.EndRound ->
            { model | date = DateExt.addMonths 1 model.date, lords = Entities.Cons (endRoundForLord (getPlayer model)) (updateAIsAfterPlayerRound (Entities.npcs model.lords)) }

        Msg.EndGame bool ->
            { model | gameState = GameOver bool }

        Msg.CloseModal ->
            { model | gameState = GameSetup GameMenue }

        Msg.BattleAction bmsg ->
            updateBattle bmsg model

        Msg.SettlementAction action ->
            updateSettlement action model

        Msg.EventAction emsg ->
            updateEvent emsg model

        Msg.MapTileAction action ->
            updateMaptileAction model action

        Msg.Click p ->
            { model | selectedPoint = Just p }


updateAIsAfterPlayerRound : List Entities.Model.Lord -> List Entities.Model.Lord
updateAIsAfterPlayerRound lords =
    List.map (\l -> updateAI l |> endRoundForLord) lords


updateAI : Entities.Model.Lord -> Entities.Model.Lord
updateAI lord =
    { lord | entity = Entities.setPosition lord.entity (Vector.addPoints (Vector.Point 1 1) lord.entity.position) }


endRoundForLord : Entities.Model.Lord -> Entities.Model.Lord
endRoundForLord l =
    Entities.applyLordGoldIncome l |> PathAgent.resetLordUsedMovement |> Entities.applyLordNewRecruits



-- update function for the map action messages
-- like move to a point or engage a battle
----------------------------------------------------------


updateMaptileAction : Model -> MapAction.SubModel.MapTileMsg -> Model
updateMaptileAction model ma =
    case ma of
        MapAction.SubModel.LordMsg msg lord ->
            updateLordAction msg lord model

        MapAction.SubModel.SettlementMsg msg settlement ->
            case msg of
                MapAction.SubModel.ViewSettlement ->
                    { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Msg.RestrictedView) }

                MapAction.SubModel.EnterSettlement ->
                    { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Msg.StandardView) }

                MapAction.SubModel.SiegeSettlement ->
                    let
                        defender =
                            Entities.findLordWithSettlement settlement (Entities.flattenLordList model.lords)
                    in
                    case defender of
                        Nothing ->
                            model

                        Just l ->
                            { model
                                | gameState =
                                    GameSetup
                                        (BattleView
                                            { attacker = getPlayer model
                                            , defender = l
                                            , round = 1
                                            , attackerCasualties = Troops.emptyTroops
                                            , defenderCasualties = Troops.emptyTroops
                                            , settlement = Just settlement
                                            , siege = True
                                            , finished = False
                                            }
                                        )
                            }

        MapAction.SubModel.MoveTo p ->
            let
                player =
                    getPlayer model
            in
            case getPathTo player.entity.position p model.map of
                Nothing ->
                    model

                Just path ->
                    let
                        ( usedMove, point ) =
                            PathAgent.moveAlongPath path player.entity.position player.agent

                        newPlayer =
                            { player
                                | agent = PathAgent.setUsedMovement usedMove player.agent
                                , entity = Entities.setPosition player.entity point
                            }
                    in
                    { model | lords = Entities.Cons newPlayer (Entities.npcs model.lords) }


updateLordAction : MapAction.SubModel.LordTileMsg -> Entities.Model.Lord -> Model -> Model
updateLordAction msg lord m =
    case msg of
        MapAction.SubModel.ViewLord ->
            { m | gameState = GameSetup (LordView lord) }

        MapAction.SubModel.EngageLord ->
            { m
                | gameState =
                    GameSetup
                        (BattleView
                            { attacker = getPlayer m
                            , defender = lord
                            , round = 1
                            , attackerCasualties = Troops.emptyTroops
                            , defenderCasualties = Troops.emptyTroops
                            , settlement = Nothing
                            , siege = False
                            , finished = False
                            }
                        )
            }



-- update function for the settlement messages
-- like view a settlement, recruit or station troops, etc.
----------------------------------------------------------


updateSettlement : Msg.SettlementMsg -> Model -> Model
updateSettlement msg model =
    case msg of
        Msg.UIMsg umsg ->
            updateSettlementUI umsg model

        Msg.TroopMsg tmsg ->
            updateSettlementStats tmsg model


updateSettlementUI : Msg.SettlementUIMsg -> Model -> Model
updateSettlementUI msg model =
    case msg of
        Msg.ShowBuyTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Msg.RecruitView) }

        Msg.ShowStationTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Msg.StationView) }

        Msg.ShowSettlement s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Msg.StandardView) }

        Msg.ShowBuildings s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Msg.BuildingView) }


updateSettlementStats : Msg.SettlementArmyMsg -> Model -> Model
updateSettlementStats msg model =
    case msg of
        Msg.BuyTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.buyTroops (Entities.getPlayer model.lords) t s))
                s
                Msg.RecruitView
                model

        Msg.StationTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.stationTroops (Entities.getPlayer model.lords) t s))
                s
                Msg.StationView
                model

        Msg.TakeTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.takeTroops (Entities.getPlayer model.lords) t s))
                s
                Msg.StationView
                model

        Msg.UpgradeBuilding b s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.upgradeBuilding (Entities.getPlayer model.lords) b s))
                s
                Msg.BuildingView
                model


updateMultipleTroopStats : Entities.LordList -> Entities.Model.Settlement -> Msg.UiSettlementState -> Model -> Model
updateMultipleTroopStats l s u m =
    let
        newSettle =
            Entities.getSettlementByName (Entities.getPlayer l).land s.entity.name
    in
    case newSettle of
        Nothing ->
            m

        Just x ->
            { m
                | lords = l
                , gameState = GameSetup (SettlementView (Entities.getPlayer l) x u)
            }



-- update function for the battle messages
-- like start a skirmish, flee a battle, etc.
----------------------------------------------------------


updateBattle : Msg.BattleMsg -> Model -> Model
updateBattle msg model =
    case msg of
        Msg.StartSkirmish bS ->
            let
                newBattleStats =
                    Battle.evaluateBattleResult bS (Map.getTerrainForPoint bS.attacker.entity.position model.map)
            in
            { model
                | gameState =
                    GameSetup
                        (BattleView
                            newBattleStats
                        )
            }

        Msg.SkipSkirmishes bS ->
            { model | gameState = GameSetup (BattleView (Battle.skipBattle bS (Map.getTerrainForPoint bS.attacker.entity.position model.map))) }

        Msg.FleeBattle bS ->
            updateLordsAfterBattle
                (Battle.fleeBattle bS)
                (List.map (\x -> OperatorExt.ternary (x.entity.name == bS.defender.entity.name) bS.defender x) (Entities.tailLordList model.lords))
                model
                (GameSetup GameMenue)

        Msg.EndBattle bS ->
            case bS.settlement of
                Nothing ->
                    updateLordsAfterBattle
                        bS.attacker
                        (List.map (\x -> OperatorExt.ternary (x.entity.name == bS.defender.entity.name) bS.defender x) (Entities.tailLordList model.lords))
                        model
                        (GameSetup GameMenue)

                Just settle ->
                    let
                        ( newAttacker, newDefender, lordKilled ) =
                            Battle.siegeBattleAftermath bS settle

                        newEnemyLords =
                            checkLordLost
                                lordKilled
                                newDefender.entity.name
                                (List.map (\x -> OperatorExt.ternary (x.entity.name == bS.defender.entity.name) bS.defender x) (Entities.tailLordList model.lords))
                    in
                    updateLordsAfterBattle
                        newAttacker
                        newEnemyLords
                        model
                        (OperatorExt.ternary (List.length (Entities.tailLordList model.lords) > 0) (GameSetup GameMenue) (GameOver True))


updateLordsAfterBattle : Entities.Model.Lord -> List Entities.Model.Lord -> Model -> GameState -> Model
updateLordsAfterBattle player enemyLords model state =
    { model | lords = Entities.Cons player enemyLords, gameState = state }


checkLordLost : Bool -> String -> List Entities.Model.Lord -> List Entities.Model.Lord
checkLordLost k n l =
    if k then
        List.filter (\x -> x.entity.name /= n) l

    else
        l



-- update function for the event log system
----------------------------------------------------------


updateEvent : Msg.EventMsg -> Model -> Model
updateEvent msg model =
    case msg of
        Msg.DeleteEvent index ->
            { model | event = Event.removeEvent model.event index }

        Msg.SwitchEventView ->
            { model | event = Event.updateEventState model.event }

        Msg.ClearEvents ->
            { model | event = Event.clearEvents model.event }


main : Program () Model Msg.Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }
