module Main exposing (..)

import AI
import Battle
import Browser
import DateExt
import Dict
import Entities
import Entities.Drawer
import Event
import Faction
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (..)
import ListExt
import Map
import MapData
import MapDrawer
import MapGenerator
import OperatorExt
import PathAgent
<<<<<<< Updated upstream
import PathDrawer
=======
import PathAgentExt
>>>>>>> Stashed changes
import Pathfinder
import Pathfinder.Drawer
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
import Types
import Vector
import Building



-- Model and the states of the UI (show/hide specific layouts)
----------------------------------------------------------


type alias Model =
    { lords : Entities.LordList
    , gameState : GameState
    , selectedPoint : Maybe Vector.Point
    , date : DateExt.Date
    , map : Map.Map
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
    | BattleView Entities.BattleStats
    | SettlementView Entities.Lord Entities.Settlement Types.UiSettlementState
    | LordView Entities.Lord


villagesPerLord : Int
villagesPerLord =
    3


villageCaptialDistance : Float
villageCaptialDistance =
    7


getPlayer : Model -> Entities.Lord
getPlayer model =
    Entities.getPlayer model.lords



-- items and path drawing on the game map
----------------------------------------------------------


hasActionOnPoint : Vector.Point -> Types.MapTileMsg -> MapDrawer.MapClickAction -> Bool
hasActionOnPoint p msg dict =
    List.member msg (MapDrawer.actionsOnPoint p dict)


canMoveToPoint : MapDrawer.MapClickAction -> Vector.Point -> Bool
canMoveToPoint dict p =
    hasActionOnPoint p (Types.MoveTo p) dict


buildAllMapSvgs : Model -> MapDrawer.MapClickAction
buildAllMapSvgs m =
    filterMapSvgs
        (buildPathSvgs m
            (List.foldl
                (Entities.Drawer.drawSettlement (getPlayer m))
                (List.foldl (Entities.Drawer.drawLord (getPlayer m)) (drawnMap m.map) (Entities.flattenLordList m.lords))
                (allSettlements m)
            )
        )


filterMapSvgs : MapDrawer.MapClickAction -> MapDrawer.MapClickAction
filterMapSvgs =
    Dict.map (\_ v -> filterInteractables v)


filterInteractables : List MapDrawer.InteractableSvg -> List MapDrawer.InteractableSvg
filterInteractables =
    List.foldr
        (\svg r ->
            if MapDrawer.isSvgAllowedIn svg r then
                svg :: r

            else
                r
        )
        []


buildPathSvgs : Model -> MapDrawer.MapClickAction -> MapDrawer.MapClickAction
buildPathSvgs m mapDict =
    let
        player =
            Entities.getPlayer m.lords
    in
    case getSelectedPath m of
        Nothing ->
            mapDict

        Just path ->
            PathDrawer.drawPath
                player.agent
                (Pathfinder.cutFirstStepFromPath path)
                mapDict


getSelectedPath : Model -> Maybe Pathfinder.Path
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


getPathTo : Vector.Point -> Vector.Point -> Map.Map -> Maybe Pathfinder.Path
getPathTo from to map =
    if canMoveToPoint (drawnMap map) to then
        Pathfinder.getPath
            from
            (Pathfinder.PathInfo (MapGenerator.getNav map) to)

    else
        Nothing


allSettlements : Model -> List Entities.Settlement
allSettlements m =
    List.concat (List.map .land (Entities.flattenLordList m.lords))


drawnMap : Map.Map -> MapDrawer.MapClickAction
drawnMap map =
    Map.drawMap map



-- testLordData For Battlesimulation


testWorldEntity : Entities.WorldEntity
testWorldEntity =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Malaca"
    }


testSetelement : Entities.Settlement
testSetelement =
    { entity = testWorldEntity
    , settlementType = Entities.Castle
    , recruitLimits = Troops.emptyTroops
    , income = 3.19
    , isSieged = False
    , buildings = Building.startBuildings
    }


testLordWorldEntity : Entities.WorldEntity
testLordWorldEntity =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "RÜDIGER"
    }


testLordWorldEntity2 : Entities.WorldEntity
testLordWorldEntity2 =
    { army = Troops.startTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Peter von Haskell"
    }


testLord2 : Entities.Lord
testLord2 =
    { entity = testLordWorldEntity2
    , gold = 450
    , land = [ testSetelement ]
    , agent = PathAgent.getAgent 6
    }


testLord : Entities.Lord
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


initPlayer : Map.Map -> Int -> Float -> Entities.Lord
initPlayer m i rad =
    let
        entity =
            Entities.WorldEntity
                Troops.startTroops
                (Faction.getFaction i)
                (Pathfinder.getClosestFreeFieldAt (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad)) (MapGenerator.getNav m) Dict.empty)
                ("Lord " ++ String.fromInt i)
    in
    Entities.Lord
        entity
        250
        (initSettlementsFor m Dict.empty entity i)
        (PathAgent.getAgent 5)


initSettlementsFor : Map.Map -> Dict.Dict Int () -> Entities.WorldEntity -> Int -> List Entities.Settlement
initSettlementsFor m usedFields e i =
    Entities.createCapitalFor e
        (Maybe.withDefault
            (e.name ++ "`s Capital`")
            (ListExt.getElementAt i Entities.castleNames)
        )
        :: List.map
            Entities.getSettlementFor
            (getVillagesInQuadrant m e i villagesPerLord |> getSafeSettlementInfos m usedFields)


getVillagesInQuadrant : Map.Map -> Entities.WorldEntity -> Int -> Int -> List Entities.SettlementInfo
getVillagesInQuadrant m e q i =
    List.map
        (\index ->
            Entities.SettlementInfo
                Entities.Village
                (getVillagesPosition i q index e.position)
                (Maybe.withDefault
                    (e.name ++ " " ++ String.fromInt i ++ "th Village`")
                    (ListExt.getElementAt (q * 4 + index) Entities.villageNames)
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


getSafeSettlementInfos : Map.Map -> Dict.Dict Int () -> List Entities.SettlementInfo -> List Entities.SettlementInfo
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


getSafeSettlementInfo : Entities.SettlementInfo -> Map.Map -> Dict.Dict Int () -> Entities.SettlementInfo
getSafeSettlementInfo i m dict =
    Entities.editSettlmentInfoPosition (Pathfinder.getClosestFreeFieldAt i.position (MapGenerator.getNav m) dict) i



-- init the view (build the base layout of the page)
----------------------------------------------------------


view : Model -> Html Types.Msg
view model =
    let
        allClickActions =
            buildAllMapSvgs model
    in
    div [ Html.Attributes.class "page-container" ]
        [ findModalWindow model
        , HeaderTemplate.generateHeaderTemplate (Entities.getPlayer model.lords) model.date
        , div [ Html.Attributes.class "page-map" ]
            (List.map addStylesheet stylessheets ++
            [ MapActionTemplate.generateMapActionTemplate model.selectedPoint allClickActions
            , div []
                [ Svg.svg
                    [ Svg.Attributes.viewBox "0 0 850 1000"
                    , Svg.Attributes.fill "none"
                    ]
                    (MapDrawer.allSvgs allClickActions)
                ]
            , EventTemplate.generateEventOverview model.event

            , span [] [ Html.text (Debug.toString (Entities.getPlayer model.lords).land) ]
            ])
        ]



-- load / show right modal-window by the current model menue state
----------------------------------------------------------


findModalWindow : Model -> Html Types.Msg
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


addStylesheet : String -> Html Types.Msg
addStylesheet href =
    Html.node "link" [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" ("./assets/styles/" ++ href ++ ".css") ] []

stylessheets : List String
stylessheets = ["main_styles", "battle_styles", "end_styles", "event_styles", "header_styles", "lord_styles", "mapaction_styles", "settlement_styles", "start_styles", "tooltip_styles"]

-- update function
----------------------------------------------------------


update : Types.Msg -> Model -> Model
update msg model =
    case msg of
        Types.EndRound ->
            { model | date = DateExt.addMonths 1 model.date, lords = Entities.Cons (endRoundForLord (getPlayer model)) (updateAIsAfterPlayerRound (Entities.npcs model.lords)) }

        Types.EndGame bool ->
            { model | gameState = GameOver bool }

        Types.CloseModal ->
            { model | gameState = GameSetup GameMenue }

        Types.BattleAction bmsg ->
            updateBattle bmsg model

        Types.SettlementAction action ->
            updateSettlement action model

        Types.EventAction emsg ->
            updateEvent emsg model

        Types.MapTileAction action ->
            updateMaptileAction model action

        Types.Click p ->
            { model | selectedPoint = Just p }


updateAIsAfterPlayerRound : List Entities.Lord -> List Entities.Lord
updateAIsAfterPlayerRound lords =
    List.map (\l -> updateAI l |> endRoundForLord) lords


updateAI : Entities.Lord -> Entities.Lord
updateAI lord =
    { lord | entity = Entities.setPosition lord.entity (Vector.addPoints (Vector.Point 1 1) lord.entity.position) }


endRoundForLord : Entities.Lord -> Entities.Lord
endRoundForLord l =
    Entities.applyLordGoldIncome l |> Entities.resetUsedMovement |> Entities.applyLordNewRecruits



-- update function for the map action messages
-- like move to a point or engage a battle
----------------------------------------------------------


updateMaptileAction : Model -> Types.MapTileMsg -> Model
updateMaptileAction model ma =
    case ma of
        Types.LordMsg msg lord ->
            updateLordAction msg lord model

        Types.SettlementMsg msg settlement ->
            case msg of
                Types.ViewSettlement ->
                    { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Types.RestrictedView) }

                Types.EnterSettlement ->
                    { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Types.StandardView) }

                Types.SiegeSettlement ->
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

        Types.MoveTo p ->
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


updateLordAction : Types.LordTileMsg -> Entities.Lord -> Model -> Model
updateLordAction msg lord m =
    case msg of
        Types.ViewLord ->
            { m | gameState = GameSetup (LordView lord) }

        Types.EngageLord ->
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


updateSettlement : Types.SettlementMsg -> Model -> Model
updateSettlement msg model =
    case msg of
        Types.UIMsg umsg ->
            updateSettlementUI umsg model

        Types.TroopMsg tmsg ->
            updateSettlementStats tmsg model


updateSettlementUI : Types.SettlementUIMsg -> Model -> Model
updateSettlementUI msg model =
    case msg of
        Types.ShowBuyTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.RecruitView) }

        Types.ShowStationTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.StationView) }

        Types.ShowSettlement s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.StandardView) }

        Types.ShowBuildings s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.BuildingView) }


updateSettlementStats : Types.SettlementArmyMsg -> Model -> Model
updateSettlementStats msg model =
    case msg of
        Types.BuyTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.buyTroops (Entities.getPlayer model.lords) t s))
                s
                Types.RecruitView
                model

        Types.StationTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.stationTroops (Entities.getPlayer model.lords) t s))
                s
                Types.StationView
                model

        Types.TakeTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.takeTroops (Entities.getPlayer model.lords) t s))
                s
                Types.StationView
                model

        Types.UpgradeBuilding b s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.upgradeBuilding (Entities.getPlayer model.lords) b s))
                s
                Types.BuildingView
                model 


updateMultipleTroopStats : Entities.LordList -> Entities.Settlement -> Types.UiSettlementState -> Model -> Model
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


updateBattle : Types.BattleMsg -> Model -> Model
updateBattle msg model =
    case msg of
        Types.StartSkirmish bS ->
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

        Types.SkipSkirmishes bS ->
            { model | gameState = GameSetup (BattleView (Battle.skipBattle bS (Map.getTerrainForPoint bS.attacker.entity.position model.map))) }

        Types.FleeBattle bS ->
            updateLordsAfterBattle
                (Battle.fleeBattle bS)
                (List.map (\x -> OperatorExt.ternary (x.entity.name == bS.defender.entity.name) bS.defender x) (Entities.tailLordList model.lords))
                model
                (GameSetup GameMenue)

        Types.EndBattle bS ->
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


updateLordsAfterBattle : Entities.Lord -> List Entities.Lord -> Model -> GameState -> Model
updateLordsAfterBattle player enemyLords model state = 
            { model | lords = Entities.Cons player enemyLords, gameState = state }


checkLordLost : Bool -> String -> List Entities.Lord -> List Entities.Lord
checkLordLost k n l =
    if k then
        List.filter (\x -> x.entity.name /= n) l

    else
        l



-- update function for the event log system
----------------------------------------------------------


updateEvent : Types.EventMsg -> Model -> Model
updateEvent msg model =
    case msg of
        Types.DeleteEvent index ->
            { model | event = Event.removeEvent model.event index }

        Types.SwitchEventView ->
            { model | event = Event.updateEventState model.event }

        Types.ClearEvents ->
            { model | event = Event.clearEvents model.event }


main : Program () Model Types.Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }
