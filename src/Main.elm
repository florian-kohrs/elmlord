module Main exposing (..)

import AI
import AI.Model
import Battle
import Battle.Model
import Browser
import DateExt
import Dict
import Entities
import Entities.Drawer
import Entities.Lords
import Entities.Model
import Event
import Faction
import Html exposing (Html, audio, div, span, text)
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
import MaybeExt
import Msg
import OperatorExt
import PathAgent
import Pathfinder
import Pathfinder.Drawer
import Pathfinder.Model
import Ports
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Templates.BattleTemplate as BattleTemplate
import Templates.EndTemplate as EndTemplate
import Templates.EventTemplate as EventTemplate
import Templates.HeaderTemplate as HeaderTemplate
import Templates.LordTemplate as LordTemplate
import Templates.MapActionTemplate as MapActionTemplate
import Templates.SettlementTemplate as SettlementTemplate
import Templates.StartTemplate as StartTemplate
import Templates.TroopTemplate as TroopTemplate
import Time
import Troops
import Vector



-- Model and the states of the UI (show/hide specific layouts)
----------------------------------------------------------


type alias Model =
    { lords : Entities.Lords.LordList
    , gameState : GameState
    , selectedPoint : Maybe Vector.Point
    , date : DateExt.Date
    , map : Map.Model.Map
    , errorMsg : String
    , event : Event.EventState
    , playersTurn : Int
    , playerInput : String
    , volume : Int
    }


type GameState
    = GameSetup UiState
    | GameOver Bool


type UiState
    = MainMenue MainMenueState
    | GameMenue
    | BattleView Battle.Model.BattleStats
    | TroopView Entities.Model.Lord
    | SettlementView Entities.Model.Lord Entities.Model.Settlement Msg.UiSettlementState
    | LordView Entities.Model.Lord


aiTickFrequenz : Float
aiTickFrequenz =
    10


type MainMenueState
    = Menue
    | Campaign


hashString : String -> Int
hashString s =
    Tuple.first <|
        List.foldl
            (\c ( r, count ) ->
                ( r + Char.toCode c * count
                , count + 1
                )
            )
            ( 2276, 1 )
        <|
            String.toList
                s


getPlayer : Model -> Entities.Model.Lord
getPlayer model =
    Entities.Lords.getPlayer model.lords


isPlayersTurn : Model -> Bool
isPlayersTurn model =
    model.playersTurn == 0



-- items and path drawing on the game map
----------------------------------------------------------


canMoveToPoint : MapAction.Model.InteractableMapSVG -> Vector.Point -> Bool
canMoveToPoint dict p =
    MapAction.hasActionOnPoint p (MapAction.SubModel.MoveTo p) dict


buildAllMapSvgs : Model -> MapAction.Model.InteractableMapSVG
buildAllMapSvgs m =
    filterMapSvgs
        (buildPathSvgs m
            (List.foldl
                (Entities.Drawer.drawSettlement (getPlayer m))
                (List.foldl (Entities.Drawer.drawLord (getPlayer m)) (drawnMap m.map) (Entities.Lords.lordListToList m.lords))
                (allSettlements m)
            )
        )


filterMapSvgs : MapAction.Model.InteractableMapSVG -> MapAction.Model.InteractableMapSVG
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


buildPathSvgs : Model -> MapAction.Model.InteractableMapSVG -> MapAction.Model.InteractableMapSVG
buildPathSvgs m mapDict =
    let
        player =
            Entities.Lords.getPlayer m.lords
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
            Entities.Lords.getPlayer m.lords
    in
    case m.selectedPoint of
        Just point ->
            getPathTo player.entity.position point m.map

        _ ->
            Nothing


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
    List.concat (List.map .land (Entities.Lords.lordListToList m.lords))


drawnMap : Map.Model.Map -> MapAction.Model.InteractableMapSVG
drawnMap map =
    Map.Drawer.drawMap map



-- init game data (set the model data)
-- add the player, lords, settlements, etc.
----------------------------------------------------------


startGame : String -> Cmd Msg.Msg -> ( Model, Cmd Msg.Msg )
startGame playerName cmd =
    ( initialModel playerName, cmd )


initialModel : String -> Model
initialModel playerName =
    let
        map =
            MapGenerator.createMap (hashString playerName)
    in
    Model (initPlayers playerName map Entities.Model.playerCount) (GameSetup (MainMenue Menue)) Nothing (DateExt.Date 1017 DateExt.Jan) map "" { state = True, events = [] } 0 playerName 10


initPlayers : String -> Map.Model.Map -> Int -> Entities.Lords.LordList
initPlayers playerName m count =
    Entities.Lords.Cons
        (initPlayer (Just playerName) m 0 (2 * 0.125))
        (List.map
            (\i ->
                initAI
                    (initPlayer
                        (ListExt.getElementAt
                            i
                            Entities.Model.aiNames
                        )
                        m
                        i
                        (2 * (toFloat i / toFloat count + 0.125))
                    )
                    (toFloat i)
                    (toFloat (hashString playerName) / 97)
            )
            (List.range 1 (count - 1))
        )


initAI : Entities.Model.Lord -> Float -> Float -> AI.Model.AI
initAI l i offset =
    AI.Model.AI l
        (AI.Model.ActionMultipliers
            (AI.getAiActionMultiplier (offset + 0.3 + 2 * i / toFloat Entities.Model.playerCount))
            (AI.getAiActionMultiplier (offset + 0.7 + 2 * i / toFloat Entities.Model.playerCount))
            (AI.getAiActionMultiplier (offset + 1 + 2 * i / toFloat Entities.Model.playerCount))
            (AI.getAiActionMultiplier (offset + 1.3 + 2 * i / toFloat Entities.Model.playerCount))
            (AI.getAiActionMultiplier (offset + 1.6 + 2 * i / toFloat Entities.Model.playerCount))
        )


initPlayer : Maybe String -> Map.Model.Map -> Int -> Float -> Entities.Model.Lord
initPlayer name m i rad =
    let
        entity =
            Entities.Model.WorldEntity
                Troops.lordStartTroops
                (Faction.getFaction i)
                (Pathfinder.getClosestFreeFieldAt (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad)) (Pathfinder.getNav m) Dict.empty)
                (Maybe.withDefault
                    ("Lord " ++ String.fromInt i)
                    name
                )
    in
    Entities.Model.Lord
        entity
        5000
        (initSettlementsFor m Dict.empty entity i)
        (PathAgent.getAgent 6)


initSettlementsFor : Map.Model.Map -> Dict.Dict Int () -> Entities.Model.WorldEntity -> Int -> List Entities.Model.Settlement
initSettlementsFor m usedFields e i =
    Entities.createCapitalFor e
        (Maybe.withDefault
            (e.name ++ "`s Capital`")
            (ListExt.getElementAt i Entities.Model.castleNames)
        )
        :: List.map
            Entities.getSettlementFor
            (getVillagesInQuadrant e i Entities.Model.villagesPerLord |> getSafeSettlementInfos m usedFields)


getVillagesInQuadrant : Entities.Model.WorldEntity -> Int -> Int -> List Entities.Model.SettlementInfo
getVillagesInQuadrant e q i =
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
getVillagesPosition max q i p =
    let
        distanceFromCapital =
            Entities.Model.villageCaptialDistance
                + toFloat (round (4 * sin (pi * toFloat (i - 1) / toFloat (max - 1))))

        rad =
            0.5 * pi * (toFloat i / toFloat max + (toFloat -q + 1.9))

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
    div []
        [ case model.gameState of
            GameSetup uistate ->
                case uistate of
                    MainMenue state ->
                        setMenueView state model.playerInput

                    _ ->
                        setGameView model

            GameOver _ ->
                setGameView model
        , audio [ src "./assets/sounds/songs/background.wav", Html.Attributes.loop True, Html.Attributes.id "audio-player" ] []
        , audio [ Html.Attributes.id "sound-player" ] []
        ]


setMenueView : MainMenueState -> String -> Html Msg.Msg
setMenueView state v =
    div [ Html.Attributes.class "main-container" ]
        (case state of
            Menue ->
                List.map addStylesheet stylessheets ++ StartTemplate.startMenuTemplate

            Campaign ->
                List.map addStylesheet stylessheets ++ StartTemplate.startCampaign v
        )


setGameView : Model -> Html Msg.Msg
setGameView model =
    let
        allClickActions =
            buildAllMapSvgs model
    in
    div [ Html.Attributes.class "page-container" ]
        [ findModalWindow model
        , HeaderTemplate.generateHeaderTemplate model.volume (Entities.Lords.getPlayer model.lords) model.date
        , div [ Html.Attributes.class "page-map" ]
            (List.map addStylesheet stylessheets
                ++ [ buildMapActionTemplate model allClickActions
                   , div []
                        [ Svg.svg
                            [ Svg.Attributes.viewBox "0 0 850 1000"
                            , Svg.Attributes.fill "none"
                            ]
                            (MapAction.allSvgs allClickActions)
                        ]
                   , EventTemplate.generateEventOverview model.event
                   ]
            )
        ]


buildMapActionTemplate : Model -> MapAction.Model.InteractableMapSVG -> Html Msg.Msg
buildMapActionTemplate model allClickActions =
    if isPlayersTurn model then
        MapActionTemplate.generateMapActionTemplate model.selectedPoint (getPlayer model) allClickActions

    else
        let
            lordName =
                MaybeExt.foldMaybe
                    (\l -> l.entity.name)
                    ("Unkown Enemy" ++ String.fromInt model.playersTurn ++ String.fromInt (List.length <| Entities.Lords.lordListToList model.lords))
                    (ListExt.getElementAt
                        model.playersTurn
                        (Entities.Lords.lordListToList model.lords)
                    )
        in
        div [ Html.Attributes.class "map-action-menu" ]
            --add enemy turn skip button
            (span [] [ Html.text ("It`s " ++ lordName ++ "`s turn") ] :: [])



-- load / show right modal-window by the current model menue state
----------------------------------------------------------


findModalWindow : Model -> Html Msg.Msg
findModalWindow model =
    case model.gameState of
        GameSetup uistate ->
            case uistate of
                SettlementView l s u ->
                    SettlementTemplate.generateSettlementModalTemplate (getPlayer model).entity.faction l s u

                LordView l ->
                    LordTemplate.generateLordTemplate l

                BattleView bS ->
                    BattleTemplate.generateBattleTemplate bS (Map.getTerrainForPoint bS.attacker.entity.position model.map)

                TroopView l ->
                    TroopTemplate.generateTroopTemplate l

                _ ->
                    div [] []

        GameOver bool ->
            EndTemplate.generateEndTemplate bool



-- for the implementation of design during the development
----------------------------------------------------------


addStylesheet : String -> Html Msg.Msg
addStylesheet href =
    Html.node "link" [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" ("./assets/styles/" ++ href ++ ".css") ] []


stylessheets : List String
stylessheets =
    [ "main_styles", "battle_styles", "end_styles", "event_styles", "header_styles", "lord_styles", "mapaction_styles", "settlement_styles", "start_styles", "tooltip_styles", "troop_styles" ]



-- update function
----------------------------------------------------------


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.EventAction emsg ->
            emptyCmd (updateEvent emsg model)

        Msg.EndGame bool ->
            emptyCmd { model | gameState = GameOver bool }

        other ->
            let
                ( newModel, cmd ) =
                    if isPlayersTurn model then
                        case other of
                            Msg.EndRound ->
                                emptyCmd (endAnyRound <| { model | date = DateExt.addMonths 1 model.date, lords = Entities.Lords.updatePlayer model.lords (endRoundForLord (getPlayer model)) })

                            Msg.CloseModal ->
                                emptyCmd { model | gameState = GameSetup GameMenue }

                            Msg.BattleAction bmsg ->
                                updateBattle bmsg model

                            Msg.MenueAction mmsg ->
                                updateMenue mmsg model

                            Msg.SettlementAction action ->
                                emptyCmd (updateSettlement action model)

                            Msg.MapTileAction action ->
                                updateMaptileAction model action

                            Msg.TroopAction tomsg ->
                                emptyCmd (updateTroopOverView model tomsg)

                            Msg.Click p ->
                                emptyCmd { model | selectedPoint = Just p }

                            _ ->
                                emptyCmd model

                    else
                        case msg of
                            Msg.AiRoundTick ->
                                emptyCmd <| playAiTurn model

                            _ ->
                                emptyCmd <| { model | errorMsg = "Its not your turn" }
            in
            ( checkIfGameEnded <| removeLordsWithoutCapitalFromModel newModel, cmd )


hasGameStarted : Model -> Bool
hasGameStarted m =
    case m.gameState of
        GameSetup state ->
            case state of
                MainMenue _ ->
                    False

                _ ->
                    True

        _ ->
            True


checkIfGameEnded : Model -> Model
checkIfGameEnded m =
    if hasGameStarted m then
        if hasPlayerLost m then
            { m | gameState = GameOver False }

        else if (List.length <| Entities.Lords.npcs m.lords) == 0 then
            { m | gameState = GameOver True }

        else
            m

    else
        m


endAnyRound : Model -> Model
endAnyRound m =
    if m.playersTurn + 1 >= (List.length <| Entities.Lords.lordListToList m.lords) then
        { m | playersTurn = 0 }

    else
        { m | playersTurn = m.playersTurn + 1 }


playAiTurn : Model -> Model
playAiTurn m =
    case ListExt.getElementAt (m.playersTurn - 1) (Entities.Lords.getAis m.lords) of
        Nothing ->
            endAnyRound m

        Just ai ->
            let
                action =
                    AI.getAiAction
                        ai
                        (PathAgent.lordsTurnToReachDestination m.map)
                        (PathAgent.canMoveTowardsInTurn m.map)
                        (Entities.Lords.getLordsExcept m.lords ai.lord)
            in
            case action of
                AI.Model.EndRound ->
                    endAnyRound <|
                        { m
                            | lords =
                                Entities.Lords.replaceAi m.lords
                                    (AI.setLord ai (endRoundForLord ai.lord))
                        }

                other ->
                    let
                        ( newLords, actionEvent ) =
                            AI.updateAi ai other (OperatorExt.flip Map.getTerrainForPoint <| m.map) (PathAgent.moveLordOnPath m.map) m.lords
                    in
                    { m
                        | lords =
                            newLords
                        , event =
                            MaybeExt.foldMaybe (\event -> Event.appendEvent m.event event)
                                m.event
                                actionEvent

                        {- Event.appendEvent
                           m.event
                           (Event.Event
                               ai.lord.entity.name
                               (List.foldl (\a s -> s ++ ";\n " ++ AI.showAiRoundActionPreference a) "Plain Action Preferences" (List.sortBy (\a -> -a.actionValue) <| AI.getAiActions ai (Entities.Lords.getLordsExcept m.lords ai.lord))
                                   ++ List.foldl (\a s -> s ++ ";\n " ++ AI.showAiRoundActionPreference a) "\n\nWith Distance Penalty Action Preferences" (List.sortBy (\a -> -a.actionValue) <| AI.getAiActionsWithDistancePenalty ai (PathAgent.lordsTurnToReachDestination m.map) (Entities.Lords.getLordsExcept m.lords ai.lord))
                               )
                               --(AI.showAiRoundAction other)
                               Event.Minor
                           )
                        -}
                    }


endRoundForLord : Entities.Model.Lord -> Entities.Model.Lord
endRoundForLord l =
    Entities.applyLordGoldIncome l |> PathAgent.resetLordUsedMovement |> Entities.applyLordNewRecruits



-- update function for troop overview in the header
----------------------------------------------------------


updateTroopOverView : Model -> Msg.TroopOverviewMsg -> Model
updateTroopOverView model msg =
    case msg of
        Msg.TroopActionMsg ->
            { model | gameState = GameSetup (TroopView (getPlayer model)) }

        Msg.TroopArmyMsg t ->
            let
                newLords =
                    Entities.Lords.updatePlayer model.lords (Entities.disbandTroops (Entities.Lords.getPlayer model.lords) t)
            in
            { model | lords = newLords, gameState = GameSetup (TroopView (Entities.Lords.getPlayer newLords)) }



-- update function for the map action messages
-- like move to a point or engage a battle
----------------------------------------------------------


updateMaptileAction : Model -> MapAction.SubModel.MapTileMsg -> ( Model, Cmd Msg.Msg )
updateMaptileAction model ma =
    case ma of
        MapAction.SubModel.LordMsg msg lord ->
            updateLordAction msg lord model

        MapAction.SubModel.SettlementMsg msg settlement ->
            case msg of
                MapAction.SubModel.ViewSettlement ->
                    case Entities.factionToLord settlement.entity.faction (Entities.Lords.lordListToList model.lords) of
                        Nothing ->
                            emptyCmd { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Msg.RestrictedView) }

                        Just lord ->
                            emptyCmd { model | gameState = GameSetup (SettlementView lord settlement Msg.RestrictedView) }

                MapAction.SubModel.EnterSettlement ->
                    emptyCmd { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Msg.StandardView) }

                MapAction.SubModel.SiegeSettlement ->
                    let
                        defender =
                            Entities.findLordWithSettlement settlement (Entities.Lords.lordListToList model.lords)
                    in
                    case defender of
                        Nothing ->
                            emptyCmd model

                        Just l ->
                            ( { model
                                | gameState =
                                    GameSetup
                                        (BattleView
                                            { attacker = getPlayer model
                                            , defender = l
                                            , round = 1
                                            , attackerCasualties = Troops.emptyTroops
                                            , defenderCasualties = Troops.emptyTroops
                                            , settlement = Just settlement
                                            , finished = False
                                            }
                                        )
                              }
                            , Ports.playSound "KampfschreiLight"
                            )

        MapAction.SubModel.MoveTo p ->
            let
                player =
                    getPlayer model
            in
            case getPathTo player.entity.position p model.map of
                Nothing ->
                    emptyCmd model

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
                    emptyCmd { model | lords = Entities.Lords.Cons newPlayer (Entities.Lords.getAis model.lords) }


updateLordAction : MapAction.SubModel.LordTileMsg -> Entities.Model.Lord -> Model -> ( Model, Cmd Msg.Msg )
updateLordAction msg lord m =
    case msg of
        MapAction.SubModel.ViewLord ->
            emptyCmd { m | gameState = GameSetup (LordView lord) }

        MapAction.SubModel.EngageLord ->
            ( { m
                | gameState =
                    GameSetup
                        (BattleView
                            { attacker = getPlayer m
                            , defender = lord
                            , round = 1
                            , attackerCasualties = Troops.emptyTroops
                            , defenderCasualties = Troops.emptyTroops
                            , settlement = Nothing
                            , finished = False
                            }
                        )
              }
            , Ports.playSound "KampfschreiLight"
            )


removeLordsWithoutCapitalFromModel : Model -> Model
removeLordsWithoutCapitalFromModel m =
    let
        removedLords =
            List.filter (\ai -> not <| Entities.hasCapital ai.lord) <| Entities.Lords.getAis m.lords

        newTurnIndex =
            List.foldl
                (\l index ->
                    if Entities.Lords.getLordIndex m.lords l > m.playersTurn then
                        index - 1

                    else
                        index
                )
                m.playersTurn
                (List.map .lord removedLords)
    in
    { m | lords = removeLordsWithoutCapital m.lords, playersTurn = newTurnIndex }


removeLordsWithoutCapital : Entities.Lords.LordList -> Entities.Lords.LordList
removeLordsWithoutCapital lordList =
    Entities.Lords.updateNpcs lordList <| List.filter (\ai -> Entities.hasCapital ai.lord) <| Entities.Lords.getAis lordList


hasPlayerLost : Model -> Bool
hasPlayerLost m =
    not <| MaybeExt.hasValue <| Entities.getLordCapital <| .land <| getPlayer m



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
            { model | gameState = GameSetup (SettlementView (Entities.Lords.getPlayer model.lords) s Msg.RecruitView) }

        Msg.ShowStationTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.Lords.getPlayer model.lords) s Msg.StationView) }

        Msg.ShowSettlement s ->
            { model | gameState = GameSetup (SettlementView (Entities.Lords.getPlayer model.lords) s Msg.StandardView) }

        Msg.ShowBuildings s ->
            { model | gameState = GameSetup (SettlementView (Entities.Lords.getPlayer model.lords) s Msg.BuildingView) }


updateSettlementStats : Msg.SettlementArmyMsg -> Model -> Model
updateSettlementStats msg model =
    case msg of
        Msg.BuyTroops t s ->
            updateMultipleTroopStats
                (Entities.Lords.updatePlayer model.lords (Entities.buyTroops (Entities.Lords.getPlayer model.lords) t s))
                s
                Msg.RecruitView
                model

        Msg.BuyAllTroops s ->
            updateMultipleTroopStats
                (Entities.Lords.updatePlayer model.lords (Entities.buyAllTroops (Entities.Lords.getPlayer model.lords) s))
                s
                Msg.RecruitView
                model

        Msg.StationTroops t s ->
            updateMultipleTroopStats
                (Entities.Lords.updatePlayer model.lords (Entities.stationTroops (Entities.Lords.getPlayer model.lords) t s))
                s
                Msg.StationView
                model

        Msg.TakeTroops t s ->
            updateMultipleTroopStats
                (Entities.Lords.updatePlayer model.lords (Entities.takeTroops (Entities.Lords.getPlayer model.lords) t s))
                s
                Msg.StationView
                model

        Msg.UpgradeBuilding b s ->
            updateMultipleTroopStats
                (Entities.Lords.updatePlayer model.lords (Entities.upgradeBuilding (Entities.Lords.getPlayer model.lords) b s))
                s
                Msg.BuildingView
                model


updateMultipleTroopStats : Entities.Lords.LordList -> Entities.Model.Settlement -> Msg.UiSettlementState -> Model -> Model
updateMultipleTroopStats l s u m =
    let
        newSettle =
            Entities.getSettlementByName (Entities.Lords.getPlayer l).land s.entity.name
    in
    case newSettle of
        Nothing ->
            m

        Just x ->
            { m
                | lords = l
                , gameState = GameSetup (SettlementView (Entities.Lords.getPlayer l) x u)
            }



-- update function for the battle messages
-- like start a skirmish, flee a battle, etc.
----------------------------------------------------------


updateBattle : Msg.BattleMsg -> Model -> ( Model, Cmd Msg.Msg )
updateBattle msg model =
    case msg of
        Msg.StartSkirmish bS ->
            let
                newBattleStats =
                    Battle.evaluateBattleResult bS (Map.getTerrainForPoint bS.attacker.entity.position model.map)
            in
            ( { model | gameState = GameSetup (BattleView newBattleStats) }, Ports.playSound "Kampfgeklirre2Sec" )

        Msg.SkipSkirmishes bS ->
            emptyCmd { model | gameState = GameSetup (BattleView (Battle.skipBattle (Map.getTerrainForPoint bS.attacker.entity.position model.map) bS)) }

        Msg.FleeBattle bS ->
            ( { model | gameState = GameSetup GameMenue, lords = Battle.fleeBattle model.lords bS }, getBattleAftermathSound bS )

        Msg.EndBattle bS ->
            ( { model | gameState = GameSetup GameMenue, lords = Battle.applyBattleAftermath model.lords bS }, getBattleAftermathSound bS )


getBattleAftermathSound : Battle.Model.BattleStats -> Cmd Msg.Msg
getBattleAftermathSound bS =
    if Troops.sumTroops bS.attacker.entity.army /= 0 then
        Ports.transitSoundToMusic ( "Kampfsieg", 3500 )

    else
        Ports.startMusic ""



-- updae function for the menue
----------------------------------------------------------


updateMenue : Msg.MenueMsg -> Model -> ( Model, Cmd Msg.Msg )
updateMenue msg model =
    case msg of
        Msg.StartGame name ->
            let
                ( startModel, cmd ) =
                    startGame name <| Ports.startMusic "background"
            in
            ( { startModel | gameState = GameSetup GameMenue }, cmd )

        Msg.ChangeName str ->
            emptyCmd { model | playerInput = str }

        Msg.ChangeVolume vol ->
            ( { model | volume = vol }, Ports.updateVolume vol )

        Msg.ShowMenue ->
            emptyCmd { model | gameState = GameSetup (MainMenue Menue) }

        Msg.ShowDocumentation ->
            ( { model | gameState = GameSetup (MainMenue Menue) }, Ports.openLink "https://github.com/flofe104/elmlord/wiki" )

        Msg.SetCampaingn ->
            ( { model | gameState = GameSetup (MainMenue Campaign) }, Ports.startMusic "menue" )

        Msg.ShowCredits ->
            ( { model | gameState = GameSetup (MainMenue Menue) }, Ports.openLink "https://github.com/flofe104/elmlord" )



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


isGameOver : Model -> Bool
isGameOver m =
    case m.gameState of
        GameOver _ ->
            True

        _ ->
            False


tickSub : Model -> Sub Msg.Msg
tickSub model =
    if isPlayersTurn model || isGameOver model then
        Sub.none

    else
        Time.every aiTickFrequenz (\_ -> Msg.AiRoundTick)


emptyCmd : Model -> ( Model, Cmd Msg.Msg )
emptyCmd m =
    ( m, Cmd.none )


main : Program () Model Msg.Msg
main =
    Browser.element
        { init = \_ -> startGame "Jan von Haskell" Cmd.none
        , subscriptions = \m -> tickSub m
        , view = view
        , update = update
        }
