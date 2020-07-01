module Main exposing (..)

import AI
import Battle
import Browser
import DateExt
import Dict
import Entities exposing (..)
import EntitiesDrawer
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ListExt
import Map exposing (Map, MapTile)
import MapData exposing (..)
import MapDrawer
import MapGenerator exposing (createMap)
import MaybeExt
import OperatorExt
import PathAgent
import PathDrawer
import Pathfinder
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Templates.BattleTemplate exposing (..)
import Templates.EndTemplate exposing (..)
import Templates.HeaderTemplate exposing (..)
import Templates.LordTemplate exposing (..)
import Templates.MapActionTemplate exposing (..)
import Templates.SettlementTemplate exposing (..)
import Troops exposing (Troop, TroopType)
import Types exposing (BattleMsg(..), MapTileMsg(..), Msg(..), SettlementArmyMsg(..), SettlementMsg(..), SettlementUIMsg(..), UiSettlementState(..))
import Vector exposing (..)


type alias Model =
    { lords : LordList
    , gameState : GameState
    , selectedPoint : Maybe Point
    , date : DateExt.Date
    , map : Map.Map --used for pathfinding
    }


type GameState
    = GameSetup UiState
    | InGame Int Int -- playerCount, playersTurn
    | GameOver Bool -- true = gewonnen, false = verloren


type UiState
    = MainMenue
    | SaveLoad
    | NewCampain
    | GameMenue
    | BattleView Entities.BattleStats
    | SettlementView Lord Settlement UiSettlementState
    | LordView Lord


hasActionOnPoint : Vector.Point -> MapTileMsg -> MapDrawer.MapClickAction -> Bool
hasActionOnPoint p msg dict =
    List.member msg (MapDrawer.actionsOnPoint p dict)


canMoveToPoint : MapDrawer.MapClickAction -> Vector.Point -> Bool
canMoveToPoint dict p =
    hasActionOnPoint p (MoveTo p) dict


buildAllMapSvgs : Model -> MapDrawer.MapClickAction
buildAllMapSvgs m =
    filterMapSvgs
        (buildPathSvgs m
            (List.foldl
                (EntitiesDrawer.drawSettlement (getPlayer m))
                (List.foldl (EntitiesDrawer.drawLord (getPlayer m)) (drawnMap m.map) (Entities.flattenLordList m.lords))
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


allSettlements : Model -> List Settlement
allSettlements m =
    List.concat (List.map .land (Entities.flattenLordList m.lords))



-- begin STATIC TEST DATA


testTroopList : List Troop
testTroopList =
    [ { amount = 30, troopType = Troops.Sword }, { amount = 30, troopType = Troops.Spear }, { amount = 30, troopType = Troops.Archer }, { amount = 30, troopType = Troops.Knight } ]



-- testLordData For Battlesimulation


secondLordTroops : List Troop
secondLordTroops =
    [ { amount = 20, troopType = Troops.Sword }, { amount = 45, troopType = Troops.Spear }, { amount = 10, troopType = Troops.Archer }, { amount = 5, troopType = Troops.Knight } ]


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
    , settlementType = Entities.Castle
    , income = 3.19
    , isSieged = False
    }


testLordWorldEntity : WorldEntity
testLordWorldEntity =
    { army = testTroopList
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Jan von Haskell"
    }


testLord : Lord
testLord =
    { entity = testLordWorldEntity
    , gold = 250
    , land = [ testSetelement ]
    , agent = PathAgent.getAgent 6
    }



-- end STATIC TEST DATA
-- begin initialization


startGame : Int -> Model
startGame playerCount =
    initPlayers initialModel playerCount


initialModel : Model
initialModel =
    let
        map =
            MapGenerator.createMap
    in
    Model (Cons testLord []) (GameSetup MainMenue) Nothing (DateExt.Date 1017 DateExt.Jan) map


initPlayers : Model -> Int -> Model
initPlayers m count =
    let
        lords =
            List.map
                (\i -> initPlayer m.map i (2 * (toFloat i / toFloat count + 0.125)))
                (List.range 0 (count - 1))
    in
    { m | lords = Cons testLord lords }


initPlayer : Map.Map -> Int -> Float -> Lord
initPlayer m i rad =
    let
        entity =
            WorldEntity
                secondLordTroops
                (Faction.getFaction i)
                (Pathfinder.getClosestFreeFieldAt (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad)) (MapGenerator.getNav m) Dict.empty)
                ("Lord " ++ String.fromInt i)
    in
    Lord
        entity
        250
        (initSettlementsFor m Dict.empty entity i)
        (PathAgent.getAgent 5)


initSettlementsFor : Map.Map -> Dict.Dict Int () -> Entities.WorldEntity -> Int -> List Entities.Settlement
initSettlementsFor m usedFields e i =
    Entities.createCapitalFor e
        :: List.map
            Entities.getSettlementFor
            (getVillagesInQuadrant m e i villagesPerLord |> getSafeSettlementInfos m usedFields)


getVillagesInQuadrant : Map.Map -> Entities.WorldEntity -> Int -> Int -> List Entities.SettlementInfo
getVillagesInQuadrant m e q i =
    List.map
        (\index -> Entities.SettlementInfo Village (getVillagesPosition i q index e.position) e.faction)
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



-- end initialization
-- begin drawing


view : Model -> Html Msg
view model =
    let
        allClickActions =
            buildAllMapSvgs model
    in
    div [ Html.Attributes.class "page-container" ]
        [ findModalWindow model
        , Templates.HeaderTemplate.generateHeaderTemplate (Entities.getPlayer model.lords) model.date
        , div [ Html.Attributes.class "page-map" ]
            [ addStylesheet "link" "./assets/styles/main_styles.css"
            , generateMapActionTemplate model.selectedPoint allClickActions
            , div []
                [ Svg.svg
                    [ Svg.Attributes.viewBox "0 0 850 1000"
                    , Svg.Attributes.fill "none"
                    ]
                    (MapDrawer.allSvgs allClickActions)
                ]
            , span [] [ Html.text (gameStateToText model) ]
            ]
        ]


drawnMap : Map.Map -> MapDrawer.MapClickAction
drawnMap map =
    Map.drawMap map



-- Get the right modal-window by the current Model-Menue-State


findModalWindow : Model -> Html Msg
findModalWindow model =
    case model.gameState of
        GameSetup uistate ->
            case uistate of
                SettlementView l s u ->
                    generateSettlementModalTemplate l s u

                LordView l ->
                    generateLordTemplate l

                BattleView bS ->
                    generateBattleTemplate bS (Map.getTerrainForPoint bS.player.entity.position model.map)

                _ ->
                    div [] []

        GameOver bool ->
            generateEndTemplate bool

        _ ->
            div [] []


addStylesheet : String -> String -> Html Msg
addStylesheet tag href =
    Html.node tag [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" href ] []


gameStateToText : Model -> String
gameStateToText m =
    String.fromFloat (getPlayer m).gold



-- end drawing
--begin update


update : Msg -> Model -> Model
update msg model =
    case msg of
        EndRound ->
            { model | date = DateExt.addMonths 1 model.date, lords = Cons (endRoundForLord (getPlayer model)) (updateAIsAfterPlayerRound (npcs model.lords)) }

        EndGame bool ->
            { model | gameState = GameOver bool }

        CloseModal ->
            { model | gameState = GameSetup GameMenue }

        BattleAction bmsg ->
            updateBattle bmsg model

        SettlementAction action ->
            updateSettlement action model

        MapTileAction action ->
            updateMaptileAction model action

        Click p ->
            { model | selectedPoint = Just p }


updateAIsAfterPlayerRound : List Entities.Lord -> List Entities.Lord
updateAIsAfterPlayerRound lords =
    List.map (\l -> updateAI l |> endRoundForLord) lords


updateAI : Lord -> Lord
updateAI lord =
    { lord | entity = Entities.setPosition lord.entity (Vector.addPoints (Vector.Point 1 1) lord.entity.position) }


endRoundForLord : Lord -> Lord
endRoundForLord l =
    applyLordGoldIncome l |> Entities.resetUsedMovement


updateMaptileAction : Model -> MapTileMsg -> Model
updateMaptileAction model ma =
    case ma of
        Types.LordMsg msg lord ->
            updateLordAction msg lord model

        --{ model | gameState = GameSetup (LordView lord) }
        Types.SettlementMsg msg settlement ->
            { model | gameState = GameSetup (SettlementView (getPlayer model) settlement Types.StandardView) }

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
                    { model | lords = Cons newPlayer (npcs model.lords) }


updateLordAction : Types.LordTileMsg -> Entities.Lord -> Model -> Model
updateLordAction msg lord m =
    case msg of
        Types.ViewLord ->
            { m | gameState = GameSetup (LordView lord) }

        Types.EngageLord ->
            { m | gameState = GameSetup (BattleView { player = getPlayer m, enemy = lord, round = 1, playerCasualties = Troops.emptyTroops, enemyCasualties = Troops.emptyTroops, finished = False }) }


updateSettlement : SettlementMsg -> Model -> Model
updateSettlement msg model =
    case msg of
        UIMsg umsg ->
            updateSettlementUI umsg model

        TroopMsg tmsg ->
            updateSettlementStats tmsg model


updateSettlementUI : SettlementUIMsg -> Model -> Model
updateSettlementUI msg model =
    case msg of
        ShowBuyTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.RecruitView) }

        ShowStationTroops s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.StationView) }

        ShowSettlement s ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) s Types.StandardView) }


updateSettlementStats : SettlementArmyMsg -> Model -> Model
updateSettlementStats msg model =
    case msg of
        BuyTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.buyTroops (Entities.getPlayer model.lords) t))
                s
                Types.RecruitView
                model

        StationTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.stationTroops (Entities.getPlayer model.lords) t s))
                s
                Types.StationView
                model

        TakeTroops t s ->
            updateMultipleTroopStats
                (Entities.updatePlayer model.lords (Entities.takeTroops (Entities.getPlayer model.lords) t s))
                s
                Types.StationView
                model


updateMultipleTroopStats : LordList -> Settlement -> UiSettlementState -> Model -> Model
updateMultipleTroopStats l s u m =
    let
        newSettle =
            getSettlementByName (Entities.getPlayer l).land s.entity.name
    in
    case newSettle of
        Nothing ->
            m

        Just x ->
            { m
                | lords = l
                , gameState = GameSetup (SettlementView (Entities.getPlayer l) x u)
            }


updateBattle : BattleMsg -> Model -> Model
updateBattle msg model =
    case msg of
        StartBattle lord ->
            { model | gameState = GameSetup (BattleView { player = getPlayer model, enemy = lord, round = 1, playerCasualties = Troops.emptyTroops, enemyCasualties = Troops.emptyTroops, finished = False }) }

        StartSkirmish bS ->
            let
                newPlayer =
                    Battle.evaluateBattle bS.player bS.enemy.entity.army

                newEnemy =
                    Battle.evaluateBattle bS.enemy bS.player.entity.army
            in
            { model
                | gameState =
                    GameSetup
                        (BattleView
                            { bS
                                | round = bS.round + 1
                                , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
                                , enemyCasualties = List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army
                                , player = newPlayer
                                , enemy = newEnemy
                                , finished = Battle.sumTroops newPlayer.entity.army == 0 || Battle.sumTroops newEnemy.entity.army == 0
                            }
                        )
            }

        SkipSkirmishes bS ->
            { model | gameState = GameSetup (BattleView (skipBattle bS)) }

        FleeBattle bS ->
            let
                newEnemyLords =
                    OperatorExt.mapFilter (\_ -> bS.enemy) identity (\x -> x.entity.name == bS.enemy.entity.name) (tailLordList model.lords)

                newPlayer =
                    Entities.updatePlayerArmy bS.player (List.map (\x -> { x | amount = round (toFloat x.amount * 0.6) }) bS.player.entity.army)

                lords =
                    Cons newPlayer newEnemyLords
            in
            { model | lords = lords, gameState = GameSetup GameMenue }

        EndBattle bS ->
            let
                newEnemyLords =
                    OperatorExt.mapFilter (\_ -> bS.enemy) identity (\x -> x.entity.name == bS.enemy.entity.name) (tailLordList model.lords)

                lords =
                    Cons bS.player newEnemyLords
            in
            { model | lords = lords, gameState = GameSetup GameMenue }


skipBattle : BattleStats -> BattleStats
skipBattle bS =
    let
        newPlayer =
            Battle.evaluateBattle bS.player bS.enemy.entity.army

        newEnemy =
            Battle.evaluateBattle bS.enemy bS.player.entity.army

        playerCasualties =
            List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army

        enemyCasualties =
            List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army

        end =
            Battle.sumTroops newPlayer.entity.army == 0 || Battle.sumTroops newEnemy.entity.army == 0
    in
    if end then
        { bS | player = newPlayer, enemy = newEnemy, playerCasualties = playerCasualties, enemyCasualties = enemyCasualties, finished = True }

    else
        skipBattle { bS | player = newPlayer, enemy = newEnemy }



-- end update
-- begin config


villagesPerLord : Int
villagesPerLord =
    3


villageCaptialDistance : Float
villageCaptialDistance =
    7



-- end config


getPlayer : Model -> Entities.Lord
getPlayer model =
    Entities.getPlayer model.lords


main : Program () Model Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }
