module Main exposing (..)

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
import Types exposing (MapTileMsg(..), Msg(..), SettlementMsg(..), SettlementUIMsg(..), SettlementArmyMsg(..), BattleMsg(..), UiSettlementState(..))
import Vector exposing (..)
import Battle


type alias Model =
    { lords : LordList
    , gameState : GameState
    , selectedPoint : Maybe Point
    , date : DateExt.Date
    , map : Map.Map --used for pathfinding
    }


type GameState
    = GameSetup UiState
    | InGame Int -- int = playerCount
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
                (EntitiesDrawer.drawSettlement testLord)
                (List.foldl (EntitiesDrawer.drawLord testLord) (drawnMap m.map) (Entities.flattenLordList m.lords))
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
        player = Entities.getPlayer m.lords
    in
    case getSelectedPath m of
        Nothing ->
            mapDict

        Just path ->
            PathDrawer.drawPath player.moveSpeed path mapDict


getSelectedPath : Model -> Maybe Pathfinder.Path
getSelectedPath m =
    let
        player = Entities.getPlayer m.lords
    in
    case m.selectedPoint of
        Nothing ->
            Nothing

        Just point ->
            if canMoveToPoint (drawnMap m.map) point then
                Pathfinder.getPath
                    player.entity.position
                    (Pathfinder.PathInfo (MapGenerator.getNav m.map) point)

            else
                Nothing


allSettlements : Model -> List Settlement
allSettlements m =
    List.concat (List.map .land (Entities.flattenLordList m.lords))



-- STATIC TEST DATA


testTroopList : List Troop
testTroopList =
    [ { amount = 30, troopType = Troops.Sword }, { amount = 30, troopType = Troops.Spear }, { amount = 30, troopType = Troops.Archer }, { amount = 30, troopType = Troops.Knight } ]


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
    , name = "Sir Quicknuss"
    }


testActionType : Action
testActionType =
    { actionType = Wait
    , actionMotive = Flee
    }


testLord : Lord
testLord =
    { entity = testLordWorldEntity
    , gold = 250
    , action = testActionType
    , land = [ testSetelement ]
    , moveSpeed = 1.0
    }



-- STATIC TEST DATA --


initialModel : Model
initialModel =
    let
        map =
            MapGenerator.createMap
    in
    Model (Cons testLord []) (GameSetup MainMenue) Nothing (DateExt.Date 1017 DateExt.Jan) map


startGame : Int -> Model
startGame playerCount =
    initPlayers initialModel playerCount


initPlayers : Model -> Int -> Model
initPlayers m count =
    let
        lords =
            List.map
                (\i -> initPlayer i (2 * (toFloat i / toFloat count + 0.125)))
                (List.range 0 (count - 1))
    in
    { m | lords = Cons testLord lords }


drawnMap : Map.Map -> MapDrawer.MapClickAction
drawnMap map =
    Map.drawMap map


initPlayer : Int -> Float -> Lord
initPlayer i rad =
    let
        entity =
            WorldEntity
                secondLordTroops
                (Faction.getFaction i)
                (Vector.toPoint (Vector.pointOnCircle (toFloat MapData.mapSize * 1) rad))
                ("Lord " ++ String.fromInt i)
    in
    Lord
        entity
        250
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


updateLordsAfterRound : LordList -> LordList
updateLordsAfterRound lords =
    Entities.mapLordList Entities.updateLordOnRoundEnd lords


update : Msg -> Model -> Model
update msg model =
    case msg of
        EndRound ->
            { model | date = DateExt.addMonths 1 model.date, lords = updateLordsAfterRound model.lords }

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


updateMaptileAction : Model -> MapTileMsg -> Model
updateMaptileAction model ma =
    case ma of
        LordMsg msg lord ->
            { model | gameState = GameSetup (LordView lord) }

        SettlementMsg msg settlement ->
            { model | gameState = GameSetup (SettlementView (Entities.getPlayer model.lords) settlement Types.StandardView) }

        MoveTo _ ->
            model


updateSettlement : SettlementMsg -> Model -> Model
updateSettlement msg model =
    case msg of
        (UIMsg umsg) -> 
            updateSettlementUI umsg model

        (TroopMsg tmsg) -> 
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

-- will be refactored
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
   

        TakeTroops t s->
            updateMultipleTroopStats 
                (Entities.updatePlayer model.lords (Entities.takeTroops (Entities.getPlayer model.lords) t s))
                s
                Types.StationView
                model


updateMultipleTroopStats : LordList -> Settlement -> UiSettlementState -> Model -> Model
updateMultipleTroopStats l s u m =
                    let 
                        newSettle = getSettlementByName (getPlayer l).land s.entity.name
                    in
                        case newSettle of
                            Nothing ->
                                m

                            (Just x) -> 
                                { m | lords = l, 
                                        gameState = GameSetup (SettlementView (Entities.getPlayer l) x u) }



updateBattle : BattleMsg -> Model -> Model
updateBattle msg model =
    case msg of
        StartBattle str ->
            let 
                enemyLord = getLordByName model.lords str 
            in
                case enemyLord of
                    Nothing ->
                        model

                    (Just x) -> 
                        { model | gameState = GameSetup (BattleView  {player = getPlayer model.lords, enemy = x, round = 1, playerCasualties = Troops.emptyTroops, enemyCasualties = Troops.emptyTroops})}

        StartSkirmish bS -> 
            let
                newPlayer = Battle.evaluateBattle bS.player bS.enemy.entity.army
                newEnemy = Battle.evaluateBattle bS.enemy bS.player.entity.army
            in
                { model | gameState = GameSetup (BattleView  {bS | 
                                                                round = bS.round + 1
                                                                , playerCasualties = List.map2 Troops.troopDifferences bS.player.entity.army newPlayer.entity.army
                                                                , enemyCasualties = List.map2 Troops.troopDifferences bS.enemy.entity.army newEnemy.entity.army
                                                                , player = newPlayer
                                                                , enemy = newEnemy
                                                                })
                }

        SkipSkirmishes bS -> 
            { model | gameState = GameSetup (BattleView  {bS | round = 0})}

        FleeBattle -> 
            { model | gameState = GameSetup GameMenue }

        EndBattle -> 
            { model | gameState = GameSetup GameMenue }
            
            
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



-- temp to test


gameStateToText : Model -> String
gameStateToText gs =
    String.fromFloat (getPlayer gs.lords).gold



--temp


findModalWindow : Model -> Html Msg
findModalWindow model =
    case model.gameState of
        GameSetup uistate ->
            case uistate of
                SettlementView l s u ->
                    generateSettlementModalTemplate l s u

                LordView l ->
                    generateLordTemplate l

                {- case sView of
                   BuildingView ->
                       div [] []

                   _ ->
                       generateSettlementModalTemplate testLord testSetelement sView
                -}
                BattleView bS->
                    generateBattleTemplate bS

                _ ->
                    div [] []

        GameOver bool ->
            generateEndTemplate bool

        _ ->
            div [] []


pointToMsg : Vector.Point -> Msg
pointToMsg p =
    Click p


main : Program () Model Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }


addStylesheet : String -> String -> Html Msg
addStylesheet tag href =
    Html.node tag [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" href ] []

-- testLordData For Battlesimulation

firstLordTroops : List Troop
firstLordTroops =
    [ { amount = 30, troopType = Troops.Sword }, { amount = 30, troopType = Troops.Spear }, { amount = 30, troopType = Troops.Archer }, { amount = 30, troopType = Troops.Knight } ]


secondLordTroops : List Troop
secondLordTroops =
    [ { amount = 20, troopType = Troops.Sword }, { amount = 45, troopType = Troops.Spear }, { amount = 10, troopType = Troops.Archer }, { amount = 5, troopType = Troops.Knight } ]

fristLordEntity : WorldEntity
fristLordEntity =
    { army = firstLordTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Malaca"
    }


secondLordEntity : WorldEntity
secondLordEntity =
    { army = secondLordTroops
    , faction = Faction.Faction1
    , position = { x = 0, y = 0 }
    , name = "Sir Quicknuss"
    }


fristLord : Lord
fristLord =
    { entity = fristLordEntity
    , gold = 250
    , action = testActionType
    , land = []
    , moveSpeed = 1.0
    }


secondLord : Lord
secondLord =
    { entity = secondLordEntity
    , gold = 250
    , action = testActionType
    , land = []
    , moveSpeed = 1.0
    }