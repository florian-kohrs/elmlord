module Main exposing (..)

import Browser
import Dict
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Map exposing (Map, MapTile)
import MapData exposing (..)
import MapGenerator exposing (createMap)
import MaybeExt
import Pathfinder
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


type alias Model =
    { lords : List Lord
    , gameState : GameState
    , selectedIndex : Maybe Point
    , selectedIndex2 : Maybe Point
    , map : Map
    }


type GameState
    = GameSetup UiState
    | InGame Int -- int = playerCount
    | GameOver Bool -- true = gewonnen, false = verloren


type UiState
    = MainMenue
    | SaveLoad
    | NewCampain



--todo : Modell überarbeiten, map generierung anschauen -> pathfinding?


type Msg
    = EndRound
    | Click Point


initialModel : Model
initialModel =
    Model [] (GameSetup MainMenue) Nothing Nothing MapGenerator.createMap


startGame : Int -> Model
startGame playerCount =
    initMap (initPlayers initialModel playerCount)


initPlayers : Model -> Int -> Model
initPlayers m count =
    let
        lords =
            List.map
                (\i -> initPlayer i (2 * (toFloat i / toFloat count + 0.125)))
                (List.range 0 (count - 1))
    in
    { m | lords = lords }


initMap : Model -> Model
initMap m =
    initMapWithSettlements (List.concat (List.map (\l -> l.land) m.lords)) (initMapWithLords m.lords m)


initMapWithLords : List Entities.Lord -> Model -> Model
initMapWithLords ls m =
    let
        map =
            List.foldl
                (\l ->
                    Dict.update (Vector.showPoint l.entity.position) (Maybe.andThen (\t -> Just { t | lords = l :: t.lords }))
                )
                m.map
                ls
    in
    { m | map = map }


initMapWithSettlements : List Entities.Settlement -> Model -> Model
initMapWithSettlements setts m =
    let
        map =
            List.foldl
                (\s ->
                    Dict.update (Vector.showPoint s.entity.position) (Maybe.andThen (\t -> Just { t | settlement = Just s }))
                )
                m.map
                setts
    in
    { m | map = map }


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
        Wait
        (initSettlementsFor entity)
        0


initSettlementsFor : Entities.WorldEntity -> List Entities.Settlement
initSettlementsFor e =
    Entities.createCapitalFor e :: []


moveLord : Entities.Lord -> Vector.Point -> Model -> Model
moveLord l newP m =
    let
        newLords =
            List.map
                (\lord ->
                    if lord == l then
                        { lord | entity = Entities.setPosition lord.entity newP }

                    else
                        lord
                )
                m.lords

        newMap =
            Map.moveLord l newP m.map
    in
    { m | lords = newLords, map = newMap }



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
            case model.selectedIndex of
                Nothing ->
                    { model | selectedIndex = Just p }

                Just _ ->
                    { model | selectedIndex2 = model.selectedIndex, selectedIndex = Just p }


view : Model -> Html Msg
view model =
    let
        body f =
            [ div [ Html.Attributes.style "height" "800", Html.Attributes.style "width" "1000px" ]
                [ addStylesheet "link" "./assets/styles/main_styles.css"
                   , Svg.svg
                    [ Svg.Attributes.viewBox "0 0 2000 1800"
                    , Svg.Attributes.width "2000"
                    , Svg.Attributes.height "1800"
                    , Svg.Attributes.fill "none"
                    ]
                    (f model.map MapData.hexRadius Click)
                ]
            ]
                ++ MaybeExt.foldMaybe (\p -> [ span [] [ Html.text (Vector.showPoint p) ] ]) [] model.selectedIndex
                ++ List.foldl (\l r -> span [] [ Html.text (Vector.showPoint l.entity.position) ] :: r) [] model.lords
    in
    case model.selectedIndex of
        Nothing ->
            div
                [Html.Attributes.class "page-container"]
                (body Map.mapToSvg)

        Just s1 ->
            case model.selectedIndex2 of
                Nothing ->
                    div
                        [Html.Attributes.class "page-container"]
                        (body Map.mapToSvg)

                Just s2 ->
                    let
                        path =
                            Pathfinder.getPath s1 (Pathfinder.PathInfo (MapGenerator.getNav model.map) s2)
                    in
                    div
                        [Html.Attributes.class "page-container"]
                        (body (Map.mapWithPathToSvg path)
                            ++ [ span [] [ Html.text (Vector.showPoint s2) ]
                               , span []
                                    [ Html.text
                                        (List.foldl
                                            (\c r -> r ++ Vector.showPoint c)
                                            "Path: "
                                            path
                                        )
                                    ]
                               ]
                        )


pointToMsg : Vector.Point -> Msg
pointToMsg p =
    Click p


main : Program () Model Msg
main =
    Browser.sandbox { init = startGame 4, view = view, update = update }


-- auslagern, konnte nicht gemacht werden, weil Msg in Templates benötigt wird xd
addStylesheet : String -> String -> Html Msg
addStylesheet tag href = 
    Html.node tag [ attribute "Rel" "stylesheet", attribute "property" "stylesheet", attribute "href" href] []