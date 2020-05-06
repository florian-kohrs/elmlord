module Main exposing (..)

import Browser
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapGenerator exposing (createMap, getXPosForIndex, getYPosForIndex, hexRadius)
import MapModel exposing (Map, MapTile)
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
    = GameSetup
    | InGame Int -- int = playerCount
    | GameOver Bool -- true = gewonnen, false = verloren



--todo : Modell überarbeiten, map generierung anschauen -> pathfinding?


type Msg
    = EndRound
    | Click Point


initialModel : Model
initialModel =
    Model [] GameSetup Nothing Nothing MapGenerator.createMap



{-
   startGame : Int -> Model
   startGame playerCount =
       initPlayers playerCount initialModel


   initPlayers : Model -> Int -> Model
   initPlayers m count =
       List.map
           (\i ->
               Lord
                   (WorldEntity []
                       (Faction.getFaction i)
                       (Vector.toPoint (Vector.pointOnCircle (MapGenerator.mapSize * 0.85) (2 * m / count)))
                       ("Lord" ++ String.fromInt i)
                   )
                   0
                   Wait
           )
           (List.range 0 (count - 1))
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
                    { model | selectedIndex2 = Just p }


view : Model -> Html Msg
view model =
    let
        body =
            [ div [ Html.Attributes.style "height" "800", Html.Attributes.style "width" "1000px" ]
                [ Svg.svg
                    [ Svg.Attributes.viewBox "0 0 2000 1800"
                    , Svg.Attributes.width "2000"
                    , Svg.Attributes.height "1800"
                    , Svg.Attributes.fill "none"
                    ]
                    (MapModel.mapToSvg model.map MapGenerator.hexRadius Click)
                ]
            ]
                ++ Maybe.withDefault
                    []
                    (Maybe.andThen
                        (\p -> Just [ span [] [ Html.text (Vector.showPoint p) ] ])
                        model.selectedIndex
                    )
    in
    case model.selectedIndex of
        Nothing ->
            div
                []
                body

        Just s1 ->
            case model.selectedIndex2 of
                Nothing ->
                    div
                        []
                        body

                Just s2 ->
                    div
                        []
                        (body
                            ++ [ span []
                                    [ Html.text
                                        (List.foldl
                                            (\c r -> r ++ Vector.showPoint c)
                                            "Path: "
                                            (Pathfinder.getPath s1 (Pathfinder.PathInfo MapGenerator.getNav s2))
                                        )
                                    ]
                               ]
                        )


pointToMsg : Vector.Point -> Msg
pointToMsg p =
    Click p


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
