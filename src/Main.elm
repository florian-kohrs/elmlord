module Main exposing (..)

import Browser
import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapGenerator exposing (createMap, getXPosForIndex, getYPosForIndex, hexRadius)
import MapModel exposing (Map, MapTile)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


type alias Model =
    { lords : List Lord
    , gameState : GameState
    , selectedIndex : Maybe Point
    , map : Map
    }


type GameState
    = GameSetup
    | InGame
    | GameOver Bool -- true = gewonnen, false = verloren



--todo : Modell überarbeiten, map generierung anschauen -> pathfinding?


type Msg
    = EndRound
    | Click Point


initialModel : Model
initialModel =
    Model [] GameSetup Nothing MapGenerator.createMap


update : Msg -> Model -> Model
update msg model =
    case msg of
        EndRound ->
            model

        Click p ->
            { model | selectedIndex = Just p }


view : Model -> Html Msg
view model =
    div
        []
        ([ div [ Html.Attributes.style "height" "800", Html.Attributes.style "width" "1000px" ]
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
        )


pointToMsg : Vector.Point -> Msg
pointToMsg p =
    Click p


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
