module Main exposing (..)

import Browser
import Entities exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapModel exposing (Map)
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
    | Click Vector


initialModel : Model
initialModel =
    Model [] GameSetup Nothing []


update : Msg -> Model -> Model
update msg model =
    case msg of
        EndRound ->
            model

        Click v ->
            model


view : Model -> Html Msg
view model =
    div []
        []


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
