module Templates.Helpers exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing ( Msg(..), UiSettlementState(..))
import Troops exposing (..)


troopsToIntList : Troop ->  Int
troopsToIntList troop =
            troop.amount