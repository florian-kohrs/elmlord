module Templates.EventTemplate exposing (..)

import Event
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MapDrawer
import Types
import Vector


{-| Returns the layout for the map actions, in dependence to the chosen point (in the model)

    @param {Maybe Point}: Takes the point that is currently chosen point (at the start no point is chosen, therefore Maybe)
    @param {MapDrawer.MapClickAction}: Takes a dict with all possible actions

-}
generateEventOverview : List Event.Event -> Html Types.Msg
generateEventOverview l =
    div [ Html.Attributes.class "event-logs" ]
        [ div [ Html.Attributes.class "event-logs-header" ]
            [ span [] [ Html.text "Events" ]
            ]
        , div [ Html.Attributes.class "event-logs-body" ] []
        ]


{-| Function for map, that displays a button for each possible action

    @param {Types.MapTileMsg}: Takes the action type, that the button sends, when it gets clicked

-}
generateEventComponent : Event.Event -> Html Types.Msg
generateEventComponent e =
    div [] [
        div [] [Html.text e.header] 
        div [] [Html.text x]
        div [] [Html.text e.text]
    ]
