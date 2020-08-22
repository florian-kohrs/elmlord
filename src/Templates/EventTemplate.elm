module Templates.EventTemplate exposing (..)

import Event
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msg
import OperatorExt


{-| Returns the layout for the map actions, in dependence to the chosen point (in the model)

    @param {Maybe Point}: Takes the point that is currently chosen point (at the start no point is chosen, therefore Maybe)
    @param {MapDrawer.InteractableMapSVG}: Takes a dict with all possible actions

-}
generateEventOverview : Event.EventState -> Html Msg.Msg
generateEventOverview event =
    if event.state then
        div [ Html.Attributes.class "event-logs box-shadow" ]
            [ div [ Html.Attributes.class "event-logs-header" ]
                [ span [] [ Html.text "Events" ]
                ]
            , div []
                [ button [ onClick (Msg.EventAction Msg.ClearEvents) ] [ Html.text "Clear events" ]
                ]
            , div [ Html.Attributes.class "event-logs-body" ] (Tuple.first (List.foldr (\e ( r, i ) -> ( generateEventComponent i e :: r, i + 1 )) ( [], 0 ) event.events))
            ]

    else
        div [] []


{-| Function for map, that displays a button for each possible action

    @param {Types.MapTileMsg}: Takes the action type, that the button sends, when it gets clicked

-}
generateEventComponent : Int -> Event.Event -> Html Msg.Msg
generateEventComponent index e =
    div [ Html.Attributes.class ("event-logs-component " ++ OperatorExt.ternary (e.eventType == Event.Important) "important-log" "minor-log") ]
        [ div [] [ span [] [ Html.text e.header ] ]
        , div [ onClick (Msg.EventAction (Msg.DeleteEvent index)), Html.Attributes.class "event-logs-close" ] [ Html.text "x" ]
        , div [] [ span [] [ Html.text e.text ] ]
        ]
