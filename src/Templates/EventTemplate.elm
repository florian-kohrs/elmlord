module Templates.EventTemplate exposing (..)

import Event
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msg
import OperatorExt



-- events component (top right component)
--------------------------------------------------------


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


generateEventComponent : Int -> Event.Event -> Html Msg.Msg
generateEventComponent index e =
    div [ Html.Attributes.class ("event-logs-component " ++ OperatorExt.ternary (e.eventType == Event.Important) "important-log" "minor-log") ]
        [ div [] [ span [] [ Html.text e.header ] ]
        , div [ onClick (Msg.EventAction (Msg.DeleteEvent index)), Html.Attributes.class "event-logs-close" ] [ Html.text "x" ]
        , div [] [ span [] [ Html.text e.text ] ]
        ]
