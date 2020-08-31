module Event exposing (..)

import ListExt


type alias EventState =
    { state : Bool
    , events : List Event
    }


type EventType
    = Important
    | Minor


type alias Event =
    { header : String
    , text : String
    , eventType : EventType
    }


setEvents : EventState -> List Event -> EventState
setEvents eventState eventList =
    { eventState | events = eventList }


appendEvent : EventState -> Event -> EventState
appendEvent es e =
    { es | events = e :: es.events }


removeEvent : EventState -> Int -> EventState
removeEvent e i =
    { e | events = ListExt.removeElementAt i e.events }


updateEventState : EventState -> EventState
updateEventState e =
    { e | state = not e.state }


clearEvents : EventState -> EventState
clearEvents e =
    { e | events = [] }
