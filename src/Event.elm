module Event exposing (..)


type alias EventState = 
    {
        state: Bool
        , events: List Event
    }

type EventType
    = Important
    | Minor


type alias Event =
    { index : Int
    , header : String
    , text : String
    , eventType : EventType
    }

appendEvent : List Event -> String -> String -> EventType -> List Event
appendEvent l h t e = 
    {index = List.length l, header = h, text = t, eventType = e} :: l

removeEvent : EventState -> Int -> EventState 
removeEvent e i =
    {e | events = List.filter (\x -> x.index /= i) e.events}

updateEventState : EventState -> EventState
updateEventState e = 
    {e | state = not e.state}

clearEvents : EventState -> EventState
clearEvents e = 
    {e | events = []}