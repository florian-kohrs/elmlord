module Event exposing (..)


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

removeEvent : List Event -> Int -> List Event 
removeEvent l i =
    List.filter (\x -> x.index /= i) l