module OperatorExt exposing (..)


ternary : Bool -> a -> a -> a
ternary bool op1 op2 =
    if bool then
        op1
    else 
        op2


mapFilter : (a -> b) -> (a -> b) -> (a -> Bool) -> List a -> List b
mapFilter func esc fil l =
    case l of
        [] -> 
            []
        
        (x :: xs) -> 
            ternary (fil x) (func x :: (mapFilter func esc fil xs)) (esc x :: (mapFilter func esc fil xs))

const : a -> a 
const a =
    a