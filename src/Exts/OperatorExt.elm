module OperatorExt exposing (..)


ternary : Bool -> a -> a -> a
ternary bool op1 op2 =
    if bool then
        op1
    else 
        op2

