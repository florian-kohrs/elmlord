module OperatorExt exposing (..)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


ternary : Bool -> a -> a -> a
ternary bool op1 op2 =
    if bool then
        op1

    else
        op2

