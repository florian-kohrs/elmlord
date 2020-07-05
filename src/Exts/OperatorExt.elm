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

        x :: xs ->
            if fil x then
                func x :: mapFilter func esc fil xs
            else 
                esc x :: mapFilter func esc fil xs


{- hashMultiplicator : Int
   hashMultiplicator =
       31


   hashModulo : Int
   hashModulo =
       79119


   createSimplePseudoSeed : List Char -> Int
   createSimplePseudoSeed l =
       modBy (List.foldr (\x y -> (Char.toCode x * hashMultiplicator) + y) 0 l) hashModulo


   getPseudoRandomElement : Random.Seed -> List a -> Maybe a
   getPseudoRandomElement s l =
       let
           ( x, _ ) =
               Random.step (Random.int 0 (List.length l - 1)) s
       in
       case l of
           [] ->
               Nothing

           v ->
               getIndexedElement v x


   getIndexedElement : List a -> Int -> Maybe a
   getIndexedElement l i =
       case l of
           [] ->
               Nothing

           x :: xs ->
               if i > 0 then
                   getIndexedElement xs (i - 1)

               else
                   Just x
-}