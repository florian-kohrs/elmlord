module Lists exposing (indexedMap, repeat, snoc)


repeat : Int -> a -> List a
repeat i a =
    if i > 0 then
        a :: repeat (i - 1) a

    else
        []


snoc : List a -> a -> List a
snoc as1 a =
    case as1 of
        a2 :: as2 ->
            a2 :: snoc as2 a

        [] ->
            [ a ]


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap =
    indexedMap_ 0


indexedMap_ : Int -> (Int -> a -> b) -> List a -> List b
indexedMap_ i f xs =
    case xs of
        [] ->
            []

        x :: xs2 ->
            f i x :: indexedMap_ (i + 1) f xs2
