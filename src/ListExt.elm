module ListExt exposing (indexOf)


indexOf : (a -> Bool) -> List a -> Int
indexOf p xs =
    indexOf_ 0 p xs


indexOf_ : Int -> (a -> Bool) -> List a -> Int
indexOf_ i p xs =
    case xs of
        [] ->
            -1

        x :: xs2 ->
            if p x then
                i

            else
                indexOf_ (i + 1) p xs2