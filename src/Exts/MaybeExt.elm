module MaybeExt exposing (foldMaybe, hasValue)


hasValue : Maybe a -> Bool
hasValue m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


foldMaybe : (a -> b) -> b -> Maybe a -> b
foldMaybe f n m =
    case m of
        Nothing ->
            n

        Just a ->
            f a
