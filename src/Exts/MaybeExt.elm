module MaybeExt exposing (foldMaybe)


foldMaybe : (a -> b) -> b -> Maybe a -> b
foldMaybe f n m =
    case m of
        Nothing ->
            n

        Just a ->
            f a
