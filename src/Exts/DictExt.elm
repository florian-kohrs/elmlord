module DictExt exposing (foldlOverKeys, mergeKeys)

import Dict


mergeKeys :
    (comparable -> v -> r -> r)
    -> (comparable -> v -> v -> r -> r)
    -> (comparable -> v -> r -> r)
    -> (comparable -> r -> r)
    -> Dict.Dict comparable v
    -> Dict.Dict comparable v
    -> List comparable
    -> r
    -> r
mergeKeys f1 f2 f3 f4 d1 d2 keys r =
    case keys of
        [] ->
            r

        k :: ks ->
            case ( Dict.get k d1, Dict.get k d2 ) of
                ( Nothing, Nothing ) ->
                    mergeKeys f1 f2 f3 f4 d1 d2 ks (f4 k r)

                ( Just v1, Nothing ) ->
                    mergeKeys f1 f2 f3 f4 d1 d2 ks (f1 k v1 r)

                ( Nothing, Just v2 ) ->
                    mergeKeys f1 f2 f3 f4 d1 d2 ks (f3 k v2 r)

                ( Just v1, Just v2 ) ->
                    mergeKeys f1 f2 f3 f4 d1 d2 ks (f2 k v1 v2 r)


foldlOverKeys :
    (comparable -> v -> r -> r)
    -> (comparable -> r -> r)
    -> r
    -> Dict.Dict comparable v
    -> List comparable
    -> r
foldlOverKeys f1 f2 e d =
    List.foldl
        (\k r ->
            case Dict.get k d of
                Nothing ->
                    f2 k r

                Just v ->
                    f1 k v r
        )
        e
