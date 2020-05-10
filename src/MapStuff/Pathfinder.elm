module Pathfinder exposing (..)

import Dict exposing (Dict)
import Vector



--negative x going up is bugged , psitive x giong down is bugged -> cant move on x axis


type PathPart
    = PathPart
        { parent : Maybe PathPart
        , minDistanceToTarget : Float
        , previousDistance : Float
        , position : Vector.Point
        }


type alias PathInfo =
    { nav : NavigatableMap, target : Vector.Point }


type alias NavigatableMap =
    { timeToCrossField : Vector.Point -> Float, getCircumjacentFields : Vector.Point -> List Vector.Point, getMinDistanceBetween : Vector.Point -> Vector.Point -> Float }


createPathPart : Vector.Point -> Maybe PathPart -> PathInfo -> PathPart
createPathPart p parent info =
    PathPart
        { parent = parent
        , minDistanceToTarget = info.nav.getMinDistanceBetween p info.target
        , previousDistance =
            Maybe.withDefault 0
                (Maybe.andThen
                    (\(PathPart part) -> Just (part.previousDistance + info.nav.timeToCrossField p))
                    parent
                )
        , position = p
        }


toPath1 : PathPart -> List Vector.Point
toPath1 p =
    List.reverse (toPath1_ p)


toPath1_ : PathPart -> List Vector.Point
toPath1_ (PathPart p) =
    List.reverse (p.position :: Maybe.withDefault [] (Maybe.andThen (\(PathPart x) -> Just (toPath1 (PathPart x))) p.parent))


toPath2 : PathPart -> List Vector.Point
toPath2 p =
    toPath2_ p []


toPath2_ : PathPart -> List Vector.Point -> List Vector.Point
toPath2_ (PathPart p) ps =
    case p.parent of
        Nothing ->
            p.position :: ps

        Just parent ->
            toPath2_ parent (p.position :: ps)


minDistance : PathPart -> Float
minDistance (PathPart p) =
    p.previousDistance + p.minDistanceToTarget


getPath : Vector.Point -> PathInfo -> List Vector.Point
getPath from info =
    buildPath [ createPathPart from Nothing info ] (Dict.singleton (Vector.showPoint from) ()) info


buildPath : PathTails -> PathTailLookup -> PathInfo -> List Vector.Point
buildPath tails dict info =
    case tails of
        [] ->
            []

        (PathPart closest) :: ts ->
            --info.nav.getCircumjacentFields closest.position
            if Vector.pointEqual closest.position info.target then
                toPath2 (PathPart closest)

            else
                let
                    circumjacent =
                        List.filter (\p -> not (Dict.member (Vector.showPoint p) dict))
                            (info.nav.getCircumjacentFields closest.position)
                in
                buildPath
                    (List.foldl (\p ts2 -> addSortedPathTail ts2 (createPathPart p (Just (PathPart closest)) info)) ts circumjacent)
                    (List.foldl (\p dict2 -> Dict.insert (Vector.showPoint p) () dict2) dict circumjacent)
                    info


type alias PathTails =
    List PathPart


type alias PathTailLookup =
    Dict String ()


addSortedPathTail : PathTails -> PathPart -> PathTails
addSortedPathTail tails p =
    case tails of
        [] ->
            [ p ]

        t :: ts ->
            if minDistance p <= minDistance t then
                p :: t :: ts

            else
                t :: addSortedPathTail ts p
