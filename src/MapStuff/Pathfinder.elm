module Pathfinder exposing (..)

import Dict exposing (Dict)
import MapData
import MaybeExt
import Vector



--negative x going up is bugged , psitive x giong down is bugged -> cant move on x axis


type PathPart
    = PathPart
        { parent : Maybe PathPart
        , minDistanceToTarget : Float
        , previousDistance : Float
        , position : Vector.Point
        }


type alias Path =
    { target : Vector.Point
    , path : List PathTile
    }


type alias PathTile =
    { indices : Vector.Point
    , timeLoss : Float
    }


pathToPoints : Path -> List Vector.Point
pathToPoints path =
    List.map .indices path.path


pathPartToTile : PathPart -> PathTile
pathPartToTile (PathPart p) =
    PathTile p.position (pathTimeLoss (PathPart p))


pathTimeLoss : PathPart -> Float
pathTimeLoss (PathPart p) =
    MaybeExt.foldMaybe (\(PathPart parent) -> p.previousDistance - parent.previousDistance) 0 p.parent


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
            MaybeExt.foldMaybe
                (\(PathPart part) -> part.previousDistance + info.nav.timeToCrossField p)
                0
                parent
        , position = p
        }


toPath : PathPart -> List PathTile
toPath p =
    toPath_ p []


toPath_ : PathPart -> List PathTile -> List PathTile
toPath_ (PathPart p) ps =
    case p.parent of
        Nothing ->
            pathPartToTile (PathPart p) :: ps

        Just parent ->
            toPath_ parent (pathPartToTile (PathPart p) :: ps)


totalDistance : PathPart -> Float
totalDistance (PathPart p) =
    p.previousDistance + p.minDistanceToTarget


getPath : Vector.Point -> PathInfo -> Maybe Path
getPath from info =
    buildPath (createPathPart from Nothing info) [] (Dict.singleton (MapData.hashMapPoint from) ()) info


buildPath : PathPart -> PathTails -> PathTailLookup -> PathInfo -> Maybe Path
buildPath (PathPart closest) tails dict info =
    if Vector.pointEqual closest.position info.target then
        Just (Path info.target (toPath (PathPart closest)))

    else
        let
            circumjacent =
                List.filter (\p -> not (Dict.member (MapData.hashMapPoint p) dict))
                    (info.nav.getCircumjacentFields closest.position)

            tails2 =
                List.foldl (\p ts2 -> addSortedPathTail ts2 (createPathPart p (Just (PathPart closest)) info)) tails circumjacent
        in
        case closestPath tails2 dict of
            ( _, Nothing ) ->
                Nothing

            ( newTails, Just (PathPart newClosest) ) ->
                buildPath
                    (PathPart newClosest)
                    tails2
                    (Dict.insert (MapData.hashMapPoint newClosest.position) () dict)
                    info


type alias PathTails =
    List PathPart


closestPath : PathTails -> PathTailLookup -> ( PathTails, Maybe PathPart )
closestPath tails dict =
    case tails of
        [] ->
            ( [], Nothing )

        (PathPart p) :: ps ->
            if Dict.member (MapData.hashMapPoint p.position) dict then
                closestPath ps dict

            else
                ( ps, Just (PathPart p) )


type alias PathTailLookup =
    Dict Int ()


addSortedPathTail : PathTails -> PathPart -> PathTails
addSortedPathTail tails p =
    case tails of
        [] ->
            [ p ]

        t :: ts ->
            if totalDistance p <= totalDistance t then
                p :: t :: ts

            else
                t :: addSortedPathTail ts p
