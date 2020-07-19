module Pathfinder exposing (..)

import Dict exposing (Dict)
import ListExt
import MapData
import MaybeExt
import Pathfinder.Model exposing (..)
import Vector



--negative x going up is bugged , psitive x giong down is bugged -> cant move on x axis


getClosestFreeFieldAt : Vector.Point -> NavigatableMap -> Dict Int () -> Vector.Point
getClosestFreeFieldAt p nav invalidDict =
    Maybe.withDefault (Vector.Point 0 0) (getClosestFreeFieldAt_ p p [] nav invalidDict Dict.empty)


getClosestFreeFieldAt_ :
    Vector.Point
    -> Vector.Point
    -> List Vector.Point
    -> NavigatableMap
    -> Dict Int ()
    -> Dict Int ()
    -> Maybe Vector.Point
getClosestFreeFieldAt_ start closest tails nav invalidFields usedFields =
    if MaybeExt.hasValue (nav.timeToCrossField closest) && not (Dict.member (MapData.hashMapPoint closest) invalidFields) then
        Just closest

    else
        let
            circumjacent =
                List.filter (\p -> not (Dict.member (MapData.hashMapPoint p) usedFields))
                    (nav.getCircumjacentFields closest True)

            tails2 =
                List.foldl (\p ts2 -> ListExt.insertToSortedList p (nav.getMinDistanceBetween start) ts2) tails circumjacent
        in
        case tails2 of
            [] ->
                Nothing

            x :: xs ->
                getClosestFreeFieldAt_ start x xs nav invalidFields (addCircumjacentToDict usedFields circumjacent)


addCircumjacentToDict : Dict Int () -> List Vector.Point -> Dict Int ()
addCircumjacentToDict =
    List.foldl (\p dict2 -> Dict.insert (MapData.hashMapPoint p) () dict2)


pathToPoints : Path -> List Vector.Point
pathToPoints path =
    List.map .indices path.path


pathPartToTile : PathPart -> PathTile
pathPartToTile (PathPart p) =
    PathTile p.position (pathTimeLoss (PathPart p))


pathTimeLoss : PathPart -> Float
pathTimeLoss (PathPart p) =
    MaybeExt.foldMaybe (\(PathPart parent) -> p.previousDistance - parent.previousDistance) 0 p.parent


createPathPart : Vector.Point -> Maybe PathPart -> PathInfo -> PathPart
createPathPart p parent info =
    PathPart
        { parent = parent
        , minDistanceToTarget = info.nav.getMinDistanceBetween p info.target
        , previousDistance =
            MaybeExt.foldMaybe
                (\(PathPart part) ->
                    Maybe.withDefault 9999 (info.nav.timeToCrossField p) + part.previousDistance
                )
                0
                parent
        , position = p
        }


toPath : PathPart -> List PathTile
toPath p =
    toPath_ p []


toPath_ : PathPart -> List PathTile -> List PathTile
toPath_ (PathPart p) ps =
    MaybeExt.foldMaybe
        (\parent -> toPath_ parent (pathPartToTile (PathPart p) :: ps))
        (pathPartToTile (PathPart p) :: ps)
        p.parent


totalDistance : PathPart -> Float
totalDistance (PathPart p) =
    p.previousDistance + p.minDistanceToTarget


getPath : Vector.Point -> PathInfo -> Maybe Path
getPath from info =
    buildPath (createPathPart from Nothing info)
        []
        (Dict.singleton (MapData.hashMapPoint from) ())
        info


cutFirstStepFromPath : Path -> Path
cutFirstStepFromPath p =
    Path p.target
        (Maybe.withDefault
            []
            (List.tail p.path)
        )


buildPath : PathPart -> PathTails -> PathTailLookup -> PathInfo -> Maybe Path
buildPath (PathPart closest) tails dict info =
    if Vector.pointEqual closest.position info.target then
        Just (Path info.target (toPath (PathPart closest)))

    else
        let
            circumjacent =
                List.filter (\p -> not (Dict.member (MapData.hashMapPoint p) dict))
                    (info.nav.getCircumjacentFields closest.position False)

            tails2 =
                List.foldl
                    (\p ts2 ->
                        addSortedPathTail ts2
                            (createPathPart p (Just (PathPart closest)) info)
                    )
                    tails
                    circumjacent
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
