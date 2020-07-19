module PathAgent exposing (getAgent, moveAlongPath, pathPartsToTime, remainingMovement, resetLordUsedMovement, resetUsedMovement, setUsedMovement, simulateDistance)

import Dict exposing (Dict)
import Entities
import Entities.Model
import Map.Model
import MapData
import MaybeExt
import PathAgent.Model exposing (..)
import Pathfinder
import Pathfinder.Model
import Vector


moveLordOnPath : Map.Model.Map -> Entities.Model.Lord -> Vector.Point -> Entities.Model.Lord
moveLordOnPath map l target =
    case Pathfinder.getPathTo l.entity.position target map of
        Nothing ->
            l

        Just path ->
            let
                ( usedMove, point ) =
                    moveAlongPath path l.entity.position l.agent
            in
            { l
                | agent = setUsedMovement usedMove l.agent
                , entity = Entities.setPosition l.entity point
            }


remainingMovement : Agent -> Float
remainingMovement a =
    a.speed - a.usedMovement


resetLordUsedMovement : Entities.Model.Lord -> Entities.Model.Lord
resetLordUsedMovement l =
    { l | agent = resetUsedMovement l.agent }


resetUsedMovement : Agent -> Agent
resetUsedMovement a =
    { a | usedMovement = 0 }


setUsedMovement : Float -> Agent -> Agent
setUsedMovement f a =
    { a | usedMovement = f }


type alias MoveSimulator =
    { turn : Int, turnUsedMove : Float, maxMove : Float }


newMoveSimulator : Float -> Float -> MoveSimulator
newMoveSimulator move usedMove =
    { turn = 1, turnUsedMove = usedMove, maxMove = move }


moveSimulatorFromAgent : Agent -> MoveSimulator
moveSimulatorFromAgent a =
    newMoveSimulator a.speed a.usedMovement


pathPartsToTime : Agent -> List Pathfinder.Model.PathTile -> List ( Pathfinder.Model.PathTile, Int )
pathPartsToTime a ts =
    Tuple.first
        (List.foldl
            (\t ( r, sim ) ->
                let
                    simulator =
                        simulateDistance t.timeLoss sim
                in
                ( ( t, simulator.turn ) :: r, simulator )
            )
            ( [], moveSimulatorFromAgent a )
            ts
        )


roundsToFinishPath : Agent -> List Pathfinder.Model.PathTile -> Int
roundsToFinishPath a ps =
    case List.head (List.reverse (pathPartsToTime a ps)) of
        Nothing ->
            0

        Just t ->
            Tuple.second t


getAgent : Float -> Agent
getAgent speed =
    { target = Nothing, speed = speed, usedMovement = 0.0 }


simulateDistance : Float -> MoveSimulator -> MoveSimulator
simulateDistance f sim =
    if canReachInRound sim.maxMove sim.turnUsedMove f then
        { sim | turnUsedMove = sim.turnUsedMove + f }

    else
        { sim | turn = sim.turn + 1, turnUsedMove = f }


moveAlongPath : Pathfinder.Model.Path -> Vector.Point -> Agent -> ( Float, Vector.Point )
moveAlongPath p start a =
    followPath (List.reverse (pathPartsToTime a p.path)) ( a.usedMovement, start )


followPath : List ( Pathfinder.Model.PathTile, Int ) -> ( Float, Vector.Point ) -> ( Float, Vector.Point )
followPath l ( used, p ) =
    case l of
        [] ->
            ( used, p )

        ( t, turn ) :: ts ->
            if turn > 1 then
                ( used, p )

            else
                followPath ts ( t.timeLoss + used, t.indices )


canReachInRound : Float -> Float -> Float -> Bool
canReachInRound speed usedSpeed distance =
    usedSpeed == 0 || speed - usedSpeed >= distance
