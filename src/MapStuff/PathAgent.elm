module PathAgent exposing (Agent, getAgent, moveAlongPath, pathPartsToTime, remainingMovement, resetUsedMovement, setUsedMovement, simulateDistance)

import Dict exposing (Dict)
import MapData
import MaybeExt
import Pathfinder
import Vector


type alias Agent =
    { target : Maybe Vector.Point, speed : Float, usedMovement : Float }


remainingMovement : Agent -> Float
remainingMovement a =
    a.speed - a.usedMovement


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


pathPartsToTime : Agent -> List Pathfinder.PathTile -> List ( Pathfinder.PathTile, Int )
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


getAgent : Float -> Agent
getAgent speed =
    { target = Nothing, speed = speed, usedMovement = 0.0 }


simulateDistance : Float -> MoveSimulator -> MoveSimulator
simulateDistance f sim =
    if canReachInRound sim.maxMove sim.turnUsedMove f then
        { sim | turnUsedMove = sim.turnUsedMove + f }

    else
        { sim | turn = sim.turn + 1, turnUsedMove = f }


moveAlongPath : Pathfinder.Path -> Vector.Point -> Agent -> ( Float, Vector.Point )
moveAlongPath p start a =
    followPath (List.reverse (pathPartsToTime a p.path)) ( a.usedMovement, start )


followPath : List ( Pathfinder.PathTile, Int ) -> ( Float, Vector.Point ) -> ( Float, Vector.Point )
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
