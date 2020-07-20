port module Ports exposing (startMusic, playSound, links)

port startMusic : String -> Cmd msg

port playSound :  String -> Cmd msg

port links : String -> Cmd msg