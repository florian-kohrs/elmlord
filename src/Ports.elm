port module Ports exposing (startMusic, playSound, openLink)

port startMusic : String -> Cmd msg

port playSound :  String -> Cmd msg

port openLink : String -> Cmd msg