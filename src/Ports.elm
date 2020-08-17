port module Ports exposing (startMusic, playSound, openLink, updateVolume, transitSoundToMusic)

port updateVolume : Int -> Cmd msg

port startMusic : String -> Cmd msg

port playSound :  String -> Cmd msg

port transitSoundToMusic : (String, Int) -> Cmd msg

port openLink : String -> Cmd msg