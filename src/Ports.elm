port module Ports exposing (startMusic, playSound, openLink, updateVolumne, transitSoundToMusic)

port updateVolumne : Int -> Cmd msg

port startMusic : String -> Cmd msg

port playSound :  String -> Cmd msg

port transitSoundToMusic : (String, Int) -> Cmd msg

port openLink : String -> Cmd msg