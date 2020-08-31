port module Ports exposing (openLink, playSound, startMusic, transitSoundToMusic, updateVolume)


port updateVolume : Int -> Cmd msg


port startMusic : String -> Cmd msg


port playSound : String -> Cmd msg

-- plays a sound and after this resumes the set background-music
port transitSoundToMusic : ( String, Int ) -> Cmd msg


port openLink : String -> Cmd msg
