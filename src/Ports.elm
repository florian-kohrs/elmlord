port module Ports exposing (sounds, links)

port sounds : String -> Cmd msg

port links : String -> Cmd msg