module MapAction.Model exposing (..)

import Dict
import MapAction.SubModel exposing (..)
import Msg
import Svg


type alias InteractableSvg =
    { svg : SvgItem, action : List MapTileMsg }


type SvgItem
    = SvgItem Int (Svg.Svg Msg.Msg)


type alias InteractableMapSVG =
    Dict.Dict Int (List InteractableSvg)
