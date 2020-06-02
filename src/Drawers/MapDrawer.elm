module MapDrawer exposing (..)

import Dict
import Entities
import ListExt
import MaybeExt
import Svg
import Types
import Vector
import MapData


type alias InteractableSvg =
    { svg : SvgItem, action : Maybe SvgAction }


type alias SvgAction =
    { toolTip : String, action : Types.MapTileMsg }


type SvgItem
    = SvgItem Int (Svg.Svg Types.Msg)


type alias MapClickAction =
    Dict.Dict Int (List InteractableSvg)


actionsOnPoint : Vector.Point -> MapClickAction -> List SvgAction
actionsOnPoint p dict =
    MaybeExt.foldMaybe (\l -> ListExt.justList (List.map .action l)) [] (Dict.get (MapData.hashMapPoint p) dict)


addToMap : Int -> InteractableSvg -> MapClickAction -> MapClickAction
addToMap k v =
    Dict.update
        k
        (\vs ->
            Just
                (MaybeExt.foldMaybe
                    (ListExt.insertToSortedList v (\svg -> getZIndex svg.svg))
                    [ v ]
                    vs
                )
        )


sortDict : MapClickAction -> MapClickAction
sortDict =
    Dict.map (\_ -> List.sortBy (\svg -> getZIndex svg.svg))


allSvgs : MapClickAction -> List (Svg.Svg Types.Msg)
allSvgs a =
    List.concat (List.map (List.map (\svg -> getSvg svg.svg)) (Dict.values a))


getZIndex : SvgItem -> Int
getZIndex (SvgItem i _) =
    i


getSvg : SvgItem -> Svg.Svg Types.Msg
getSvg (SvgItem _ svg) =
    svg
