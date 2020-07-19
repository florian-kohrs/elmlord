module MapAction exposing (..)

import Dict
import ListExt
import MapAction.Model exposing (..)
import MapAction.SubModel exposing (..)
import MapData
import MaybeExt
import Msg
import Svg
import Vector


isZAllowedOn : Int -> Int -> Bool
isZAllowedOn z main =
    ((main /= MapData.lordZIndex && main /= MapData.settlementZIndex) || z /= MapData.imageTileZIndex)
        && (main /= MapData.pathZIndex || z /= MapData.imageTileZIndex)


isZAllowedIn : Int -> List Int -> Bool
isZAllowedIn i is =
    List.all (isZAllowedOn i) is


isSvgAllowedIn : InteractableSvg -> List InteractableSvg -> Bool
isSvgAllowedIn svg svgs =
    isZAllowedIn (getZIndex svg.svg) (List.map (\s -> getZIndex s.svg) svgs)


actionsOnPoint : Vector.Point -> MapClickAction -> List MapTileMsg
actionsOnPoint p dict =
    MaybeExt.foldMaybe (\l -> List.concat (List.map .action l)) [] (Dict.get (MapData.hashMapPoint p) dict)


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


allSvgs : MapClickAction -> List (Svg.Svg Msg.Msg)
allSvgs a =
    List.concat (List.map (List.map (\svg -> getSvg svg.svg)) (Dict.values a))


getZIndex : SvgItem -> Int
getZIndex (SvgItem i _) =
    i


getSvg : SvgItem -> Svg.Svg Msg.Msg
getSvg (SvgItem _ svg) =
    svg
