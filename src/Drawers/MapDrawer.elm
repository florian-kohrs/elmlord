module MapDrawer exposing (..)

import Dict
import Entities
import ListExt
import MapData
import MaybeExt
import Svg
import Types
import Vector


type alias InteractableSvg =
    { svg : SvgItem, action : Maybe SvgAction }


type alias SvgAction =
    { toolTip : String, action : Types.MapTileMsg }


type SvgItem
    = SvgItem Int (Svg.Svg Types.Msg)


type alias MapClickAction =
    Dict.Dict Int (List InteractableSvg)


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
