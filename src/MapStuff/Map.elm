module Map exposing (..)

import Browser
import Dict
import Entities exposing (Lord, Settlement)
import Faction exposing (Faction(..))
import Html.Events exposing (onClick)
import List exposing (..)
import ListExt
import MapData exposing (rad)
import MaybeExt
import Pathfinder exposing (NavigatableMap)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)


type alias Map =
    Dict.Dict String MapTile


type alias MapTile =
    { indices : Vector.Point
    , point : Vector.Vector
    , terrain : Terrain
    , settlement : Maybe Entities.Settlement
    , lords : List Entities.Lord
    , faction : Faction
    }


type alias MapTileDesign =
    { backgroundColor : String
    , strokeColor : String
    , strokeWidth : String
    }


moveLord : Entities.Lord -> Vector.Point -> Map -> Map
moveLord l newP m =
    updateLordsOnTile newP { l | entity = Entities.setPosition l.entity newP } (::) (updateLordsOnTile l.entity.position l (\lord -> List.filter ((/=) lord)) m)


updateLordsOnTile : Vector.Point -> Entities.Lord -> (Entities.Lord -> List Entities.Lord -> List Entities.Lord) -> Map -> Map
updateLordsOnTile p l update m =
    Dict.update
        (Vector.showPoint p)
        (Maybe.andThen (\t -> Just { t | lords = update l t.lords }))
        m



{-
   getClosestFreeFieldAt : Vector.Point -> Map -> Vector.Point
   getClosestFreeFieldAt p =
-}


type SvgItem a
    = SvgItem Int (Svg.Svg a)


getSvgForSettlement : Vector.Vector -> Vector.Vector -> Entities.Settlement -> SvgItem a
getSvgForSettlement pos size s =
    getImage 2 (Entities.settlementImageName s.settlementType) pos size


getSvgForLord : Vector.Vector -> Vector.Vector -> Lord -> SvgItem a
getSvgForLord pos size l =
    getImage 3 "Lord1.png" pos size


getImage : Int -> String -> Vector.Vector -> Vector.Vector -> SvgItem a
getImage z imgName pos size =
    SvgItem z
        (Svg.image
            [ --onClick (f tile.indices),
              Svg.Attributes.x (String.fromFloat (pos.x - size.x / 2))
            , Svg.Attributes.y (String.fromFloat (pos.y - size.y / 2))
            , Svg.Attributes.width (String.fromFloat size.x)
            , Svg.Attributes.height (String.fromFloat size.y)
            , Svg.Attributes.xlinkHref ("../Images/" ++ imgName)

            --, Svg.Attributes.src "../Images/Background.png"
            --, Svg.Attributes.overflow "visible"
            --, Svg.Attributes.fill "red"
            ]
            []
        )


setSettlement : MapTile -> Maybe Settlement -> MapTile
setSettlement t s =
    { t | settlement = s }


heighProgressToTerrain : Float -> Terrain
heighProgressToTerrain f =
    if f < 0.15 then
        Water

    else if f < 0.5 then
        Grass

    else if f < 0.85 then
        Forest

    else
        Mountain


type Terrain
    = Grass
    | Water
    | Forest
    | Mountain


type TerrainMoveType
    = CantWalkOn
    | CanWalkOn Float


canMoveOnTile : MapTile -> Bool
canMoveOnTile mapTile =
    case terrainToMove mapTile.terrain of
        CantWalkOn ->
            False

        CanWalkOn _ ->
            True


terrainToMove : Terrain -> TerrainMoveType
terrainToMove t =
    case t of
        Grass ->
            CanWalkOn 1

        Water ->
            CantWalkOn

        Forest ->
            CanWalkOn 0.8

        Mountain ->
            CanWalkOn 0.5


terrainToColor : Terrain -> String
terrainToColor t =
    case t of
        Grass ->
            "#00cd00"

        Water ->
            "#4ca1d2"

        Forest ->
            "#008000"

        Mountain ->
            "#ba8f30"


terrainToName : Terrain -> String
terrainToName t =
    case t of
        Grass ->
            "Grass"

        Water ->
            "Water"

        Forest ->
            "Forest"

        Mountain ->
            "Mountain"


mapToSvg : Map -> Float -> (Point -> a) -> List (Svg a)
mapToSvg =
    mapWithPathToSvg []


mapWithPathToSvg : List Vector.Point -> Map -> Float -> (Point -> a) -> List (Svg a)
mapWithPathToSvg ps m r f =
    List.map getSvg (List.sortBy getZIndex (List.concat (List.map (showMapTile ps r f) (Dict.values m))))


getZIndex : SvgItem a -> Int
getZIndex (SvgItem i _) =
    i


getSvg : SvgItem a -> Svg a
getSvg (SvgItem _ svg) =
    svg


styleMapTile : List Vector.Point -> MapTile -> MapTileDesign
styleMapTile ps tile =
    { backgroundColor = terrainToColor tile.terrain
    , strokeColor = getStrokeStyle ps tile "Orange" (factionToStrokeColor tile.faction)
    , strokeWidth = getStrokeStyle ps tile "8px" "2px"
    }


getStrokeStyle : List Vector.Point -> MapTile -> String -> String -> String
getStrokeStyle ps tile op1 op2 =
    if ListExt.indexOf (Vector.pointEqual tile.indices) ps >= 0 then
        op1

    else
        op2


factionToStrokeColor : Faction -> String
factionToStrokeColor faction =
    case faction of
        Faction.Faction1 ->
            "#ff4c4c"

        Faction.Faction2 ->
            "blue"

        Faction.Faction3 ->
            "green"

        Faction.Faction4 ->
            "yellow"


showMapTile : List Vector.Point -> Float -> (Point -> a) -> MapTile -> List (SvgItem a)
showMapTile ps tileRadius f tile =
    let
        tileDesign =
            styleMapTile ps tile
    in
    [ SvgItem 0
        (polygon
            [ onClick (f tile.indices)
            , Svg.Attributes.overflow "visible"
            , fill tileDesign.backgroundColor
            , stroke tileDesign.strokeColor
            , strokeWidth tileDesign.strokeWidth
            , points (pointsToHexagonPoints (generateHexagonPoints tile.point tileRadius))
            ]
            []
        )
    ]
        ++ MaybeExt.foldMaybe (\s -> [ getSvgForSettlement tile.point (Vector.scale (Vector.Vector MapData.hexRadius MapData.hexRadius) 1.5) s ]) [] tile.settlement
        ++ List.foldl (\l r -> getSvgForLord tile.point (Vector.scale (Vector.Vector MapData.hexRadius MapData.hexRadius) 1.5) l :: r) [] tile.lords


pointsToHexagonPoints : List Vector.Vector -> String
pointsToHexagonPoints =
    List.foldl (\v r -> r ++ String.fromFloat v.x ++ "," ++ String.fromFloat v.y ++ " ") ""



{-
   intialSettlementMapSetup : List Entities.Settlement -> Map -> Map
   intialSettlementMapSetup settlements =
       List.map (\( k, tile ) ->  setSettlement tile (List.head (List.filter (\s -> Vector.pointEqual tile.indices s.entity.position) settlements)))
-}


generateHexagonPoints : Vector -> Float -> List Vector.Vector
generateHexagonPoints v r =
    let
        topLeft =
            Vector.pointOnCircle r rad

        middleLeft =
            Vector.pointOnCircle r 0

        bottomLeft =
            Vector.flipOnY topLeft

        tl =
            Vector.add topLeft v

        ml =
            Vector.add middleLeft v

        bl =
            Vector.add bottomLeft v

        tr =
            Vector.add (Vector.flipOnX topLeft) v

        mr =
            Vector.add (Vector.flipOnX middleLeft) v

        br =
            Vector.add (Vector.flipOnX bottomLeft) v
    in
    [ tl, ml, bl, br, mr, tr ]
