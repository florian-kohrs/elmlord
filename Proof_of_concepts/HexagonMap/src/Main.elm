module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


edgeLength : Float
edgeLength =
    25


edgeDistance : Float
edgeDistance =
    55


hexHeight : Int
hexHeight =
    6


hexRange : Float
hexRange =
    6



-- temp-Factions


type Faction
    = Empty
    | Player String
    | Aserai String
    | Battania String
    | Khuzait String
    | Empire String
    | Sturgia String
    | Vlandia String


type alias Model =
    { map : List Hexagon
    , chosen : String
    }


type Hexagon
    = Hexagon ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) Faction


type Msg
    = None
    | Click String


init : Model
init =
    { map = pointToHexagons ( 400, 300 ) hexRange hexRange hexHeight
    , chosen = ""
    }


pointToHexagons : ( Float, Float ) -> Float -> Float -> Int -> List Hexagon
pointToHexagons ( mX, mY ) left right height =
    buildHexagon ( mX, mY ) ( 0, 0 )
        :: drawHexaColumPart ( mX, mY ) ( 0, 0 ) -1 left
        ++ drawHexaColumPart ( mX, mY ) ( 0, 0 ) 1 right
        ++ drawHexaRowPart ( mX, mY ) ( 0, 0 ) (calcRowParts ( left, right ) height) -1 height
        ++ drawHexaRowPart ( mX, mY ) ( 0, 0 ) (calcRowParts ( left, right ) height) 1 height


drawHexaRowPart : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> Int -> List Hexagon
drawHexaRowPart ( cx, cy ) ( px, py ) ( left, right ) faktor counter =
    if counter > 0 then
        buildHexagon (calcRowCenter ( cx, cy ) faktor counter) ( px, py + faktor )
            :: drawHexaColumPart (calcRowCenter ( cx, cy ) faktor counter) ( px, py + faktor ) -1 left
            ++ drawHexaColumPart (calcRowCenter ( cx, cy ) faktor counter) ( px, py + faktor ) 1 right
            ++ drawHexaRowPart (calcRowCenter ( cx, cy ) faktor counter) ( px, py + faktor ) (calcRowParts ( left, right ) (counter - 1)) faktor (counter - 1)

    else
        []


calcRowParts : ( Float, Float ) -> Int -> ( Float, Float )
calcRowParts ( left, right ) counter =
    if modBy 2 counter == 0 then
        ( left - 1, right )

    else
        ( left, right - 1 )


calcRowCenter : ( Float, Float ) -> Float -> Int -> ( Float, Float )
calcRowCenter ( x, y ) faktor counter =
    if modBy 2 counter == 0 then
        ( x - (edgeDistance / 2), y + (edgeDistance * faktor) )

    else
        ( x + (edgeDistance / 2), y + (edgeDistance * faktor) )


drawHexaColumPart : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> List Hexagon
drawHexaColumPart ( cx, cy ) ( px, py ) faktor counter =
    if counter > 0 then
        buildHexagon ( cx + (edgeDistance * faktor), cy ) ( px + faktor, py ) :: drawHexaColumPart ( cx + (edgeDistance * faktor), cy ) ( px + faktor, py ) faktor (counter - 1)

    else
        []


buildHexagon : ( Float, Float ) -> ( Float, Float ) -> Hexagon
buildHexagon ( x, y ) ( px, py ) =
    Hexagon ( x - edgeLength / 2, y - edgeLength ) ( x + edgeLength / 2, y - edgeLength ) ( x + edgeLength, y ) ( x + edgeLength / 2, y + edgeLength ) ( x - edgeLength / 2, y + edgeLength ) ( x - edgeLength, y ) ( px, py ) (Player "")


update : Msg -> Model -> Model
update dir model =
    case dir of
        None ->
            init

        Click text ->
            { model | chosen = text }


hexagonMapToSvg : Model -> List (Svg Msg)
hexagonMapToSvg model =
    List.map generateHexagon model.map


generateHexagon : Hexagon -> Svg.Svg Msg
generateHexagon (Hexagon p1 p2 p3 p4 p5 p6 ( l, r ) _) =
    polygon [ onClick (Click ("Chosen-Hexagon: (" ++ String.fromFloat l ++ "," ++ String.fromFloat r ++ ")")), fill "green", stroke "black", points (tTS p1 ++ tTS p2 ++ tTS p3 ++ tTS p4 ++ tTS p5 ++ tTS p6) ] []


tTS : ( Float, Float ) -> String
tTS ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


view : Model -> Html Msg
view model =
    div
        []
        [ div [ Html.Attributes.style "height" "600px", Html.Attributes.style "width" "600px" ]
            [ Svg.svg
                [ Svg.Attributes.viewBox "0 0 1000 800"
                , Svg.Attributes.width "600"
                , Svg.Attributes.height "600"
                , Svg.Attributes.fill "none"
                ]
                (hexagonMapToSvg model)
            ]
        , span [] [ Html.text model.chosen ]
        ]
