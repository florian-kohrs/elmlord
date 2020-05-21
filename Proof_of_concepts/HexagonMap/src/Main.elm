module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, span, text, input, select, option, img)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import List exposing (..)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (..)




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
-- Cleanup Hexagon -> zu einem Punkt -> Add Surfacetype


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
    , state : State
    }


type Hexagon
    = Hexagon ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) ( Float, Float ) Faction


type Msg
    = None
    | ShowSaves
    | ShowLogin
    | ShowMap
    | ShowStart
    | ShowIndex String


type State
    = Login
    | Normal
    | Start
    | Saves


init : Model
init =
    { map = pointToHexagons ( 400, 300 ) hexRange hexRange hexHeight
    , chosen = ""
    , state = Login
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

        ShowMap ->
            { model | state = Normal }

        ShowLogin -> 
            { model | state = Login }

        ShowSaves -> 
            { model | state = Saves }

        ShowStart -> 
            { model | state = Start }

        ShowIndex text ->
            { model | chosen = text }


hexagonMapToSvg : Model -> List (Svg Msg)
hexagonMapToSvg model =
    List.map generateHexagon model.map


generateHexagon : Hexagon -> Svg.Svg Msg
generateHexagon (Hexagon p1 p2 p3 p4 p5 p6 ( l, r ) _) =
    polygon [ onClick (ShowIndex ("Chosen-Hexagon: (" ++ String.fromFloat l ++ "," ++ String.fromFloat r ++ ")")), fill "green", stroke "black", points (tTS p1 ++ tTS p2 ++ tTS p3 ++ tTS p4 ++ tTS p5 ++ tTS p6) ] []


tTS : ( Float, Float ) -> String
tTS ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


view : Model -> Html Msg
view model =
    case model.state of
        Normal ->
            gameTemplate model

        Login ->
            startMenuTemplate

        Saves -> 
            loadSavesTemplate 

        Start -> 
            startCampaign 



startMenuTemplate : Html Msg
startMenuTemplate  =
    div [ HtmlAttr.class "main-container" ]
    [ addStylesheet "link" "styles.css"
    , addStylesheet "link" "//fonts.googleapis.com/css?family=MedievalSharp"
    , div [ HtmlAttr.class "start-logo-container" ] [ img [HtmlAttr.src "https://i.ibb.co/hRgjXkq/logo.png"] [] ]
    , div [ HtmlAttr.class "start-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "Welcome mylord, what is your decision?" ] ]
        , div [ HtmlAttr.class "start-actions" ]
            [ div [] [ button [ onClick ShowStart, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Start Campaign" ] ] ]
            , div [] [ button [ onClick ShowSaves, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Load Campaign" ] ] ]
            , div [] [ button [ onClick ShowSaves, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Documentation" ] ] ]
            ]
        ]
    ]


startCampaign : Html Msg 
startCampaign =
    div [ HtmlAttr.class "main-container" ]
    [ addStylesheet "link" "styles.css"
    , addStylesheet "link" "//fonts.googleapis.com/css?family=MedievalSharp"
    , div [ HtmlAttr.class "start-logo-container" ] [ img [HtmlAttr.src "https://i.ibb.co/hRgjXkq/logo.png"] [] ]
    , div [ HtmlAttr.class "campaign-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "M'lord, what are your campaign-settings?" ] ]
        , div [ HtmlAttr.class "campaign-actions" ]
            [ div [ HtmlAttr.class "campaign-name-container"] 
                [
                    span [HtmlAttr.class "campaign-name"] [Html.text "Name:"]
                    , input [HtmlAttr.class "campaign-input"] []
                ]
            , div [ HtmlAttr.class "campaign-select-container"] 
                [
                    span [HtmlAttr.class "campaign-name"] [Html.text "Lords:"]
                    , select [HtmlAttr.class "campaign-select"] (lordsToOption (List.range 2 4))
                ]
            , div [ HtmlAttr.class "campaign-buttons-container"] 
                [
                    div [] [ button [ onClick ShowMap, HtmlAttr.class "start-buttons start-campaign-button" ] [ span [] [ Html.text "Start Campaign" ] ] ]
                ]
            ]
        , div [] [ button [ onClick ShowLogin, HtmlAttr.class "back-btn" ] [ span [] [ Html.text "Back" ] ] ]
        ]
    ]


lordsToOption : List Int -> List (Html Msg) 
lordsToOption list = 
    case list of
        [] -> []
        (x :: xs) -> 
            option [HtmlAttr.value (String.fromInt x)] [ Html.text ((String.fromInt x) ++ " Lords") ] :: lordsToOption xs     


loadSavesTemplate : Html Msg
loadSavesTemplate =
    div [ HtmlAttr.class "main-container" ]
    [ addStylesheet "link" "styles.css"
    , addStylesheet "link" "//fonts.googleapis.com/css?family=MedievalSharp"
    , div [ HtmlAttr.class "start-logo-container" ] [ img [HtmlAttr.src "https://i.ibb.co/hRgjXkq/logo.png"] [] ]
    , div [ HtmlAttr.class "save-loads-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "Mylord, choose your save" ] ]
        , div [ HtmlAttr.class "save-loads" ]
            [ div [ HtmlAttr.class "save-load" ]
                [ div [ HtmlAttr.class "save-load-name" ] [ span [] [ Html.text "The greatest campain of all time" ] ]
                , div [ HtmlAttr.class "save-load-created" ] [ span [] [ Html.text "Created: 01.05.2020 14:34" ] ]
                , div [ HtmlAttr.class "save-load-updated" ] [ span [] [ Html.text "Last updated: 03.05.2020 13:51" ] ]
                ]
            , div [ HtmlAttr.class "save-load" ]
                [ div [ HtmlAttr.class "save-load-name" ] [ span [] [ Html.text "Destroy all herectis" ] ]
                , div [ HtmlAttr.class "save-load-created" ] [ span [] [ Html.text "Created: 30.04.2020 14:34" ] ]
                , div [ HtmlAttr.class "save-load-updated" ] [ span [] [ Html.text "Last updated: 30.04.2020 18:51" ] ]
                ]
            , div [ HtmlAttr.class "save-load" ]
                [ div [ HtmlAttr.class "save-load-name" ] [ span [] [ Html.text "Be the trader not the traded :)" ] ]
                , div [ HtmlAttr.class "save-load-created" ] [ span [] [ Html.text "Created: 27.05.2020 10:14" ] ]
                , div [ HtmlAttr.class "save-load-updated" ] [ span [] [ Html.text "Last updated: 30.04.2020 16:51" ] ]
                ]
            , div [ HtmlAttr.class "save-load" ]
                [ div [ HtmlAttr.class "save-load-name" ] [ span [] [ Html.text "TestTestTest" ] ]
                , div [ HtmlAttr.class "save-load-created" ] [ span [] [ Html.text "Created: 22.04.2020 12:11" ] ]
                , div [ HtmlAttr.class "save-load-updated" ] [ span [] [ Html.text "Last updated: 22.04.2020 12:12" ] ]
                ]
            ]
        , div [] [ button [ onClick ShowLogin, HtmlAttr.class "back-btn" ] [ span [] [ Html.text "Back" ] ] ]
        ]
    ]


gameTemplate : Model -> Html Msg
gameTemplate model =
    div
        []
        [ div [ HtmlAttr.style "height" "600px", HtmlAttr.style "width" "600px" ]
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


addStylesheet : String -> String -> Html Msg 
addStylesheet tag href = 
    Html.node tag [ HtmlAttr.attribute "Rel" "stylesheet", HtmlAttr.attribute "property" "stylesheet", HtmlAttr.attribute "href" href] []

