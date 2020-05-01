module Modal exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import String exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- temp-Factions


type alias Model =
    { hide : Bool
    }


type Msg
    = None
    | Click


init : Model
init =
    { hide = False }


update : Msg -> Model -> Model
update dir model =
    case dir of
        None ->
            init

        Click ->
            init


stylesheet : Html Msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "Rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "styles.css"
            ]

        children =
            []
    in
    node tag attrs children


appendModal : Model -> Html Msg
appendModal model = 
    if model.hide then
        div [class "modal-container"] []
    else
        div [class "modal-container"] []


view : Model -> Html Msg
view model =
    div
        [ class "main-container" ]
        [ stylesheet, button [ onClick Click, class "test-button" ] [ Html.text "Hide/Show" ], (appendModal model) ]