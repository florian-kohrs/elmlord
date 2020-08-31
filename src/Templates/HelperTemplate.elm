module Templates.HelperTemplate exposing (roundDigits, troopToHtml)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Msg
import OperatorExt
import Troops


-- helper functions
--------------------------------------------------------

troopToHtml : Troops.TroopType -> Int -> String -> Html Msg.Msg
troopToHtml t amount cls =
    div [ Html.Attributes.class cls ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt amount ++ "  " ++ Troops.troopName t) ]
        ]


roundDigits : Float -> Int -> String
roundDigits v i =
    let
        parts =
            String.split "." (String.fromFloat v)
    in
    case parts of
        [] ->
            "0.00"

        x :: [] ->
            x ++ ".00"

        x :: (xs :: _) ->
            x ++ OperatorExt.ternary (i > 0) ("." ++ String.left i xs) ".00"
