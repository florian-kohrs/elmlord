module Templates.HelperTemplate exposing (roundDigits, troopToHtml)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Msg
import Troops


{-| Returns a container for a troop overview (image, name and amount)

    @param {Troop}: Takes the troop that needs to be displayed

-}
troopToHtml : Troops.TroopType -> Int -> String -> Html Msg.Msg
troopToHtml t amount cls =
    div [ Html.Attributes.class cls ]
        [ img [ src ("./assets/images/troops/" ++ String.toLower (Troops.troopName t) ++ ".png") ] []
        , span [] [ Html.text (String.fromInt amount ++ "  " ++ Troops.troopName t) ]
        ]


{-| Round the a float value to two digits after the decimal point
(the float does not get rounded, all digits after second digit are getting cut)

    @param {Float}: Takes the float value that has to be round up

-}
roundDigits : Float -> String
roundDigits v =
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
            x ++ "." ++ String.left 2 xs
