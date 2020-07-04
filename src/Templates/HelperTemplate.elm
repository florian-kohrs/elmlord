module Templates.HelperTemplate exposing (troopToHtml, roundDigits)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (..)
import Types
import Troops

{-| Returns a container for a troop overview (image, name and amount)

    @param {Troop}: Takes the troop that needs to be displayed
-}

troopToHtml : Troops.Troop -> Html Types.Msg
troopToHtml troop =
        div [Html.Attributes.class "stationed-troop-container troop-container"] [
            img [src  ("./assets/images/troops/" ++ String.toLower (Troops.troopName troop.troopType) ++ ".png")] [],
            span [] [Html.text (String.fromInt troop.amount ++ "  " ++ Troops.troopName troop.troopType) ]
        ]

{-| Round the a float value to two digits after the decimal point
    (the float does not get rounded, all digits after second digit are getting cut)

    @param {Float}: Takes the float value that has to be round up
-}
roundDigits : Float -> String
roundDigits v =
    let 
        parts = String.split "." (String.fromFloat v)
    in
        case parts of 
            [] -> 
                "0.00"
            
            (x :: []) -> 
                x ++ ".00"

            (x :: (xs :: _)) -> 
                x ++ "." ++ String.left 2 xs