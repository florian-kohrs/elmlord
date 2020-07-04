module Templates.EndTemplate exposing (..)

import Entities exposing (..)
import Faction exposing (..)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Troops exposing (..)
import OperatorExt exposing (..)

winningTitle : String
winningTitle = "We have won!"

winningDesc : String
winningDesc = "My lord we have destroyed the last enemy, the campaign is done, we showed them our supremacy."

losingTitle : String
losingTitle = "We have lost!"

losingDesc : String
losingDesc = "My lord our castle was taken, the campaign is a lost cause."


{-| Returns the layout for the ending modal, when the player either won or lost

    @param {Bool}: Takes a bool, which says if the player won (True) or lost (False) the game
-}

generateEndTemplate : Bool -> Html Msg
generateEndTemplate bool =
    div [Html.Attributes.class "modal-background"] [
        div [Html.Attributes.class "end-modal"] (generateEndData bool)
    ]


{-| Fills the layout with the notification whether the player won or lost (somes texts and a return button)

    @param {Bool}: Takes a bool, which says if the player won (True) or lost (False) the game
-}

generateEndData : Bool -> List (Html Msg)
generateEndData res =
        [div [Html.Attributes.class "end-modal-title"] [
            span [Html.Attributes.class (ternary res "winning-color" "losing-color")] [Html.text (ternary res winningTitle losingTitle)]
        ]
        , div [Html.Attributes.class "end-modal-desc"] [
            span [] [Html.text (ternary res winningDesc losingDesc)]
        ]
        , div [] [
            button [] [
                span [] [Html.text "Go to the main menue"]
            ]
        ]]
