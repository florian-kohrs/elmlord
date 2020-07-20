module Templates.StartTemplate exposing (..)

import Html exposing (Html, button, div, img, input, option, select, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import List exposing (..)
import Msg
import String exposing (..)



-- Start menu with some menu selections


startMenuTemplate : List (Html Msg.Msg)
startMenuTemplate =
    [ div [ HtmlAttr.class "start-logo-container" ] [ img [ HtmlAttr.src "./assets/images/general/logo.png" ] [] ]
    , div [ HtmlAttr.class "start-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "Welcome mylord, what is your decision?" ] ]
        , div [ HtmlAttr.class "start-actions" ]
            [ div [] [ button [ onClick (Msg.MenueAction Msg.SetCampaingn),HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Start Campaign" ] ] ]
            , div [] [ button [ HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Documentation" ] ] ]
            , div [] [ button [ HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Credits" ] ] ]
            ]
        ]
    ]


startCampaign : List (Html Msg.Msg)
startCampaign =
    [ div [ HtmlAttr.class "start-logo-container" ] [ img [ HtmlAttr.src "./assets/images/general/logo.png" ] [] ]
    , div [ HtmlAttr.class "campaign-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "M'lord, what is your name?" ] ]
        , div [ HtmlAttr.class "campaign-actions" ]
            [ div [ HtmlAttr.class "campaign-name-container" ]
                [ span [ HtmlAttr.class "campaign-name" ] [ Html.text "Name:" ]
                , input [ HtmlAttr.class "campaign-input" ] []
                ]
            , div [ HtmlAttr.class "campaign-buttons-container" ]
                [ div [] [ button [ onClick (Msg.MenueAction Msg.StartGame), HtmlAttr.class "start-buttons start-campaign-button" ] [ span [] [ Html.text "Start Campaign" ] ] ]
                ]
            ]
        , div [] [ button [ onClick (Msg.MenueAction Msg.ShowMenue), HtmlAttr.class "back-btn" ] [ span [] [ Html.text "Back" ] ] ]
        ]
    ]
