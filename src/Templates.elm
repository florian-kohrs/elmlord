module Template exposing (startMenuTemplate, loadSavesTemplate)

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import List exposing (..)
import String exposing (..)


-- Start menu with some menu selections
startMenuTemplate : Html Msg
startMenuTemplate  =
    div [ HtmlAttr.class "main-container" ]
    [ addStylesheet "link" "styles.css"
    , addStylesheet "link" "//fonts.googleapis.com/css?family=MedievalSharp"
    , div [ HtmlAttr.class "start-logo-container" ] [ span [] [ Html.text "Logo" ] ]
    , div [ HtmlAttr.class "start-container" ]
        [ div [ HtmlAttr.class "start-header" ]
            [ span [ HtmlAttr.class "start-header-text" ] [ Html.text "Welcome mylord, what is your decision?" ] ]
        , div [ HtmlAttr.class "start-actions" ]
            [ div [] [ button [ onClick ShowMap, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Start Campaign" ] ] ]
            , div [] [ button [ onClick ShowSaves, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Load Campaign" ] ] ]
            , div [] [ button [ onClick ShowSaves, HtmlAttr.class "start-buttons" ] [ span [] [ Html.text "Documentation" ] ] ]
            ]
        ]
    ]


-- Saved loads template with some example data
loadSavesTemplate : Html Msg
loadSavesTemplate =
    div [ HtmlAttr.class "main-container" ]
    [ addStylesheet "link" "styles.css"
    , addStylesheet "link" "//fonts.googleapis.com/css?family=MedievalSharp"
    , div [ HtmlAttr.class "start-logo-container" ] [ span [] [ Html.text "Logo" ] ]
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


addStylesheet : String -> String -> Html Msg 
addStylesheet tag href = 
    Html.node tag [ HtmlAttr.attribute "Rel" "stylesheet", HtmlAttr.attribute "property" "stylesheet", HtmlAttr.attribute "href" href] []