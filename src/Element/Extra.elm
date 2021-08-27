module Element.Extra exposing
    ( date
    , time
    )

import Element exposing (..)
import Html
import Html.Attributes
import Html.Events


date : List (Attribute msg) -> { label : String, date : String, onChange : String -> msg } -> Element msg
date attributes model =
    Html.label
        []
        [ Html.text model.label
        , Html.br [] []
        , Html.input
            [ Html.Attributes.type_ "date"
            , Html.Attributes.value model.date
            , Html.Events.onInput model.onChange
            , Html.Attributes.style "font-size" "16px"
            , Html.Attributes.style "padding" "8px"
            ]
            []
        ]
        |> html
        |> el attributes


time : List (Attribute msg) -> { label : String, time : String, onChange : String -> msg } -> Element msg
time attributes model =
    Html.label
        []
        [ Html.text model.label
        , Html.br [] []
        , Html.input
            [ Html.Attributes.type_ "time"
            , Html.Attributes.value model.time
            , Html.Events.onInput model.onChange
            , Html.Attributes.style "font-size" "16px"
            , Html.Attributes.style "padding" "8px"
            ]
            []
        ]
        |> html
        |> el attributes
