module Ui.InputCell exposing (..)

import Browser
import Element as E exposing (Attribute, Element)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



-- Uses input-cell.js web component


htmlInputCell : List (H.Attribute msg) -> List (Html msg) -> Html msg
htmlInputCell =
    H.node "input-cell"


inputCell : Int -> String -> (String -> msg) -> Element msg
inputCell fontSize text onChange =
    E.html
        (htmlInputCell [ HE.onInput onChange, HA.value text, HA.style "font-size" (String.fromInt fontSize ++ "px") ] [])
