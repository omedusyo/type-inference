module Calculus.Ui.Button exposing (blue, buttonStyle)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


blue =
    E.rgb255 142 207 245


buttonStyle =
    [ Background.color blue
    , E.paddingXY 9 4
    , Border.rounded 2
    ]
