module Ui.Element exposing (heading)

import Element as E exposing (Element)
import Element.Font as Font


headingSize : Int
headingSize =
    20


heading : String -> Element a
heading str =
    E.el [ Font.size headingSize, Font.bold ] (E.text str)
