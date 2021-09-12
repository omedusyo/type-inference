module Main exposing (..)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Evaluation as L
import Html as H
import Inference as L
import LambdaBasics as L
import Return exposing (Return)
import TermParser as L


type alias Model =
    {}


type Msg
    = NothinYet


update : Msg -> Model -> Return Msg Model
update msg model =
    model
        |> Return.singleton


main =
    E.layout []
        (E.row [ E.width E.fill, E.centerX ]
            [ e
            , e
            ]
        )


e =
    E.el
        [ Background.color (E.rgb255 240 0 245)
        , Font.color (E.rgb255 255 255 255)
        , Border.rounded 3
        , E.padding 30
        ]
        (E.text "hello, world!")
