module Main exposing (..)

import Evaluation
import Html as H
import Inference
import LambdaBasics
import Return exposing (Return)
import TermParser


type alias Model =
    {}


type Msg
    = NothinYet


update : Msg -> Model -> Return Msg Model
update msg model =
    model
        |> Return.singleton


main =
    H.text "Hello!"
