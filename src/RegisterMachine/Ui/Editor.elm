module RegisterMachine.Ui.Editor exposing (..)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)


type alias Model =
    {}


init : InitContext Model Msg
init =
    InitContext.setModelTo
        {}


type Msg
    = TODO


update : Msg -> Context Model msg
update msg =
    case msg of
        TODO ->
            Context.none


view model =
    E.text "hello from editor"
