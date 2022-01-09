module Calculus.Ui.Module exposing (Model, Msg, init, update, view)

import Calculus.Ui.Control.Context as Context exposing (Config, Context)
import Calculus.Ui.Control.InitContext as InitContext exposing (InitContext)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Model =
    {}


init : InitContext Model Msg
init =
    InitContext.setModelTo
        {}


type Msg
    = Wat


update : Msg -> Context Model msg
update msg =
    case msg of
        Wat ->
            Context.none


view : Config -> Model -> Element Msg
view config model =
    E.column []
        [ E.text "hello from Module" ]
