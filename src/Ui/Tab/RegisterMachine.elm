module Ui.Tab.RegisterMachine exposing (Model, Msg, init, update, view)

import Element as E exposing (Element)
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
            Context.update (\model -> model)


view : Config -> Model -> Element Msg
view config model =
    E.el []
        (E.text "hello, register machine world!")
