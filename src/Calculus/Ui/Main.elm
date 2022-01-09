module Calculus.Ui.Main exposing (Model, Msg, init, update, view)

import Calculus.Ui.Control.Context as Context exposing (Config, Context)
import Calculus.Ui.Control.InitContext as InitContext exposing (InitContext)
import Calculus.Ui.Help as Help
import Calculus.Ui.Module as Module
import Calculus.Ui.Program as Program
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Model =
    { programModel : Program.Model
    , moduleModel : Module.Model
    , helpModel : Help.Model
    }


init : InitContext Model Msg
init =
    InitContext.setModelTo
        (\programModel moduleModel helpModel ->
            { programModel = programModel
            , moduleModel = moduleModel
            , helpModel = helpModel
            }
        )
        |> InitContext.ooo (Program.init |> InitContext.mapCmd ProgramMsg)
        |> InitContext.ooo (Module.init |> InitContext.mapCmd ModuleMsg)
        |> InitContext.ooo (Help.init |> InitContext.mapCmd HelpMsg)


type Msg
    = HelpMsg Help.Msg
    | ModuleMsg Module.Msg
    | ProgramMsg Program.Msg


update : Msg -> Context Model msg
update msg =
    case msg of
        HelpMsg helpMsg ->
            Help.update helpMsg
                |> Context.embed
                    .helpModel
                    (\model helpModel -> { model | helpModel = helpModel })

        ModuleMsg moduleMsg ->
            Module.update moduleMsg
                |> Context.embed
                    .helpModel
                    (\model moduleModel -> { model | moduleModel = moduleModel })

        ProgramMsg programMsg ->
            Program.update programMsg
                |> Context.embed
                    .programModel
                    (\model programModel -> { model | programModel = programModel })


view : Config -> Model -> Element Msg
view config model =
    E.column []
        [ E.text "hello from Lambda-Ui"
        , Help.view config model.helpModel |> E.map HelpMsg
        , Program.view config model.helpModel |> E.map ProgramMsg
        , Module.view config model.helpModel |> E.map ModuleMsg
        ]
