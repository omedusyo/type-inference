module Calculus.Ui.Main exposing (Model, Msg, init, update, view)

import Calculus.Ui.Button as Button
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


type Tab
    = Program
    | Module
    | Help


tabs : List Tab
tabs =
    [ Program, Module, Help ]


initTab : Tab
initTab =
    Module


tabToString : Tab -> String
tabToString tab =
    case tab of
        Program ->
            "Program"

        Module ->
            "Module"

        Help ->
            "Help"


type alias Model =
    { tab : Tab

    -- subcomponents
    , programModel : Program.Model
    , moduleModel : Module.Model
    , helpModel : Help.Model
    }


init : InitContext Model Msg
init =
    InitContext.setModelTo
        (\programModel moduleModel helpModel ->
            { tab = initTab

            -- subcomponents
            , programModel = programModel
            , moduleModel = moduleModel
            , helpModel = helpModel
            }
        )
        |> InitContext.ooo (Program.init |> InitContext.mapCmd ProgramMsg)
        |> InitContext.ooo (Module.init |> InitContext.mapCmd ModuleMsg)
        |> InitContext.ooo (Help.init |> InitContext.mapCmd HelpMsg)


type Msg
    = ChangeTab Tab
      -- subcomponents
    | HelpMsg Help.Msg
    | ModuleMsg Module.Msg
    | ProgramMsg Program.Msg


update : Msg -> Context Model msg
update msg =
    case msg of
        ChangeTab tab ->
            Context.update (\model -> { model | tab = tab })

        HelpMsg helpMsg ->
            Help.update helpMsg
                |> Context.embed
                    .helpModel
                    (\model helpModel -> { model | helpModel = helpModel })

        ModuleMsg moduleMsg ->
            Module.update moduleMsg
                |> Context.embed
                    .moduleModel
                    (\model moduleModel -> { model | moduleModel = moduleModel })

        ProgramMsg programMsg ->
            Program.update programMsg
                |> Context.embed
                    .programModel
                    (\model programModel -> { model | programModel = programModel })


view : Config -> Model -> Element Msg
view config model =
    E.column [ E.width E.fill, E.padding 10 ]
        [ E.row []
            (tabs
                |> List.map
                    (\tab ->
                        Input.button Button.buttonStyle
                            { onPress = Just (ChangeTab tab)
                            , label = E.text (tabToString tab)
                            }
                    )
            )
        , case model.tab of
            Program ->
                Program.view config model.programModel |> E.map ProgramMsg

            Module ->
                Module.view config model.moduleModel |> E.map ModuleMsg

            Help ->
                Help.view config model.helpModel |> E.map HelpMsg
        ]
