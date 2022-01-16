module Ui.Main exposing (Model, Msg, init, update, view)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.Style.Button as Button
import Ui.Tab.Help as Help
import Ui.Tab.Module as Module
import Ui.Tab.Program as Program
import Ui.Tab.RegisterMachine as RegisterMachine


type Tab
    = ProgramTab
    | ModuleTab
    | HelpTab
    | RegisterMachineTab


tabs : List Tab
tabs =
    [ ProgramTab, ModuleTab, HelpTab, RegisterMachineTab ]


initTab : Tab
initTab =
    RegisterMachineTab


tabToString : Tab -> String
tabToString tab =
    case tab of
        ProgramTab ->
            "Program"

        ModuleTab ->
            "Module"

        HelpTab ->
            "Help"

        RegisterMachineTab ->
            "Register Machine"


type alias Model =
    { tab : Tab

    -- subcomponents
    , programModel : Program.Model
    , moduleModel : Module.Model
    , helpModel : Help.Model
    , registerMachineModel : RegisterMachine.Model
    }


init : InitContext Model Msg
init =
    InitContext.setModelTo
        (\programModel moduleModel helpModel registerMachineModel ->
            { tab = initTab

            -- subcomponents
            , programModel = programModel
            , moduleModel = moduleModel
            , helpModel = helpModel
            , registerMachineModel = registerMachineModel
            }
        )
        |> InitContext.ooo (Program.init |> InitContext.mapCmd ProgramMsg)
        |> InitContext.ooo (Module.init |> InitContext.mapCmd ModuleMsg)
        |> InitContext.ooo (Help.init |> InitContext.mapCmd HelpMsg)
        |> InitContext.ooo (RegisterMachine.init |> InitContext.mapCmd RegisterMachineMsg)


type Msg
    = ChangeTab Tab
      -- subcomponents
    | HelpMsg Help.Msg
    | ModuleMsg Module.Msg
    | ProgramMsg Program.Msg
    | RegisterMachineMsg RegisterMachine.Msg


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

        RegisterMachineMsg registerMachineMsg ->
            RegisterMachine.update registerMachineMsg
                |> Context.embed
                    .registerMachineModel
                    (\model registerMachineModel -> { model | registerMachineModel = registerMachineModel })


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
            ProgramTab ->
                Program.view config model.programModel |> E.map ProgramMsg

            ModuleTab ->
                Module.view config model.moduleModel |> E.map ModuleMsg

            HelpTab ->
                Help.view config model.helpModel |> E.map HelpMsg

            RegisterMachineTab ->
                RegisterMachine.view config model.registerMachineModel |> E.map RegisterMachineMsg
        ]
