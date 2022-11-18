module Ui.Main exposing (Model, Msg, init, subscriptions, update, view)

import Element as E exposing (Element)
import Element.Input as Input
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.Control.Config exposing (Config)
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


init : Effect rootMsg Msg Model
init =
    Effect.pure
        (\programModel moduleModel helpModel registerMachineModel ->
            { tab = initTab

            -- subcomponents
            , programModel = programModel
            , moduleModel = moduleModel
            , helpModel = helpModel
            , registerMachineModel = registerMachineModel
            }
        )
        |> Effect.ooo (Program.init |> Effect.mapMsg ProgramMsg)
        |> Effect.ooo (Module.init |> Effect.mapMsg ModuleMsg)
        |> Effect.ooo (Help.init |> Effect.mapMsg HelpMsg)
        |> Effect.ooo (RegisterMachine.init |> Effect.mapMsg RegisterMachineMsg)


type Msg
    = ChangeTab Tab
      -- subcomponents
    | HelpMsg Help.Msg
    | ModuleMsg Module.Msg
    | ProgramMsg Program.Msg
    | RegisterMachineMsg RegisterMachine.Msg


update : Msg -> Action rootMsg Msg Model
update msg =
    case msg of
        ChangeTab tab ->
            Context.update (\model -> { model | tab = tab })

        HelpMsg helpMsg ->
            Help.update helpMsg
                |> Context.embed
                    HelpMsg
                    .helpModel
                    (\model helpModel -> { model | helpModel = helpModel })

        ModuleMsg moduleMsg ->
            Module.update moduleMsg
                |> Context.embed
                    ModuleMsg
                    .moduleModel
                    (\model moduleModel -> { model | moduleModel = moduleModel })

        ProgramMsg programMsg ->
            Program.update programMsg
                |> Context.embed
                    ProgramMsg
                    .programModel
                    (\model programModel -> { model | programModel = programModel })

        RegisterMachineMsg registerMachineMsg ->
            RegisterMachine.update registerMachineMsg
                |> Context.embed
                    RegisterMachineMsg
                    .registerMachineModel
                    (\model registerMachineModel -> { model | registerMachineModel = registerMachineModel })


view : Model -> Element Msg
view model =
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
                Program.view model.programModel |> E.map ProgramMsg

            ModuleTab ->
                Module.view model.moduleModel |> E.map ModuleMsg

            HelpTab ->
                Help.view model.helpModel |> E.map HelpMsg

            RegisterMachineTab ->
                RegisterMachine.view model.registerMachineModel |> E.map RegisterMachineMsg
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    RegisterMachine.subscriptions model.registerMachineModel
        |> Sub.map RegisterMachineMsg
