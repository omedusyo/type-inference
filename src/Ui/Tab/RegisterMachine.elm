module Ui.Tab.RegisterMachine exposing (Model, Msg, init, subscriptions, update, view)

import Dropdown
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionPointer, Value(..))
import List.Extra as List
import RegisterMachine.Controllers as Controllers
import RegisterMachine.GarbageCollector as GarbageCollector
import RegisterMachine.Machine as RegisterMachine exposing (CompilationError(..), ComputationStep(..), ControlledMachineState, ControllerExample, LabelEnvironment, RuntimeError(..))
import RegisterMachine.MemoryState exposing (MemoryError(..))
import RegisterMachine.OperationEnvironment as OperationEnvironment
import RegisterMachine.Ui.Editor as Editor
import RegisterMachine.Ui.Runtime as Runtime
import Ui.Control.Action as Action exposing (Action)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.Element as E
import Ui.Style.Button as Button


type alias Model =
    { controllers : List ControllerExample
    , controllerDropdownModel : Dropdown.State ControllerExample
    , selectedController : ControllerExample

    -- runtime
    , runtimeModel : Result CompilationError Runtime.Model
    , currentInstructionPointer : InstructionPointer

    -- editor
    , editorModel : Editor.Model
    }


init : Effect rootMsg Msg Model
init =
    let
        controllers : List ControllerExample
        controllers =
            [ Controllers.controller0_gcd
            , Controllers.controller1_remainder
            , Controllers.controller2_fct_iterative
            , Controllers.controller3_gcd_with_inlined_remainder
            , Controllers.controller4_gcd_with_inlined_remainder_using_jump
            , Controllers.controller6_fct_recursive
            , Controllers.controller7_fibonacci_recursive
            , Controllers.controller8_memory_test
            , Controllers.controller9_range
            , Controllers.controller10_append
            , GarbageCollector.controller
            ]

        defaultSelectedController : ControllerExample
        defaultSelectedController =
            controllers |> List.getAt 0 |> Maybe.withDefault Controllers.controller0_gcd

        compiledMachine : Result CompilationError ( LabelEnvironment, ControlledMachineState )
        compiledMachine =
            RegisterMachine.compileMachineFromControllerExample defaultSelectedController OperationEnvironment.env
    in
    Effect.pure
        (\editorModel runtimeModelResult ->
            { controllers = controllers
            , controllerDropdownModel = Dropdown.init "controllers"
            , selectedController = defaultSelectedController
            , runtimeModel = runtimeModelResult
            , currentInstructionPointer = 0
            , editorModel = editorModel
            }
        )
        |> Effect.ooo (Editor.init defaultSelectedController.instructionBlock |> Effect.mapMsg EditorMsg)
        |> Effect.ooo
            (case compiledMachine of
                Ok ( labelEnv, machine ) ->
                    Runtime.init labelEnv (Continue machine)
                        |> Effect.mapMsg RuntimeMsg
                        |> Effect.map Ok

                Err err ->
                    Effect.pure (Err err)
            )


type Msg
    = RuntimeMsg Runtime.Msg
    | CurrentInstructionPointerChanged InstructionPointer
      -- ===Controllers Dropdown===
    | ControllerPicked (Maybe ControllerExample)
    | ControllersDropdownMsg (Dropdown.Msg ControllerExample)
    | NewProgramButtonClicked
      -- editor
    | EditorMsg Editor.Msg


resetRuntime : ControllerExample -> Action rootMsg Msg Model
resetRuntime controllerExample =
    Action.fromEffect
        (\model ->
            let
                compiledMachine : Result CompilationError ( LabelEnvironment, ControlledMachineState )
                compiledMachine =
                    RegisterMachine.compileMachineFromControllerExample controllerExample OperationEnvironment.env
            in
            case compiledMachine of
                Ok ( labelEnv, machine ) ->
                    Runtime.init labelEnv (Continue machine)
                        |> Effect.mapMsg RuntimeMsg
                        |> Effect.map (\runtimeModel -> { model | runtimeModel = Ok runtimeModel, currentInstructionPointer = 0 })

                Err err ->
                    Effect.pure { model | runtimeModel = Err err, currentInstructionPointer = 0 }
        )


resetEditor : ControllerExample -> Action rootMsg Msg Model
resetEditor controllerExample =
    Action.fromEffect
        (\model ->
            Editor.init controllerExample.instructionBlock
                |> Effect.mapMsg EditorMsg
                |> Effect.map (\editorModel -> { model | editorModel = editorModel })
        )


update : Msg -> Action rootMsg Msg Model
update msg =
    case msg of
        RuntimeMsg runtimeMsg ->
            Action.liftMsgToCmd
                (\lift ->
                    Runtime.update (lift << CurrentInstructionPointerChanged) runtimeMsg
                        |> Action.embedIfOk
                            RuntimeMsg
                            .runtimeModel
                            (\model runtimeModel -> { model | runtimeModel = Ok runtimeModel })
                )

        CurrentInstructionPointerChanged instructionPointer ->
            let
                _ =
                    Debug.log "INSTRUCTION POINTER CHANGES" instructionPointer
            in
            Action.from (\model -> { model | currentInstructionPointer = instructionPointer })

        ControllerPicked maybeControllerExample ->
            case maybeControllerExample of
                Just controllerExample ->
                    Action.from (\model -> { model | selectedController = controllerExample })
                        |> Action.thenAction (resetRuntime controllerExample)
                        |> Action.thenAction (resetEditor controllerExample)

                Nothing ->
                    Action.none

        ControllersDropdownMsg dropdownMsg ->
            Action.fromWithCommand
                (\model ->
                    let
                        ( newDropdownModel, cmd ) =
                            Dropdown.update dropdownConfig dropdownMsg model model.controllerDropdownModel
                    in
                    ( { model | controllerDropdownModel = newDropdownModel }, cmd )
                )

        NewProgramButtonClicked ->
            Action.from
                (\model ->
                    let
                        newController =
                            RegisterMachine.emptyControllerExample
                    in
                    { model
                        | controllers = newController :: model.controllers
                        , selectedController = newController
                    }
                )

        EditorMsg editorMsg ->
            Editor.update editorMsg
                |> Action.embed
                    EditorMsg
                    .editorModel
                    (\model editorModel -> { model | editorModel = editorModel })


view : Model -> Element Msg
view model =
    E.column [ E.width E.fill, E.spacing 15 ]
        [ E.row []
            [ Dropdown.view dropdownConfig model model.controllerDropdownModel
            , Input.button Button.whiteButtonStyle
                { onPress = Just NewProgramButtonClicked
                , label = E.text "New"
                }
            ]
        , E.row [ E.width E.fill, E.spacing 30 ]
            [ -- ===Instructions===
              E.el [ E.width E.fill, E.alignTop, E.alignLeft ] (Editor.view model.currentInstructionPointer model.editorModel |> E.map EditorMsg)
            , -- ===Runtime State===
              case model.runtimeModel of
                Ok runtimeModel ->
                    Runtime.view runtimeModel |> E.map RuntimeMsg

                Err compilationError ->
                    E.column [ E.alignTop, E.width E.fill ]
                        [ E.text
                            (Runtime.compilationErrorToString compilationError)
                        ]
            ]
        ]



-- ===Controller Dropdown===


dropdownConfig : Dropdown.Config ControllerExample Msg Model
dropdownConfig =
    Dropdown.basic
        { itemsFromModel = \model -> model.controllers
        , selectionFromModel = \model -> Just model.selectedController
        , dropdownMsg = ControllersDropdownMsg
        , onSelectMsg = ControllerPicked
        , itemToPrompt =
            \controllerExample ->
                E.text controllerExample.name
        , itemToElement =
            \selected highlighted controllerExample ->
                let
                    bgColor =
                        if selected then
                            E.rgb255 100 100 100

                        else
                            E.rgb255 255 255 255
                in
                E.row
                    [ Background.color bgColor
                    , E.padding 8
                    , E.spacing 10
                    , E.width E.fill
                    ]
                    [ E.el [ Font.size 16 ] (E.text controllerExample.name)
                    ]
        }
        |> Dropdown.withContainerAttributes
            [ E.width (E.px 400) ]
        |> Dropdown.withSelectAttributes
            [ Border.width 1, Border.rounded 5, E.paddingXY 16 8, E.spacing 10, E.width E.fill ]
        |> Dropdown.withListAttributes
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , E.width E.fill
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Editor.subscriptions model.editorModel
        |> Sub.map EditorMsg
