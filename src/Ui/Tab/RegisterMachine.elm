module Ui.Tab.RegisterMachine exposing (Model, Msg, init, subscriptions, update, view)

import Dropdown
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionPointer, Value(..))
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


type alias Model =
    { controllers : List ControllerExample
    , controllerDropdownModel : Dropdown.State ControllerExample
    , selectedController : ControllerExample

    -- runtime
    , runtimeModel : Result CompilationError Runtime.Model
    , currentInstructionPointerResult : Maybe InstructionPointer

    -- editor
    , editorModel : Editor.Model
    }


shouldDisplayEditor : Bool
shouldDisplayEditor =
    True


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
            Controllers.controller7_fibonacci_recursive

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
            , -- TODO: Is 0 as the default instruction pointer reasonable?
              currentInstructionPointerResult = Just 0
            , editorModel = editorModel
            }
        )
        |> Effect.ooo (Editor.init |> Effect.mapMsg EditorMsg)
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
      -- editor
    | EditorMsg Editor.Msg


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
            Action.update (\model -> { model | currentInstructionPointerResult = Just instructionPointer })

        ControllerPicked maybeControllerExample ->
            case maybeControllerExample of
                Just controllerExample ->
                    Action.update (\model -> { model | selectedController = controllerExample })
                        |> Action.updateFromEffect
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
                                            |> Effect.map (\runtimeModel -> { model | runtimeModel = Ok runtimeModel })

                                    Err err ->
                                        Effect.pure { model | runtimeModel = Err err }
                            )

                Nothing ->
                    Action.none

        ControllersDropdownMsg dropdownMsg ->
            Action.updateWithCommand
                (\model ->
                    let
                        ( newDropdownModel, cmd ) =
                            Dropdown.update dropdownConfig dropdownMsg model model.controllerDropdownModel
                    in
                    ( { model | controllerDropdownModel = newDropdownModel }, cmd )
                )

        EditorMsg editorMsg ->
            Editor.update editorMsg
                |> Action.embed
                    EditorMsg
                    .editorModel
                    (\model editorModel -> { model | editorModel = editorModel })


view : Model -> Element Msg
view model =
    -- 1. I need to display all the registers
    -- 2. I need to display the instruction block with labels
    E.column [ E.width E.fill, E.spacing 15 ]
        [ if shouldDisplayEditor then
            E.el [ Border.width 1, E.width E.fill ] (Editor.view model.editorModel |> E.map EditorMsg)

          else
            E.text ""
        , Dropdown.view dropdownConfig model model.controllerDropdownModel
        , E.row [ E.width E.fill, E.spacing 30 ]
            [ -- ===Instructions===
              E.column [ E.width E.fill, E.alignTop, E.alignLeft ]
                [ E.column []
                    [ E.heading "Controller"
                    , case model.currentInstructionPointerResult of
                        Nothing ->
                            -- TODO: Note that here we loose compilation error information
                            --       We need to somehow also isolate the compilation from runtime
                            --       Compilation is the responsibility of the editor and not the simulator?
                            -- Err compilationError ->
                            --     E.text (compilationErrorToString compilationError)
                            E.text "error?"

                        Just currentInstructionPointer ->
                            viewInstructions currentInstructionPointer model.selectedController.instructionBlock
                    ]
                ]
            , -- ===Runtime State===
              case model.runtimeModel of
                Ok runtimeModel ->
                    Runtime.view runtimeModel |> E.map RuntimeMsg

                Err compilationError ->
                    E.text "compilation error"
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


type LabelOrInstruction
    = Label RegisterMachine.Label
    | Perform Int RegisterMachine.Instruction


viewInstructions : Int -> RegisterMachine.InstructionBlock -> Element Msg
viewInstructions instructionPointer instructionBlock =
    let
        convertInstructionBlock : RegisterMachine.InstructionBlock -> List LabelOrInstruction
        convertInstructionBlock instructions =
            let
                update0 : RegisterMachine.LabelOrInstruction -> ( Int, List LabelOrInstruction ) -> ( Int, List LabelOrInstruction )
                update0 labelOrInstruction ( position, newInstructions ) =
                    case labelOrInstruction of
                        RegisterMachine.Label label ->
                            ( position, Label label :: newInstructions )

                        RegisterMachine.Perform instruction ->
                            ( position + 1, Perform position instruction :: newInstructions )
            in
            List.foldl update0 ( 0, [] ) instructions
                |> Tuple.second
                |> List.reverse

        viewInstructionName name =
            E.el [ Font.heavy ] (E.text name)

        viewRegisterName name =
            E.el [ Font.color (E.rgb255 0 56 186) ] (E.text name)

        viewRegisterUse name =
            viewRegisterName ("$" ++ name)

        viewLabel label =
            E.el [ Font.color (E.rgb255 239 151 0) ] (E.text label)

        viewLabelUse label =
            viewLabel (":" ++ label)

        viewOperationUse name args =
            E.row [] [ E.el [] (E.text name), E.text "(", E.row [] (List.intersperse (E.text ", ") args), E.text ")" ]

        viewLabelIntroduction label =
            E.row [ E.spacing 8 ] [ E.text "label ", E.row [] [ viewLabel label ] ]

        paddingLeft px =
            E.paddingEach { left = px, top = 0, right = 0, bottom = 0 }

        viewOperationArgument : RegisterMachine.OperationArgument -> Element Msg
        viewOperationArgument argument =
            case argument of
                RegisterMachine.Register register ->
                    viewRegisterUse register

                RegisterMachine.Constant val ->
                    viewConstant val

        viewOperationApplication : RegisterMachine.OperationName -> List RegisterMachine.OperationArgument -> Element Msg
        viewOperationApplication opName arguments =
            viewOperationUse
                opName
                (arguments |> List.map viewOperationArgument)

        viewInstruction : Bool -> RegisterMachine.Instruction -> Element Msg
        viewInstruction isFocused instruction =
            E.row
                (List.concat
                    [ [ E.spacing 8, paddingLeft 20 ]
                    , if isFocused then
                        [ Background.color (E.rgb255 215 215 215) ]

                      else
                        []
                    ]
                )
                (case instruction of
                    RegisterMachine.AssignRegister { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewRegisterUse sourceRegister ]

                    RegisterMachine.AssignLabel { targetRegister, label } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewLabelUse label ]

                    RegisterMachine.AssignOperation { targetRegister, operationApplication } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication operationApplication.name operationApplication.arguments ]

                    RegisterMachine.AssignConstant { targetRegister, constant } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewConstant constant ]

                    RegisterMachine.JumpToLabel { label } ->
                        [ viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToInstructionPointerAtRegister { instructionPointerRegister } ->
                        [ viewInstructionName "jump", viewRegisterUse instructionPointerRegister ]

                    RegisterMachine.JumpToLabelIf { testRegister, label } ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToInstructionPointerAtRegisterIf { testRegister, instructionPointerRegister } ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewRegisterUse instructionPointerRegister ]

                    RegisterMachine.Halt _ ->
                        [ viewInstructionName "halt" ]

                    RegisterMachine.PushRegister { sourceRegister } ->
                        [ viewInstructionName "push", viewRegisterUse sourceRegister ]

                    RegisterMachine.PushConstant { constant } ->
                        [ viewInstructionName "push", viewConstant constant ]

                    RegisterMachine.PushLabel { label } ->
                        [ viewInstructionName "push", viewLabelUse label ]

                    RegisterMachine.Pop { targetRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewInstructionName "pop-stack" ]

                    RegisterMachine.ConstructPair { targetRegister, operationArgument0, operationArgument1 } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "pair" [ operationArgument0, operationArgument1 ] ]

                    RegisterMachine.First { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "first" [ RegisterMachine.Register sourceRegister ] ]

                    RegisterMachine.Second { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "second" [ RegisterMachine.Register sourceRegister ] ]

                    RegisterMachine.SetFirst { targetRegister, operationArgument } ->
                        [ viewInstructionName "set-first", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                    RegisterMachine.SetSecond { targetRegister, operationArgument } ->
                        [ viewInstructionName "set-second", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                    RegisterMachine.DualFirst { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "dual-first" [ RegisterMachine.Register sourceRegister ] ]

                    RegisterMachine.DualSecond { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "dual-second" [ RegisterMachine.Register sourceRegister ] ]

                    RegisterMachine.DualSetFirst { targetRegister, operationArgument } ->
                        [ viewInstructionName "dual-set-first", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                    RegisterMachine.DualSetSecond { targetRegister, operationArgument } ->
                        [ viewInstructionName "dual-set-second", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                    RegisterMachine.MoveToDual { targetRegister, sourceRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "move-to-dual" [ RegisterMachine.Register sourceRegister ] ]

                    RegisterMachine.MarkAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } ->
                        [ viewInstructionName "mark", viewRegisterUse toBeCollectedFromRegister, viewInstructionName "as-moved-to", viewRegisterUse referenceToDualMemoryRegister ]

                    RegisterMachine.SwapMemory _ ->
                        [ viewInstructionName "swap-memory" ]
                )
    in
    E.column [ E.width E.fill ]
        (instructionBlock
            |> convertInstructionBlock
            |> List.map
                (\labelOrInstruction ->
                    case labelOrInstruction of
                        Label label ->
                            viewLabelIntroduction label

                        Perform position instruction ->
                            viewInstruction (instructionPointer == position) instruction
                )
        )


viewConstant : Constant -> Element Msg
viewConstant const =
    E.row []
        [ case const of
            Num x ->
                E.text (String.fromInt x)

            Nil ->
                E.text "nil"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Editor.subscriptions model.editorModel
        |> Sub.map EditorMsg
