module Ui.Tab.RegisterMachine exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Dropdown
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionPointer, MemoryPointer, Value(..))
import RegisterMachine.Controllers as Controllers
import RegisterMachine.GarbageCollector as GarbageCollector
import RegisterMachine.Machine as RegisterMachine exposing (Controller, ControllerExample, MachineWithInstructions, RegisterEnvironment, RuntimeError(..), TranslationError)
import RegisterMachine.MemoryState as MemoryState exposing (MemoryCell, MemoryError(..), MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import RegisterMachine.Ui.Editor as Editor
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.Style.Button as Button


type alias Model =
    { controllers : List ControllerExample
    , controllerDropdownModel : Dropdown.State ControllerExample
    , selectedController : Maybe ControllerExample
    , maybeRuntime : Maybe (Result RuntimeError MachineWithInstructions)
    , memoryView : MemoryView
    , currentlyHighlightedCell : MemoryPointer

    -- editor
    , editorModel : Editor.Model
    }


shouldDisplayEditor : Bool
shouldDisplayEditor =
    True


operationEnv : RegisterMachine.OperationEnvironment
operationEnv =
    let
        boolToInt : Bool -> Int
        boolToInt b =
            if b then
                1

            else
                0
    in
    Dict.fromList
        [ ( "sub", RegisterMachine.makeNumOperation2 (\x y -> x - y) )
        , ( "less-than?", RegisterMachine.makeNumOperation2 (\x y -> boolToInt (x < y)) )
        , ( "add", RegisterMachine.makeNumOperation2 (\x y -> x + y) )
        , ( "mul", RegisterMachine.makeNumOperation2 (\x y -> x * y) )
        , ( "zero?", RegisterMachine.makeNumOperation1 (\x -> boolToInt (x == 0)) )
        , ( "eq?", RegisterMachine.makeNumOperation2 (\x y -> boolToInt (x == y)) )
        , ( "not", RegisterMachine.makeNumOperation1 (\x -> boolToInt (x == 0)) )
        , ( "decrement", RegisterMachine.makeNumOperation1 (\x -> x - 1) )
        , ( "increment", RegisterMachine.makeNumOperation1 (\x -> x + 1) )
        , ( "remainder", RegisterMachine.makeNumOperation2 (\x y -> remainderBy y x) )
        , ( "pair?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        Pair _ ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "nil?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        ConstantValue Nil ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "num?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        ConstantValue (Num _) ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "moved?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        Moved ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        ]


init : InitContext Msg Model
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

        defaultSelectedController =
            Controllers.controller7_fibonacci_recursive

        parsedMachine : Result TranslationError MachineWithInstructions
        parsedMachine =
            RegisterMachine.makeMachine defaultSelectedController.controller defaultSelectedController.initialRegisterEnvironment operationEnv
    in
    InitContext.setModelTo
        (\editorModel ->
            { controllers = controllers
            , controllerDropdownModel = Dropdown.init "controllers"
            , selectedController = Just defaultSelectedController
            , maybeRuntime =
                case parsedMachine of
                    Ok machine ->
                        Just (Ok machine)

                    Err _ ->
                        Nothing
            , memoryView = initMemoryView
            , currentlyHighlightedCell = centerOfMemoryView initMemoryView
            , editorModel = editorModel
            }
        )
        |> InitContext.ooo (Editor.init |> InitContext.mapCmd EditorMsg)


type alias MemoryView =
    { bottom : Int, top : Int }


initMemoryView : MemoryView
initMemoryView =
    { bottom = 0, top = 10 }


centerOfMemoryView : MemoryView -> Int
centerOfMemoryView { top, bottom } =
    bottom + (top - bottom) // 2


shiftBy : Int -> MemoryView -> MemoryView
shiftBy delta ({ bottom, top } as memoryView) =
    let
        ( bottomNew, topNew ) =
            ( bottom + delta, top + delta )
    in
    if bottomNew < 0 then
        { bottom = 0, top = topNew - bottomNew }

    else
        { bottom = bottomNew, top = topNew }


centerAt : Int -> MemoryView -> MemoryView
centerAt p memoryView =
    let
        oldCenter =
            centerOfMemoryView memoryView
    in
    memoryView |> shiftBy (p - oldCenter)


reset : Model -> Model
reset model =
    case model.selectedController of
        Just controllerExample ->
            let
                parsedMachineResult =
                    RegisterMachine.makeMachine controllerExample.controller controllerExample.initialRegisterEnvironment operationEnv
            in
            case parsedMachineResult of
                Ok machine ->
                    { model
                        | maybeRuntime = Just (Ok machine)
                        , memoryView = initMemoryView
                        , currentlyHighlightedCell = centerOfMemoryView initMemoryView
                    }

                Err _ ->
                    { model
                        | maybeRuntime = Nothing
                        , memoryView = initMemoryView
                        , currentlyHighlightedCell = centerOfMemoryView initMemoryView
                    }

        Nothing ->
            model


type Msg
    = Reset
    | Start
    | RunOneStep
    | MemoryPointerClicked MemoryPointer
    | ShiftMemoryViewBy Int
      -- ===Controllers Dropdown===
    | ControllerPicked (Maybe ControllerExample)
    | ControllersDropdownMsg (Dropdown.Msg ControllerExample)
      -- editor
    | EditorMsg Editor.Msg


update : Msg -> Context rootMsg Msg Model
update msg =
    case msg of
        Reset ->
            Context.update reset

        Start ->
            Context.update
                (\model ->
                    { model
                        | maybeRuntime =
                            model.maybeRuntime
                                |> Maybe.map (Result.andThen RegisterMachine.start)
                    }
                )

        RunOneStep ->
            Context.update
                (\model ->
                    { model
                        | maybeRuntime =
                            model.maybeRuntime
                                |> Maybe.map
                                    (\resultMachine ->
                                        resultMachine
                                            |> Result.andThen RegisterMachine.runOneStep
                                            |> Result.map .machine
                                    )
                    }
                )

        MemoryPointerClicked p ->
            Context.update
                (\model ->
                    let
                        newMemoryView =
                            model.memoryView |> centerAt p
                    in
                    { model
                        | memoryView = newMemoryView
                        , currentlyHighlightedCell = p
                    }
                )

        ShiftMemoryViewBy delta ->
            Context.update
                (\model ->
                    let
                        newMemoryView =
                            model.memoryView |> shiftBy delta
                    in
                    { model
                        | memoryView = newMemoryView
                    }
                )

        ControllerPicked maybeControllerExample ->
            Context.update (\model -> { model | selectedController = maybeControllerExample } |> reset)

        ControllersDropdownMsg dropdownMsg ->
            Context.updateWithCommand
                (\model ->
                    let
                        ( newDropdownModel, cmd ) =
                            Dropdown.update dropdownConfig dropdownMsg model model.controllerDropdownModel
                    in
                    ( { model | controllerDropdownModel = newDropdownModel }, cmd )
                )

        EditorMsg editorMsg ->
            Editor.update editorMsg
                |> Context.embed
                    EditorMsg
                    .editorModel
                    (\model editorModel -> { model | editorModel = editorModel })


headingSize : Int
headingSize =
    20


heading : String -> Element a
heading str =
    E.el [ Font.size headingSize, Font.bold ] (E.text str)


view : Config -> Model -> Element Msg
view config model =
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
                [ case model.maybeRuntime of
                    Nothing ->
                        E.text ""

                    Just (Err runtimeError) ->
                        E.text (runTimeErrorToString runtimeError)

                    Just (Ok ({ machineState, instructionsState } as machine)) ->
                        case model.selectedController of
                            Just controllerExample ->
                                E.column []
                                    [ heading "Controller"
                                    , viewInstructions instructionsState.instructionPointer controllerExample.controller.instructions
                                    ]

                            Nothing ->
                                E.text ""
                ]
            , -- ===Runtime State===
              E.column [ E.alignTop ]
                [ E.row []
                    [ Input.button Button.buttonStyle
                        { onPress =
                            Just Reset
                        , label = E.text "Reset"
                        }
                    , Input.button Button.buttonStyle
                        { onPress =
                            Just Start
                        , label = E.text "Start"
                        }
                    , Input.button Button.buttonStyle
                        { onPress =
                            Just RunOneStep
                        , label = E.text "Run one step"
                        }
                    ]
                , case model.maybeRuntime of
                    Nothing ->
                        E.text ""

                    Just (Err runtimeError) ->
                        E.text (runTimeErrorToString runtimeError)

                    Just (Ok ({ machineState, instructionsState } as machine)) ->
                        E.row [ E.spacing 30 ]
                            [ E.column [ E.spacing 20, E.alignTop ]
                                [ E.column []
                                    [ heading "Registers"
                                    , viewRegisters (machineState.env |> Dict.toList) model
                                    ]
                                , E.column []
                                    [ heading "Memory"
                                    , viewMemoryState (machineState |> RegisterMachine.currentMemoryState RegisterMachine.Main) model
                                    ]
                                , E.column []
                                    [ heading "Dual Memory"
                                    , viewMemoryState (machineState |> RegisterMachine.currentMemoryState RegisterMachine.Dual) model
                                    ]
                                ]
                            , E.column [ E.alignTop, E.width (E.px 100) ]
                                [ heading "Stack"
                                , viewStack machineState.stack model
                                ]
                            ]
                ]
            ]
        ]



-- ===Controller Dropdown===


dropdownConfig : Dropdown.Config ControllerExample Msg Model
dropdownConfig =
    Dropdown.basic
        { itemsFromModel = \model -> model.controllers
        , selectionFromModel = \model -> model.selectedController
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


runTimeErrorToString : RuntimeError -> String
runTimeErrorToString err =
    case err of
        UndefinedRegister register ->
            String.concat [ "Undefined register $", register ]

        UndefinedOperation operationName ->
            String.concat [ "Undefined operation ", operationName ]

        WrongNumberOfArgumentsGivenToOperationExpected int ->
            String.concat [ "Wrong number of arguments given to the operation. Expected ", String.fromInt int ]

        LabelPointsToNothing label ->
            String.concat [ "The label :", label, " points to nothing" ]

        PoppingEmptyStack ->
            "Popping empty stack"

        TheOperationExpectsIntegerArguments ->
            "The operation expects integer arguments"

        ExpectedInstructionPointerInRegister ->
            "Expected instruction p in the register"

        ExpectedPairInRegister ->
            "Expected pair in the register"

        MemoryError memoryError ->
            case memoryError of
                MemoryExceeded ->
                    "Memory Exceeded"

                InvalidMemoryAccessAt pointer ->
                    String.concat [ "Invalid memory access at #", String.fromInt pointer ]

                ExpectedNumAt pointer ->
                    String.concat [ "Expected Num at #", String.fromInt pointer ]

                ExpectedPairAt pointer ->
                    String.concat [ "Expected Pair at #", String.fromInt pointer ]

                ExpectedNilAt pointer ->
                    String.concat [ "Expected Nil at #", String.fromInt pointer ]


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

                    RegisterMachine.JumpToLabelAtRegister { labelRegister } ->
                        [ viewInstructionName "jump", viewRegisterUse labelRegister ]

                    RegisterMachine.JumpToLabelIf { testRegister, label } ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToLabelAtRegisterIf { testRegister, labelRegister } ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewRegisterUse labelRegister ]

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

                    RegisterMachine.AssignCallAtLabel { targetRegister, label } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewInstructionName "call", viewLabelUse label ]

                    RegisterMachine.AssignCallAtRegister { targetRegister, labelRegister } ->
                        [ viewRegisterName targetRegister, viewInstructionName "<-", viewInstructionName "call", viewRegisterUse labelRegister ]

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

                    RegisterMachine.MarkAsMoved {toBeCollectedFromRegister, referenceToDualMemoryRegister} ->
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


viewRegisters : List ( RegisterMachine.Register, Value ) -> Model -> Element Msg
viewRegisters registers model =
    let
        registerStyle =
            [ Background.color (E.rgb255 240 0 245)
            , E.padding 20
            , E.width (E.px 60)
            , E.height (E.px 60)
            ]

        viewRegister : RegisterMachine.Register -> Value -> Element Msg
        viewRegister name val =
            E.row [ E.spacing 10 ]
                [ E.el [ E.width (E.px 230) ] (E.text name), E.text "<-", E.el registerStyle (viewValue val model) ]
    in
    -- registers
    E.column [ E.width E.fill, E.spacing 5 ]
        (registers
            |> List.map (\( name, val ) -> viewRegister name val)
        )


viewStack : Stack -> Model -> Element Msg
viewStack stack model =
    E.column [ E.width E.fill ]
        (stack
            |> Stack.toList
            |> List.reverse
            |> List.map
                (\val ->
                    E.column [ Border.width 1, Border.solid, E.paddingXY 0 15, E.width (E.px 70) ]
                        [ E.el [ E.centerX, E.width E.fill ] (viewValue val model)
                        ]
                )
        )


viewMemoryState : MemoryState -> Model -> Element Msg
viewMemoryState memoryState model =
    let
        viewMemorySegment a b =
            E.row [ E.width E.fill ]
                (memoryState.memory
                    |> Array.slice a (b + 1)
                    |> Array.toIndexedList
                    |> List.map
                        (\( i, memoryCell ) ->
                            viewMemoryCell (i + a) memoryCell model
                        )
                )
    in
    E.column [ E.width E.fill ]
        [ E.row [] [ E.text "Next free pointer: ", viewMemoryPointer memoryState.nextFreePointer ]
        , E.row []
            [ Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy -1), label = E.text "-1" }
            , Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy 1), label = E.text "+1" }
            ]
        , viewMemorySegment model.memoryView.bottom model.memoryView.top
        ]


viewMemoryCell : MemoryPointer -> MemoryCell -> Model -> Element Msg
viewMemoryCell memoryPointer ( a, b ) model =
    E.column [ Border.solid, Border.width 1, E.width (E.px 70) ]
        [ E.column [ E.centerX, E.paddingXY 0 15, E.height (E.px 50), E.width E.fill ]
            [ viewValue a model
            , viewValue b model
            ]
        , E.el
            (List.concat
                -- [ [ E.centerX, E.paddingXY 0 5, Background.color (E.rgb 64 52 235) ]
                [ [ E.centerX, Background.color (E.rgb 0 0 0), E.width E.fill ]
                , if model.currentlyHighlightedCell == memoryPointer then
                    [ Font.color (E.rgb255 255 0 0) ]

                  else
                    [ Font.color (E.rgb255 255 255 255) ]
                ]
            )
            (E.text (String.concat [ "#", String.fromInt memoryPointer ]))
        ]


viewValue : Value -> Model -> Element Msg
viewValue value model =
    case value of
        ConstantValue constant ->
            viewConstant constant

        Pair memoryPointer ->
            viewMemoryPointer memoryPointer

        InstructionPointer instructionPointer ->
            viewInstructionPointer instructionPointer

        Uninitialized ->
            E.text ""

        Moved ->
            E.text "Moved"


viewConstant : Constant -> Element Msg
viewConstant const =
    E.row []
        [ case const of
            Num x ->
                E.text (String.fromInt x)

            Nil ->
                E.text "nil"
        ]


viewMemoryPointer : MemoryPointer -> Element Msg
viewMemoryPointer p =
    E.el [ Events.onClick (MemoryPointerClicked p), E.pointer ]
        (E.text (String.concat [ "#", String.fromInt p ]))


viewInstructionPointer : InstructionPointer -> Element Msg
viewInstructionPointer pointer =
    E.text (String.concat [ ":", String.fromInt pointer ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Editor.subscriptions model.editorModel
        |> Sub.map EditorMsg
