module RegisterMachine.Ui.Runtime exposing (..)

import Array
import Array.Extra as Array
import Dict
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Lib.Cmd as Cmd
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionPointer, MemoryPointer, Value(..))
import RegisterMachine.Machine as RegisterMachine exposing (CompilationError(..), ComputationStep(..), ControlledMachineState, LabelEnvironment, MachineState, RuntimeError(..))
import RegisterMachine.MemoryState exposing (MemoryCell, MemoryError(..), MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.Element as E
import Ui.Style.Button as Button


type alias Model =
    { initialMachineResult : ComputationStep ControlledMachineState ControlledMachineState -- This is here so that we can reset
    , labelEnv : LabelEnvironment
    , machineResult : ComputationStep ControlledMachineState ControlledMachineState
    , memoryView : MemoryView
    , currentlyHighlightedCell : MemoryPointer
    }


init : LabelEnvironment -> ComputationStep ControlledMachineState ControlledMachineState -> Effect rootMsg Msg Model
init labelEnv machineResult =
    Effect.pure
        { initialMachineResult = machineResult
        , labelEnv = labelEnv
        , machineResult = machineResult
        , memoryView = initMemoryView
        , currentlyHighlightedCell = centerOfMemoryView initMemoryView
        }


type alias MemoryView =
    { bottom : Int, top : Int }


initMemoryView : MemoryView
initMemoryView =
    { bottom = 0, top = 10 }


centerOfMemoryView : MemoryView -> Int
centerOfMemoryView { top, bottom } =
    bottom + (top - bottom) // 2


shiftBy : Int -> MemoryView -> MemoryView
shiftBy delta { bottom, top } =
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


type Msg
    = Reset
    | StepUntilHalted
    | StepUntilNextJump
    | Step
    | MemoryPointerClicked MemoryPointer
    | ShiftMemoryViewBy Int


reset : Model -> Model
reset model =
    { model
        | machineResult = model.initialMachineResult
        , memoryView = initMemoryView
        , currentlyHighlightedCell = centerOfMemoryView initMemoryView
    }


update : (InstructionPointer -> Cmd rootMsg) -> Msg -> Action rootMsg Msg Model
update changeCurrentInstructionPointerCmd msg =
    let
        updateRuntime : (ControlledMachineState -> ComputationStep ControlledMachineState ControlledMachineState) -> Action rootMsg Msg Model
        updateRuntime f =
            Context.from
                (\({ machineResult } as model) ->
                    { model | machineResult = machineResult |> RegisterMachine.andThen f }
                )

        changeCurrentInstructionPointer : Model -> Cmd rootMsg
        changeCurrentInstructionPointer model =
            case RegisterMachine.currentInstructionPointerFromComputationStep model.machineResult of
                Just instructionPointer ->
                    changeCurrentInstructionPointerCmd instructionPointer

                Nothing ->
                    Cmd.none
    in
    case msg of
        Reset ->
            Context.from reset
                |> Context.thenRootCommand changeCurrentInstructionPointer

        StepUntilHalted ->
            updateRuntime RegisterMachine.stepUntilHalted
                |> Context.thenRootCommand changeCurrentInstructionPointer

        StepUntilNextJump ->
            updateRuntime RegisterMachine.stepUntilNextJump
                |> Context.thenRootCommand changeCurrentInstructionPointer

        Step ->
            updateRuntime RegisterMachine.step
                |> Context.thenRootCommand changeCurrentInstructionPointer

        MemoryPointerClicked p ->
            Context.from
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
            Context.from
                (\model ->
                    let
                        newMemoryView =
                            model.memoryView |> shiftBy delta
                    in
                    { model
                        | memoryView = newMemoryView
                    }
                )


view : Model -> Element Msg
view model =
    -- 1. I need to display all the registers
    -- 2. I need to display the instruction block with labels
    E.column [ E.alignTop, E.width E.fill ]
        [ E.row []
            [ Input.button Button.buttonStyle
                { onPress =
                    Just Reset
                , label = E.text "Reset"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    Just StepUntilHalted
                , label = E.text "Step until halted"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    Just StepUntilNextJump
                , label = E.text "Step until next jump"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    Just Step
                , label = E.text "Step"
                }
            ]
        , case model.machineResult of
            RuntimeError runtimeError ->
                E.column [ E.alignTop, E.width E.fill ]
                    [ E.text (runTimeErrorToString runtimeError) ]

            Halted { machineState } ->
                viewMachineState model machineState

            Continue { machineState } ->
                viewMachineState model machineState
        ]


viewMachineState : Model -> MachineState -> Element Msg
viewMachineState ({ labelEnv } as model) machineState =
    E.row [ E.spacing 30 ]
        [ E.column [ E.spacing 20, E.alignTop ]
            [ E.column []
                [ E.heading "Registers"
                , viewRegisters (machineState.registerEnv |> Dict.toList) labelEnv
                ]
            , E.column []
                [ E.heading "Memory"
                , viewMemoryState (machineState |> RegisterMachine.currentMemoryState RegisterMachine.Main) labelEnv model.memoryView model.currentlyHighlightedCell
                ]
            , E.column []
                [ E.heading "Dual Memory"
                , viewMemoryState (machineState |> RegisterMachine.currentMemoryState RegisterMachine.Dual) labelEnv model.memoryView model.currentlyHighlightedCell
                ]
            ]
        , E.column [ E.alignTop, E.width (E.px 100) ]
            [ E.heading "Stack"
            , viewStack machineState.stack labelEnv
            ]
        ]


viewRegisters : List ( RegisterMachine.Register, Value ) -> LabelEnvironment -> Element Msg
viewRegisters registers labelEnv =
    let
        registerStyle =
            [ Background.color (E.rgb255 240 0 245)
            , E.padding 20
            , E.width (E.px 60)
            , E.height (E.px 60)
            , E.clip
            , E.scrollbars
            ]

        viewRegister : RegisterMachine.Register -> Value -> Element Msg
        viewRegister name val =
            E.row [ E.spacing 10 ]
                [ E.el [ E.width (E.px 230) ] (E.text name), E.text "<-", E.el registerStyle (viewValue val labelEnv) ]
    in
    -- registers
    E.column [ E.width E.fill, E.spacing 5 ]
        (registers
            |> List.map (\( name, val ) -> viewRegister name val)
        )


viewStack : Stack -> LabelEnvironment -> Element Msg
viewStack stack labelEnv =
    E.column [ E.width E.fill ]
        (stack
            |> Stack.toList
            |> List.reverse
            |> List.map
                (\val ->
                    E.column [ Border.width 1, Border.solid, E.paddingXY 0 15, E.width (E.px 70), E.height (E.px 70), E.clip, E.scrollbars ]
                        [ E.el [ E.centerX, E.width E.fill ] (viewValue val labelEnv)
                        ]
                )
        )


viewMemoryState : MemoryState -> LabelEnvironment -> MemoryView -> MemoryPointer -> Element Msg
viewMemoryState memoryState labelEnv memoryView currentlyHighlightedCell =
    let
        viewMemorySegment a b =
            E.row [ E.width E.fill ]
                (memoryState.memory
                    |> Array.slice a (b + 1)
                    |> Array.toIndexedList
                    |> List.map
                        (\( i, memoryCell ) ->
                            viewMemoryCell (i + a) memoryCell labelEnv currentlyHighlightedCell
                        )
                )
    in
    E.column [ E.width E.fill ]
        [ E.row [] [ E.text "Next free pointer: ", viewMemoryPointer memoryState.nextFreePointer ]
        , E.row []
            [ Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy -1), label = E.text "-1" }
            , Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy 1), label = E.text "+1" }
            ]
        , viewMemorySegment memoryView.bottom memoryView.top
        ]


viewMemoryCell : MemoryPointer -> MemoryCell -> LabelEnvironment -> MemoryPointer -> Element Msg
viewMemoryCell memoryPointer ( a, b ) labelEnv currentlyHighlightedCell =
    E.column [ Border.solid, Border.width 1, E.width (E.px 70) ]
        [ E.column [ E.centerX, E.paddingXY 0 15, E.height (E.px 50), E.width E.fill ]
            [ viewValue a labelEnv
            , viewValue b labelEnv
            ]
        , E.el
            (List.concat
                -- [ [ E.centerX, E.paddingXY 0 5, Background.color (E.rgb 64 52 235) ]
                [ [ E.centerX, Background.color (E.rgb 0 0 0), E.width E.fill ]
                , if currentlyHighlightedCell == memoryPointer then
                    [ Font.color (E.rgb255 255 0 0) ]

                  else
                    [ Font.color (E.rgb255 255 255 255) ]
                ]
            )
            (E.text (String.concat [ "#", String.fromInt memoryPointer ]))
        ]


viewValue : Value -> LabelEnvironment -> Element Msg
viewValue value labelEnv =
    case value of
        ConstantValue constant ->
            viewConstant constant

        Pair memoryPointer ->
            viewMemoryPointer memoryPointer

        InstructionPointer instructionPointer ->
            viewInstructionPointer instructionPointer labelEnv

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


viewInstructionPointer : InstructionPointer -> LabelEnvironment -> Element Msg
viewInstructionPointer pointer labelEnv =
    case RegisterMachine.labelFromInstructionPointer pointer labelEnv of
        Just label ->
            E.text (String.concat [ ":", label ])

        Nothing ->
            E.text ":::error:::"


compilationErrorToString : CompilationError -> String
compilationErrorToString err =
    case err of
        LabelUsedMoreThanOnce label ->
            String.concat [ "Label :", label, " used more than once" ]

        LabelDoesNotExist label ->
            String.concat [ "Label :", label, " does not exist" ]

        UnknownRegister register ->
            String.concat [ "Unknown register $", register ]


runTimeErrorToString : RuntimeError -> String
runTimeErrorToString err =
    case err of
        UndefinedRegister register ->
            String.concat [ "Undefined register $", register ]

        UndefinedOperation operationName ->
            String.concat [ "Undefined operation ", operationName ]

        WrongNumberOfArgumentsGivenToOperationExpected int ->
            String.concat [ "Wrong number of arguments given to the operation. Expected ", String.fromInt int ]

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

        InstructionPointerOutOfBounds { instructionPointer, numberOfInstructions } ->
            String.concat [ "The instruction pointer @", String.fromInt instructionPointer, " is out of bounds (number of instructions is ", String.fromInt numberOfInstructions, ")" ]
