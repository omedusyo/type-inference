module Ui.Tab.RegisterMachine exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionAddress, MemoryAddress, Value(..))
import RegisterMachine.Controllers as Controllers
import RegisterMachine.GarbageCollector as GarbageCollector
import RegisterMachine.Machine as RegisterMachine exposing (Controller, Machine, RuntimeError(..), TranslationError)
import RegisterMachine.MemoryState as MemoryState exposing (MemoryCell, MemoryError(..), MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.Style.Button as Button


type alias Model =
    { controller : Controller
    , parsedMachine : Result TranslationError Machine
    , maybeRuntime : Maybe (Result RuntimeError Machine)
    , memoryView : MemoryView
    , currentlyHighlightedCell : MemoryAddress
    }


init : InitContext Model Msg
init =
    let
        boolToInt : Bool -> Int
        boolToInt b =
            if b then
                1

            else
                0

        operationEnv : RegisterMachine.OperationEnvironment
        operationEnv =
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

        ( controller, env ) =
            -- Controllers.controller0_gcd
            -- Controllers.controller1_remainder
            -- Controllers.controller2_fct_iterative
            -- Controllers.controller3_gcd_with_inlined_remainder
            -- Controllers.controller4_gcd_with_inlined_remainder_using_jump
            -- Controllers.controller6_fct_recursive
            -- Controllers.controller7_fibonacci_recursive
            -- Controllers.controller8_memory_test
            -- Controllers.controller9_range
            -- Controllers.controller10_append
            GarbageCollector.controller

        parsedMachine : Result TranslationError Machine
        parsedMachine =
            RegisterMachine.makeMachine controller env operationEnv
    in
    InitContext.setModelTo
        { controller = controller
        , parsedMachine = parsedMachine
        , maybeRuntime =
            case parsedMachine of
                Ok machine ->
                    Just (Ok machine)

                Err _ ->
                    Nothing
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


resetRuntime : Model -> Model
resetRuntime model =
    { model
        | maybeRuntime =
            case model.parsedMachine of
                Ok machine ->
                    Just (Ok machine)

                Err _ ->
                    Nothing
    }


type Msg
    = Reset
    | Start
    | RunOneStep
    | MemoryAddressClicked MemoryAddress
    | ShiftMemoryViewBy Int


update : Msg -> Context Model msg
update msg =
    case msg of
        Reset ->
            Context.update resetRuntime

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

        MemoryAddressClicked p ->
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


view : Config -> Model -> Element Msg
view config model =
    -- 1. I need to display all the registers
    -- 2. I need to display the instruction block with labels
    E.column [ E.width E.fill ]
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

            Just (Ok machine) ->
                E.column [ E.width E.fill ]
                    [ E.row [ E.width E.fill ]
                        [ viewRegisters (machine.env |> Dict.toList) model
                        , viewStack machine.stack model
                        , -- instructions
                          viewInstructions machine.instructionPointer model.controller.instructions
                        ]
                    , viewMemoryState (machine |> RegisterMachine.currentMemoryState RegisterMachine.Main) model
                    , viewMemoryState (machine |> RegisterMachine.currentMemoryState RegisterMachine.Dual) model
                    ]
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

        ExpectedInstructionAddressInRegister ->
            "Expected instruction address in the register"

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
                    RegisterMachine.AssignRegister target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewRegisterUse source ]

                    RegisterMachine.AssignLabel target label ->
                        [ viewRegisterName target, viewInstructionName "<-", viewLabelUse label ]

                    RegisterMachine.AssignOperation target (RegisterMachine.Operation opName args) ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication opName args ]

                    RegisterMachine.AssignConstant target x ->
                        [ viewRegisterName target, viewInstructionName "<-", viewConstant x ]

                    RegisterMachine.JumpToLabel label ->
                        [ viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToLabelAtRegister target ->
                        [ viewInstructionName "jump", viewRegisterUse target ]

                    RegisterMachine.JumpToLabelIf testRegister label ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToLabelAtRegisterIf testRegister target ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewRegisterUse target ]

                    RegisterMachine.Halt ->
                        [ viewInstructionName "halt" ]

                    RegisterMachine.PushRegister register ->
                        [ viewInstructionName "push", viewRegisterUse register ]

                    RegisterMachine.PushConstant val ->
                        [ viewInstructionName "push", viewConstant val ]

                    RegisterMachine.PushLabel label ->
                        [ viewInstructionName "push", viewLabelUse label ]

                    RegisterMachine.Pop target ->
                        [ viewRegisterName target, viewInstructionName "<-", viewInstructionName "pop-stack" ]

                    RegisterMachine.AssignCallAtLabel target label ->
                        [ viewRegisterName target, viewInstructionName "<-", viewInstructionName "call", viewLabelUse label ]

                    RegisterMachine.AssignCallAtRegister target labelRegister ->
                        [ viewRegisterName target, viewInstructionName "<-", viewInstructionName "call", viewRegisterUse labelRegister ]

                    RegisterMachine.ConstructPair target arg0 arg1 ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "pair" [ arg0, arg1 ] ]

                    RegisterMachine.First target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "first" [ RegisterMachine.Register source ] ]

                    RegisterMachine.Second target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "second" [ RegisterMachine.Register source ] ]

                    RegisterMachine.SetFirst register arg ->
                        [ viewInstructionName "set-first", viewRegisterName register, viewOperationArgument arg ]

                    RegisterMachine.SetSecond register arg ->
                        [ viewInstructionName "set-second", viewRegisterName register, viewOperationArgument arg ]

                    RegisterMachine.DualFirst target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "dual-first" [ RegisterMachine.Register source ] ]

                    RegisterMachine.DualSecond target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "dual-second" [ RegisterMachine.Register source ] ]

                    RegisterMachine.DualSetFirst register arg ->
                        [ viewInstructionName "dual-set-first", viewRegisterName register, viewOperationArgument arg ]

                    RegisterMachine.DualSetSecond register arg ->
                        [ viewInstructionName "dual-set-second", viewRegisterName register, viewOperationArgument arg ]

                    RegisterMachine.MoveToDual target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication "move-to-dual" [ RegisterMachine.Register source ] ]

                    RegisterMachine.MarkAsMoved toBeCollected referenceToDualMemory ->
                        [ viewInstructionName "mark", viewRegisterUse toBeCollected, viewInstructionName "as-moved-to", viewRegisterUse referenceToDualMemory ]

                    RegisterMachine.SwapMemory ->
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
    E.column [ E.width E.fill, E.spacing 5 ]
        (stack
            |> Stack.toList
            |> List.reverse
            |> List.map
                (\val ->
                    viewValue val model
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
        [ E.text "Memory"
        , E.row [] [ E.text "Next free address: ", viewMemoryAddress memoryState.nextFreePointer ]
        , E.row []
            [ Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy -1), label = E.text "-1" }
            , Input.button Button.buttonStyle { onPress = Just (ShiftMemoryViewBy 1), label = E.text "+1" }
            ]
        , viewMemorySegment model.memoryView.bottom model.memoryView.top
        ]


viewMemoryCell : MemoryAddress -> MemoryCell -> Model -> Element Msg
viewMemoryCell memoryAddress ( a, b ) model =
    E.column [ Border.solid, Border.width 1, E.width (E.px 100) ]
        [ E.column [ E.centerX, E.paddingXY 0 15, E.height (E.px 80) ]
            [ viewValue a model
            , viewValue b model
            ]
        , E.el
            (List.concat
                [ [ E.centerX, E.paddingXY 0 5 ]
                , if model.currentlyHighlightedCell == memoryAddress then
                    [ Font.color (E.rgb255 255 0 0) ]

                  else
                    []
                ]
            )
            (E.text (String.concat [ "#", String.fromInt memoryAddress ]))
        ]


viewValue : Value -> Model -> Element Msg
viewValue value model =
    case value of
        ConstantValue constant ->
            viewConstant constant

        Pair memoryAddress ->
            viewMemoryAddress memoryAddress

        InstructionAddress instructionAddress ->
            viewInstructionAddress instructionAddress

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


viewMemoryAddress : MemoryAddress -> Element Msg
viewMemoryAddress p =
    E.el [ Events.onClick (MemoryAddressClicked p), E.pointer ]
        (E.text (String.concat [ "#", String.fromInt p ]))


viewInstructionAddress : InstructionAddress -> Element Msg
viewInstructionAddress pointer =
    E.text (String.concat [ ":", String.fromInt pointer ])
