module RegisterMachine.Machine exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lib.Result as Result
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), Instruction(..), InstructionAddress, Label, MemoryAddress, OperationApplication(..), OperationArgument(..), OperationName, Register, Value(..))
import RegisterMachine.MemoryState as MemoryState exposing (MemoryError, MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Set exposing (Set)


type alias ControllerExample =
    { name : String
    , controller : Controller
    , initialRegisterEnvironment : RegisterEnvironment
    }


type alias MachineState =
    { env : RegisterEnvironment
    , stack : Stack
    , memory : MachineMemory
    , operationEnv : OperationEnvironment
    }


type alias MachineWithInstructions =
    { machineState : MachineState, instructionsState : InstructionsState }


type alias InstructionsState =
    { instructionPointer : InstructionAddress
    , instructions : MachineInstructions
    }


type alias MachineInstructions =
    { labels : Set Label
    , labelToPosition : Dict Label InstructionAddress
    , instructions : Array Instruction
    }


type LabelOrInstruction
    = Perform Instruction
    | Label Label


type alias InstructionBlock =
    List LabelOrInstruction


type alias RegisterEnvironment =
    Dict Register Value


type alias Operation =
    List Value -> Result RuntimeError Value


type alias OperationEnvironment =
    Dict OperationName Operation


type Two
    = Zero
    | One


type alias MachineMemory =
    { memoryState0 : MemoryState
    , memoryState1 : MemoryState
    , memoryInUse : Two
    }


type MemoryCellComponent
    = FirstComponent
    | SecondComponent


type MemoryType
    = Main
    | Dual


type TranslationError
    = LabelUsedMoreThanOnce Label
    | UnknownRegister Register



-- A controller is a description of a register machine.
-- Given a controller, we should be able to construct a register machine that is governed by said controller.


type alias Controller =
    -- TODO: Should we have typed registers? Int/Bool? This will complicate the operations though...
    { registers : Set Register
    , instructions : InstructionBlock
    }



-- ===Machine Construction===
-- helper


foldlMayFail : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlMayFail update state actions0 =
    case actions0 of
        [] ->
            Ok state

        action :: actions1 ->
            update action state
                |> Result.andThen (\newState -> foldlMayFail update newState actions1)


checkRegisters : List Register -> List OperationArgument -> Controller -> Result TranslationError ()
checkRegisters registers arguments controller =
    let
        checkRegister : Register -> Result TranslationError ()
        checkRegister register =
            if Set.member register controller.registers then
                Ok ()

            else
                Err (UnknownRegister register)

        checkArg : OperationArgument -> Result TranslationError ()
        checkArg arg =
            case arg of
                Register register ->
                    checkRegister register

                _ ->
                    Ok ()
    in
    Result.tuple2
        (registers |> List.map checkRegister |> Result.sequence)
        (arguments |> List.map checkArg |> Result.sequence)
        |> Result.ignore


parse : Controller -> Result TranslationError MachineInstructions
parse controller =
    let
        checkRegisterUse : Instruction -> Result TranslationError ()
        checkRegisterUse instruction =
            case instruction of
                AssignRegister target source ->
                    controller |> checkRegisters [ target, source ] []

                AssignLabel target _ ->
                    controller |> checkRegisters [ target ] []

                AssignOperation target (Operation _ arguments) ->
                    controller |> checkRegisters [] arguments

                AssignConstant target _ ->
                    controller |> checkRegisters [ target ] []

                JumpToLabel label ->
                    Ok ()

                JumpToLabelAtRegister target ->
                    controller |> checkRegisters [ target ] []

                JumpToLabelIf testRegister _ ->
                    controller |> checkRegisters [ testRegister ] []

                JumpToLabelAtRegisterIf testRegister target ->
                    controller |> checkRegisters [ testRegister, target ] []

                Halt ->
                    Ok ()

                PushRegister register ->
                    controller |> checkRegisters [ register ] []

                PushConstant _ ->
                    Ok ()

                PushLabel label ->
                    Ok ()

                Pop target ->
                    controller |> checkRegisters [ target ] []

                AssignCallAtLabel target _ ->
                    controller |> checkRegisters [ target ] []

                AssignCallAtRegister target labelRegister ->
                    controller |> checkRegisters [ target, labelRegister ] []

                ConstructPair target arg0 arg1 ->
                    controller |> checkRegisters [ target ] [ arg0, arg1 ]

                First target source ->
                    controller |> checkRegisters [ target, source ] []

                Second target source ->
                    controller |> checkRegisters [ target, source ] []

                SetFirst target arg ->
                    controller |> checkRegisters [ target ] [ arg ]

                SetSecond target arg ->
                    controller |> checkRegisters [ target ] [ arg ]

                DualFirst target source ->
                    controller |> checkRegisters [ target, source ] []

                DualSecond target source ->
                    controller |> checkRegisters [ target, source ] []

                DualSetFirst target arg ->
                    controller |> checkRegisters [ target ] [ arg ]

                DualSetSecond target arg ->
                    controller |> checkRegisters [ target ] [ arg ]

                MoveToDual target source ->
                    controller |> checkRegisters [ target, source ] []

                MarkAsMoved toBeCollected referenceToDualMemory ->
                    controller |> checkRegisters [ toBeCollected, referenceToDualMemory ] []

                SwapMemory ->
                    Ok ()

        initMachineInstructions : ( InstructionAddress, MachineInstructions )
        initMachineInstructions =
            ( 0
            , { labels = Set.empty
              , labelToPosition = Dict.empty
              , instructions = Array.empty
              }
            )

        update : LabelOrInstruction -> ( InstructionAddress, MachineInstructions ) -> Result TranslationError ( InstructionAddress, MachineInstructions )
        update labelOrInstruction ( pointer, machineInstructions ) =
            case labelOrInstruction of
                Label label ->
                    if Set.member label machineInstructions.labels then
                        Err (LabelUsedMoreThanOnce label)

                    else
                        Ok
                            ( pointer
                            , { machineInstructions
                                | labels = Set.insert label machineInstructions.labels
                                , labelToPosition =
                                    Dict.insert label pointer machineInstructions.labelToPosition
                              }
                            )

                Perform instruction ->
                    Ok
                        ( pointer + 1
                        , { machineInstructions
                            | instructions =
                                Array.push instruction machineInstructions.instructions
                          }
                        )
    in
    foldlMayFail update initMachineInstructions controller.instructions
        |> Result.map Tuple.second


makeMachine : Controller -> RegisterEnvironment -> OperationEnvironment -> Result TranslationError MachineWithInstructions
makeMachine controller env operationsEnv =
    parse controller
        |> Result.map
            (\instructions ->
                { machineState =
                    { env = env
                    , operationEnv = operationsEnv
                    , memory =
                        -- TODO
                        { memoryState0 = MemoryState.empty 4096

                        -- MemoryState.example0
                        , memoryState1 = MemoryState.empty 4096
                        , memoryInUse = Zero
                        }
                    , stack = Stack.empty
                    }
                , instructionsState =
                    { instructionPointer = 0
                    , instructions = instructions
                    }
                }
            )



-- === Machine ===


makeOperation2 : (Value -> Value -> Result RuntimeError Value) -> Operation
makeOperation2 op =
    \xs ->
        case xs of
            [ x, y ] ->
                op x y

            _ ->
                Err (WrongNumberOfArgumentsGivenToOperationExpected 2)


makeNumOperation2 : (Int -> Int -> Int) -> Operation
makeNumOperation2 op =
    makeOperation2
        (\v0 v1 ->
            case ( v0, v1 ) of
                ( ConstantValue (Num x), ConstantValue (Num y) ) ->
                    Ok (ConstantValue (Num (op x y)))

                _ ->
                    Err TheOperationExpectsIntegerArguments
        )


makeOperation1 : (Value -> Result RuntimeError Value) -> Operation
makeOperation1 op =
    \xs ->
        case xs of
            [ x ] ->
                op x

            _ ->
                Err (WrongNumberOfArgumentsGivenToOperationExpected 1)


makeNumOperation1 : (Int -> Int) -> Operation
makeNumOperation1 op =
    makeOperation1
        (\v ->
            case v of
                ConstantValue (Num x) ->
                    Ok (ConstantValue (Num (op x)))

                _ ->
                    Err TheOperationExpectsIntegerArguments
        )


getInstruction : InstructionsState -> Maybe Instruction
getInstruction instructionsState =
    Array.get instructionsState.instructionPointer instructionsState.instructions.instructions


advanceInstructionPointer : InstructionsState -> InstructionsState
advanceInstructionPointer instructionsState =
    { instructionsState
        | instructionPointer = instructionsState.instructionPointer + 1
    }


getRegister : Register -> MachineState -> Result RuntimeError Value
getRegister register machine =
    case Dict.get register machine.env of
        Just val ->
            Ok val

        Nothing ->
            Err (UndefinedRegister register)


getValueFromArgument : OperationArgument -> MachineState -> Result RuntimeError Value
getValueFromArgument argument machine =
    case argument of
        Register register ->
            getRegister register machine

        Constant val ->
            Ok (ConstantValue val)


getInstructionAddressAtRegister : Register -> MachineState -> Result RuntimeError InstructionAddress
getInstructionAddressAtRegister register machine =
    machine
        |> getRegister register
        |> Result.andThen
            (\value ->
                case value of
                    InstructionAddress pointer ->
                        Ok pointer

                    _ ->
                        Err ExpectedInstructionAddressInRegister
            )


getMemoryAddressAtRegister : Register -> MachineState -> Result RuntimeError MemoryAddress
getMemoryAddressAtRegister register machine =
    machine
        |> getRegister register
        |> Result.andThen
            (\value ->
                case value of
                    Pair pointer ->
                        Ok pointer

                    _ ->
                        Err ExpectedPairInRegister
            )


updateRegister : Register -> Value -> MachineState -> MachineState
updateRegister register val machine =
    { machine
        | env = Dict.insert register val machine.env
    }


setMemoryStateOfMachine : MemoryType -> MemoryState -> MachineState -> MachineState
setMemoryStateOfMachine memoryType memoryState ({ memory } as machine) =
    case ( memoryType, memory.memoryInUse ) of
        ( Main, Zero ) ->
            { machine | memory = { memory | memoryState0 = memoryState } }

        ( Main, One ) ->
            { machine | memory = { memory | memoryState1 = memoryState } }

        ( Dual, Zero ) ->
            { machine | memory = { memory | memoryState1 = memoryState } }

        ( Dual, One ) ->
            { machine | memory = { memory | memoryState0 = memoryState } }


setDualMemoryStateOfMachine : MemoryState -> MachineState -> MachineState
setDualMemoryStateOfMachine memoryState ({ memory } as machine) =
    case memory.memoryInUse of
        Zero ->
            { machine | memory = { memory | memoryState1 = memoryState } }

        One ->
            { machine | memory = { memory | memoryState0 = memoryState } }


accessPair : MemoryCellComponent -> MemoryType -> Register -> Register -> MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
accessPair memoryCellComponent memoryType target source { machineState, instructionsState } =
    machineState
        |> getMemoryAddressAtRegister source
        |> Result.andThen
            (\pointer ->
                machineState
                    |> currentMemoryState memoryType
                    |> MemoryState.get pointer
                    |> Result.mapError MemoryError
                    |> Result.map
                        (\( a, b ) ->
                            { isFinished = False
                            , machine =
                                { machineState =
                                    machineState
                                        |> (case memoryCellComponent of
                                                FirstComponent ->
                                                    updateRegister target a

                                                SecondComponent ->
                                                    updateRegister target b
                                           )
                                , instructionsState = instructionsState |> advanceInstructionPointer
                                }
                            }
                        )
            )


setPair : MemoryCellComponent -> MemoryType -> Register -> OperationArgument -> MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
setPair memoryCellComponent memoryType register arg { machineState, instructionsState } =
    Result.tuple2 (machineState |> getValueFromArgument arg) (machineState |> getMemoryAddressAtRegister register)
        |> Result.map
            (\( val, pointer ) ->
                { isFinished = False
                , machine =
                    { machineState =
                        machineState
                            |> setMemoryStateOfMachine
                                memoryType
                                (MemoryState.update pointer
                                    (\( a, b ) ->
                                        case memoryCellComponent of
                                            FirstComponent ->
                                                ( val, b )

                                            SecondComponent ->
                                                ( a, val )
                                    )
                                    (machineState |> currentMemoryState memoryType)
                                )
                    , instructionsState = instructionsState |> advanceInstructionPointer
                    }
                }
            )


currentMemoryState : MemoryType -> MachineState -> MemoryState
currentMemoryState memoryType machine =
    case ( memoryType, machine.memory.memoryInUse ) of
        ( Main, Zero ) ->
            machine.memory.memoryState0

        ( Main, One ) ->
            machine.memory.memoryState1

        ( Dual, Zero ) ->
            machine.memory.memoryState1

        ( Dual, One ) ->
            machine.memory.memoryState0


getOperation : OperationName -> MachineState -> Result RuntimeError (List Value -> Result RuntimeError Value)
getOperation operationName machine =
    case Dict.get operationName machine.operationEnv of
        Just op ->
            Ok op

        Nothing ->
            Err (UndefinedOperation operationName)


getLabelPosition : Label -> InstructionsState -> Maybe InstructionAddress
getLabelPosition label instructionsState =
    Dict.get label instructionsState.instructions.labelToPosition


jump : Label -> InstructionsState -> InstructionsState
jump label instructionsState =
    case getLabelPosition label instructionsState of
        Just pointer ->
            pointerJump pointer instructionsState

        Nothing ->
            instructionsState


pointerJump : InstructionAddress -> InstructionsState -> InstructionsState
pointerJump pointer instructionsState =
    { instructionsState
        | instructionPointer = pointer
    }


halt : MachineWithInstructions -> { isFinished : Bool, machine : MachineWithInstructions }
halt machine =
    { isFinished = True, machine = machine }


push : Value -> MachineState -> MachineState
push val machine =
    { machine
        | stack = Stack.push val machine.stack
    }


pop : MachineState -> Result RuntimeError ( Value, MachineState )
pop machine =
    case Stack.pop machine.stack of
        Just ( val, stack ) ->
            Ok ( val, { machine | stack = stack } )

        Nothing ->
            Err PoppingEmptyStack


swapMemory : MachineState -> MachineState
swapMemory ({ memory } as machine) =
    { machine
        | memory =
            { memory
                | memoryInUse =
                    case memory.memoryInUse of
                        Zero ->
                            One

                        One ->
                            Zero
            }
    }


type RuntimeError
    = UndefinedRegister Register
    | UndefinedOperation OperationName
    | WrongNumberOfArgumentsGivenToOperationExpected Int
    | TheOperationExpectsIntegerArguments
    | LabelPointsToNothing Label
    | PoppingEmptyStack
    | ExpectedInstructionAddressInRegister
    | ExpectedPairInRegister
    | MemoryError MemoryError


runOneStep : MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
runOneStep ({ machineState, instructionsState } as machine) =
    case getInstruction instructionsState of
        Just instruction ->
            case instruction of
                AssignRegister target source ->
                    getRegister source machineState
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister target val
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                AssignLabel target label ->
                    -- TODO: There could be an off-by-one error, where the label points to Nothing, but actually it is the halting label?
                    case getLabelPosition label instructionsState of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister target (InstructionAddress pointer)
                                    , instructionsState = instructionsState |> advanceInstructionPointer
                                    }
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                AssignOperation target (Operation opName args) ->
                    let
                        applyOp : Operation -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
                        applyOp op =
                            args
                                |> List.map (\argument -> machineState |> getValueFromArgument argument)
                                |> Result.sequence
                                |> Result.andThen op
                                |> Result.map
                                    (\output ->
                                        { isFinished = False
                                        , machine =
                                            { machineState =
                                                machineState
                                                    |> updateRegister target output
                                            , instructionsState = instructionsState |> advanceInstructionPointer
                                            }
                                        }
                                    )
                    in
                    getOperation opName machineState
                        |> Result.andThen applyOp

                AssignConstant target x ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> updateRegister target (ConstantValue x)
                            , instructionsState = instructionsState |> advanceInstructionPointer
                            }
                        }

                JumpToLabel label ->
                    Ok { isFinished = False, machine = { machineState = machineState, instructionsState = jump label instructionsState } }

                JumpToLabelAtRegister register ->
                    getInstructionAddressAtRegister register machineState
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine = { machineState = machineState, instructionsState = pointerJump pointer instructionsState }
                                }
                            )

                JumpToLabelIf testRegister label ->
                    getRegister testRegister machineState
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    if val == ConstantValue (Num 1) then
                                        { machineState = machineState, instructionsState = jump label instructionsState }

                                    else
                                        { machineState = machineState, instructionsState = advanceInstructionPointer instructionsState }
                                }
                            )

                JumpToLabelAtRegisterIf testRegister target ->
                    getRegister testRegister machineState
                        |> Result.andThen
                            (\val ->
                                if val == ConstantValue (Num 1) then
                                    getInstructionAddressAtRegister target machineState
                                        |> Result.map
                                            (\pointer ->
                                                { isFinished = False
                                                , machine =
                                                    { machineState = machineState
                                                    , instructionsState = pointerJump pointer instructionsState
                                                    }
                                                }
                                            )

                                else
                                    Ok
                                        { isFinished = False
                                        , machine = { machineState = machineState, instructionsState = advanceInstructionPointer instructionsState }
                                        }
                            )

                Halt ->
                    Ok (halt machine)

                PushRegister register ->
                    getRegister register machineState
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> push val
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                PushConstant val ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> push (ConstantValue val)
                            , instructionsState =
                                instructionsState |> advanceInstructionPointer
                            }
                        }

                PushLabel label ->
                    case getLabelPosition label instructionsState of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> push (InstructionAddress pointer)
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                Pop target ->
                    pop machineState
                        |> Result.map
                            (\( val, newMachineState ) ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        newMachineState
                                            |> updateRegister target val
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                AssignCallAtLabel target label ->
                    -- target <- $ip + 1
                    -- ip <- :label
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> updateRegister target (InstructionAddress (instructionsState.instructionPointer + 1))
                            , instructionsState =
                                instructionsState |> jump label
                            }
                        }

                AssignCallAtRegister target labelRegister ->
                    -- target <- $ip + 1
                    -- ip <- $labelRegister
                    getInstructionAddressAtRegister labelRegister machineState
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister target (InstructionAddress (instructionsState.instructionPointer + 1))
                                    , instructionsState =
                                        instructionsState |> pointerJump pointer
                                    }
                                }
                            )

                ConstructPair target arg0 arg1 ->
                    Result.tuple2 (machineState |> getValueFromArgument arg0) (machineState |> getValueFromArgument arg1)
                        |> Result.andThen
                            (\( value0, value1 ) ->
                                (machineState |> currentMemoryState Main |> MemoryState.new ( value0, value1 ))
                                    |> Result.mapError MemoryError
                                    |> Result.map
                                        (\( newPairAddress, newMemoryState ) ->
                                            { isFinished = False
                                            , machine =
                                                { machineState =
                                                    machineState
                                                        |> setMemoryStateOfMachine Main newMemoryState
                                                        |> updateRegister target (Pair newPairAddress)
                                                , instructionsState =
                                                    instructionsState |> advanceInstructionPointer
                                                }
                                            }
                                        )
                            )

                First target source ->
                    machine |> accessPair FirstComponent Main target source

                Second target source ->
                    machine |> accessPair SecondComponent Main target source

                SetFirst register arg ->
                    machine |> setPair FirstComponent Main register arg

                SetSecond register arg ->
                    machine |> setPair SecondComponent Main register arg

                DualFirst target source ->
                    machine |> accessPair FirstComponent Dual target source

                DualSecond target source ->
                    machine |> accessPair SecondComponent Dual target source

                DualSetFirst register arg ->
                    machine |> setPair FirstComponent Dual register arg

                DualSetSecond register arg ->
                    machine |> setPair SecondComponent Dual register arg

                MoveToDual target source ->
                    machineState
                        |> getMemoryAddressAtRegister source
                        |> Result.andThen
                            (\sourceAddress ->
                                machineState
                                    |> currentMemoryState Main
                                    |> MemoryState.get sourceAddress
                                    |> Result.mapError MemoryError
                                    |> Result.andThen
                                        (\memoryCell ->
                                            machineState
                                                |> currentMemoryState Dual
                                                |> MemoryState.new memoryCell
                                                |> Result.mapError MemoryError
                                        )
                                    |> Result.map
                                        (\( addressOfNewPair, newDualMemoryState ) ->
                                            { isFinished = False
                                            , machine =
                                                { machineState =
                                                    machineState
                                                        |> setDualMemoryStateOfMachine newDualMemoryState
                                                        |> updateRegister target (Pair addressOfNewPair)
                                                , instructionsState =
                                                    instructionsState |> advanceInstructionPointer
                                                }
                                            }
                                        )
                            )

                MarkAsMoved toBeCollected referenceToDualMemory ->
                    Result.tuple2 (machineState |> getMemoryAddressAtRegister toBeCollected) (machineState |> getMemoryAddressAtRegister referenceToDualMemory)
                        |> Result.map
                            (\( addressToBeCollected, addressToDualMemory ) ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> setMemoryStateOfMachine Main
                                                (machineState
                                                    |> currentMemoryState Main
                                                    |> MemoryState.set addressToBeCollected ( Moved, Pair addressToDualMemory )
                                                )
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                SwapMemory ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> swapMemory
                            , instructionsState =
                                instructionsState |> advanceInstructionPointer
                            }
                        }

        Nothing ->
            Ok (halt machine)


evolve : Int -> MachineWithInstructions -> Result RuntimeError MachineWithInstructions
evolve n machine0 =
    if n == 0 then
        Ok machine0

    else
        runOneStep machine0
            |> Result.andThen
                (\{ machine } ->
                    evolve (n - 1) machine
                )


start : MachineWithInstructions -> Result RuntimeError MachineWithInstructions
start machine0 =
    runOneStep machine0
        |> Result.andThen
            (\{ isFinished, machine } ->
                if isFinished then
                    Ok machine

                else
                    start machine
            )
