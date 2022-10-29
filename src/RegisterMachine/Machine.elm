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


type alias Machine =
    { env : RegisterEnvironment
    , stack : Stack
    , memory : MachineMemory
    , operationEnv : OperationEnvironment
    , instructionPointer : InstructionAddress
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


makeMachine : Controller -> RegisterEnvironment -> OperationEnvironment -> Result TranslationError Machine
makeMachine controller env operationsEnv =
    parse controller
        |> Result.map
            (\instructions ->
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
                , instructionPointer = 0
                , instructions = instructions
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


getInstruction : Machine -> Maybe Instruction
getInstruction machine =
    Array.get machine.instructionPointer machine.instructions.instructions


advanceInstructionPointer : Machine -> Machine
advanceInstructionPointer machine =
    { machine
        | instructionPointer = machine.instructionPointer + 1
    }


getRegister : Register -> Machine -> Result RuntimeError Value
getRegister register machine =
    case Dict.get register machine.env of
        Just val ->
            Ok val

        Nothing ->
            Err (UndefinedRegister register)


getValueFromArgument : OperationArgument -> Machine -> Result RuntimeError Value
getValueFromArgument argument machine =
    case argument of
        Register register ->
            getRegister register machine

        Constant val ->
            Ok (ConstantValue val)


getInstructionAddressAtRegister : Register -> Machine -> Result RuntimeError InstructionAddress
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


getMemoryAddressAtRegister : Register -> Machine -> Result RuntimeError MemoryAddress
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


updateRegister : Register -> Value -> Machine -> Machine
updateRegister register val machine =
    { machine
        | env = Dict.insert register val machine.env
    }


setMemoryStateOfMachine : MemoryType -> MemoryState -> Machine -> Machine
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


setDualMemoryStateOfMachine : MemoryState -> Machine -> Machine
setDualMemoryStateOfMachine memoryState ({ memory } as machine) =
    case memory.memoryInUse of
        Zero ->
            { machine | memory = { memory | memoryState1 = memoryState } }

        One ->
            { machine | memory = { memory | memoryState0 = memoryState } }


accessPair : MemoryCellComponent -> MemoryType -> Register -> Register -> Machine -> Result RuntimeError { isFinished : Bool, machine : Machine }
accessPair memoryCellComponent memoryType target source machine =
    machine
        |> getMemoryAddressAtRegister source
        |> Result.andThen
            (\pointer ->
                machine
                    |> currentMemoryState memoryType
                    |> MemoryState.get pointer
                    |> Result.mapError MemoryError
                    |> Result.map
                        (\( a, b ) ->
                            { isFinished = False
                            , machine =
                                machine
                                    |> (case memoryCellComponent of
                                            FirstComponent ->
                                                updateRegister target a

                                            SecondComponent ->
                                                updateRegister target b
                                       )
                                    |> advanceInstructionPointer
                            }
                        )
            )


setPair : MemoryCellComponent -> MemoryType -> Register -> OperationArgument -> Machine -> Result RuntimeError { isFinished : Bool, machine : Machine }
setPair memoryCellComponent memoryType register arg machine =
    Result.tuple2 (machine |> getValueFromArgument arg) (machine |> getMemoryAddressAtRegister register)
        |> Result.map
            (\( val, pointer ) ->
                { isFinished = False
                , machine =
                    machine
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
                                (machine |> currentMemoryState memoryType)
                            )
                        |> advanceInstructionPointer
                }
            )


currentMemoryState : MemoryType -> Machine -> MemoryState
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


getOperation : OperationName -> Machine -> Result RuntimeError (List Value -> Result RuntimeError Value)
getOperation operationName machine =
    case Dict.get operationName machine.operationEnv of
        Just op ->
            Ok op

        Nothing ->
            Err (UndefinedOperation operationName)


getLabelPosition : Label -> Machine -> Maybe InstructionAddress
getLabelPosition label machine =
    Dict.get label machine.instructions.labelToPosition


jump : Label -> Machine -> Machine
jump label machine =
    case getLabelPosition label machine of
        Just pointer ->
            pointerJump pointer machine

        Nothing ->
            machine


pointerJump : InstructionAddress -> Machine -> Machine
pointerJump pointer machine =
    { machine
        | instructionPointer = pointer
    }


halt : Machine -> { isFinished : Bool, machine : Machine }
halt machine =
    { isFinished = True, machine = machine }


push : Value -> Machine -> Machine
push val machine =
    { machine
        | stack = Stack.push val machine.stack
    }


pop : Machine -> Result RuntimeError ( Value, Machine )
pop machine =
    case Stack.pop machine.stack of
        Just ( val, stack ) ->
            Ok ( val, { machine | stack = stack } )

        Nothing ->
            Err PoppingEmptyStack


swapMemory : Machine -> Machine
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


runOneStep : Machine -> Result RuntimeError { isFinished : Bool, machine : Machine }
runOneStep machine =
    case getInstruction machine of
        Just instruction ->
            case instruction of
                AssignRegister target source ->
                    getRegister source machine
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> updateRegister target val
                                        |> advanceInstructionPointer
                                }
                            )

                AssignLabel target label ->
                    -- TODO: There could be an off-by-one error, where the label points to Nothing, but actually it is the halting label?
                    case getLabelPosition label machine of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    machine
                                        |> updateRegister target (InstructionAddress pointer)
                                        |> advanceInstructionPointer
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                AssignOperation target (Operation opName args) ->
                    let
                        applyOp : Operation -> Result RuntimeError { isFinished : Bool, machine : Machine }
                        applyOp op =
                            args
                                |> List.map (\argument -> machine |> getValueFromArgument argument)
                                |> Result.sequence
                                |> Result.andThen op
                                |> Result.map
                                    (\output ->
                                        { isFinished = False
                                        , machine =
                                            machine
                                                |> updateRegister target output
                                                |> advanceInstructionPointer
                                        }
                                    )
                    in
                    getOperation opName machine
                        |> Result.andThen applyOp

                AssignConstant target x ->
                    Ok
                        { isFinished = False
                        , machine =
                            machine
                                |> updateRegister target (ConstantValue x)
                                |> advanceInstructionPointer
                        }

                JumpToLabel label ->
                    Ok { isFinished = False, machine = jump label machine }

                JumpToLabelAtRegister register ->
                    getInstructionAddressAtRegister register machine
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine = pointerJump pointer machine
                                }
                            )

                JumpToLabelIf testRegister label ->
                    getRegister testRegister machine
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    if val == ConstantValue (Num 1) then
                                        jump label machine

                                    else
                                        advanceInstructionPointer machine
                                }
                            )

                JumpToLabelAtRegisterIf testRegister target ->
                    getRegister testRegister machine
                        |> Result.andThen
                            (\val ->
                                if val == ConstantValue (Num 1) then
                                    getInstructionAddressAtRegister target machine
                                        |> Result.map
                                            (\pointer ->
                                                { isFinished = False
                                                , machine = pointerJump pointer machine
                                                }
                                            )

                                else
                                    Ok
                                        { isFinished = False
                                        , machine = advanceInstructionPointer machine
                                        }
                            )

                Halt ->
                    Ok (halt machine)

                PushRegister register ->
                    getRegister register machine
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> push val
                                        |> advanceInstructionPointer
                                }
                            )

                PushConstant val ->
                    Ok
                        { isFinished = False
                        , machine =
                            machine
                                |> push (ConstantValue val)
                                |> advanceInstructionPointer
                        }

                PushLabel label ->
                    case getLabelPosition label machine of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    machine
                                        |> push (InstructionAddress pointer)
                                        |> advanceInstructionPointer
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                Pop target ->
                    pop machine
                        |> Result.map
                            (\( val, newMachine ) ->
                                { isFinished = False
                                , machine =
                                    newMachine
                                        |> updateRegister target val
                                        |> advanceInstructionPointer
                                }
                            )

                AssignCallAtLabel target label ->
                    -- target <- $ip + 1
                    -- ip <- :label
                    Ok
                        { isFinished = False
                        , machine =
                            machine
                                |> updateRegister target (InstructionAddress (machine.instructionPointer + 1))
                                |> jump label
                        }

                AssignCallAtRegister target labelRegister ->
                    -- target <- $ip + 1
                    -- ip <- $labelRegister
                    getInstructionAddressAtRegister labelRegister machine
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> updateRegister target (InstructionAddress (machine.instructionPointer + 1))
                                        |> pointerJump pointer
                                }
                            )

                ConstructPair target arg0 arg1 ->
                    Result.tuple2 (machine |> getValueFromArgument arg0) (machine |> getValueFromArgument arg1)
                        |> Result.andThen
                            (\( value0, value1 ) ->
                                (machine |> currentMemoryState Main |> MemoryState.new ( value0, value1 ))
                                    |> Result.mapError MemoryError
                                    |> Result.map
                                        (\( newPairAddress, newMemoryState ) ->
                                            { isFinished = False
                                            , machine =
                                                machine
                                                    |> setMemoryStateOfMachine Main newMemoryState
                                                    |> updateRegister target (Pair newPairAddress)
                                                    |> advanceInstructionPointer
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
                    machine
                        |> getMemoryAddressAtRegister source
                        |> Result.andThen
                            (\sourceAddress ->
                                machine
                                    |> currentMemoryState Main
                                    |> MemoryState.get sourceAddress
                                    |> Result.mapError MemoryError
                                    |> Result.andThen
                                        (\memoryCell ->
                                            machine
                                                |> currentMemoryState Dual
                                                |> MemoryState.new memoryCell
                                                |> Result.mapError MemoryError
                                        )
                                    |> Result.map
                                        (\( addressOfNewPair, newDualMemoryState ) ->
                                            { isFinished = False
                                            , machine =
                                                machine
                                                    |> setDualMemoryStateOfMachine newDualMemoryState
                                                    |> updateRegister target (Pair addressOfNewPair)
                                                    |> advanceInstructionPointer
                                            }
                                        )
                            )

                MarkAsMoved toBeCollected referenceToDualMemory ->
                    Result.tuple2 (machine |> getMemoryAddressAtRegister toBeCollected) (machine |> getMemoryAddressAtRegister referenceToDualMemory)
                        |> Result.map
                            (\( addressToBeCollected, addressToDualMemory ) ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> setMemoryStateOfMachine Main
                                            (machine
                                                |> currentMemoryState Main
                                                |> MemoryState.set addressToBeCollected ( Moved, Pair addressToDualMemory )
                                            )
                                        |> advanceInstructionPointer
                                }
                            )

                SwapMemory ->
                    Ok
                        { isFinished = False
                        , machine =
                            machine
                                |> swapMemory
                                |> advanceInstructionPointer
                        }

        Nothing ->
            Ok (halt machine)


evolve : Int -> Machine -> Result RuntimeError Machine
evolve n machine0 =
    if n == 0 then
        Ok machine0

    else
        runOneStep machine0
            |> Result.andThen
                (\{ machine } ->
                    evolve (n - 1) machine
                )


start : Machine -> Result RuntimeError Machine
start machine0 =
    runOneStep machine0
        |> Result.andThen
            (\{ isFinished, machine } ->
                if isFinished then
                    Ok machine

                else
                    start machine
            )
