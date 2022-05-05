module RegisterMachine.Machine exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lib.Result as Result
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), Instruction(..), InstructionAddress, Label, MemoryAddress, OperationApplication(..), OperationArgument(..), OperationName, Register, Value(..))
import RegisterMachine.MemoryState as MemoryState exposing (MemoryError, MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Set exposing (Set)


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


doesRegisterExist : Register -> Controller -> Result TranslationError ()
doesRegisterExist register controller =
    if Set.member register controller.registers then
        Ok ()

    else
        Err (UnknownRegister register)


parse : Controller -> Result TranslationError MachineInstructions
parse controller =
    let
        checkArg : OperationArgument -> Result TranslationError ()
        checkArg arg =
            case arg of
                Register register ->
                    doesRegisterExist register controller

                _ ->
                    Ok ()

        checkRegisterUse : Instruction -> Result TranslationError ()
        checkRegisterUse instruction =
            case instruction of
                AssignRegister target source ->
                    Result.tuple2 (doesRegisterExist target controller) (doesRegisterExist source controller)
                        |> Result.ignore

                AssignLabel target _ ->
                    doesRegisterExist target controller

                AssignOperation target (Operation _ arguments) ->
                    Result.tuple2
                        (doesRegisterExist target controller)
                        (arguments
                            |> List.map checkArg
                            |> Result.sequence
                            |> Result.ignore
                        )
                        |> Result.ignore

                AssignConstant target _ ->
                    doesRegisterExist target controller

                JumpToLabel label ->
                    Ok ()

                JumpToLabelAtRegister target ->
                    doesRegisterExist target controller

                JumpToLabelIf testRegister _ ->
                    doesRegisterExist testRegister controller

                JumpToLabelAtRegisterIf testRegister target ->
                    Result.tuple2 (doesRegisterExist testRegister controller) (doesRegisterExist target controller)
                        |> Result.ignore

                Halt ->
                    Ok ()

                PushRegister register ->
                    doesRegisterExist register controller

                PushConstant _ ->
                    Ok ()

                PushLabel label ->
                    Ok ()

                Pop target ->
                    doesRegisterExist target controller

                AssignCallAtLabel target _ ->
                    doesRegisterExist target controller

                AssignCallAtRegister target labelRegister ->
                    Result.tuple2 (doesRegisterExist target controller) (doesRegisterExist labelRegister controller)
                        |> Result.ignore

                ConstructPair target arg0 arg1 ->
                    [ doesRegisterExist target controller, checkArg arg0, checkArg arg1 ]
                        |> Result.sequence
                        |> Result.ignore

                First target source ->
                    Result.tuple2 (doesRegisterExist target controller) (doesRegisterExist source controller)
                        |> Result.ignore

                Second target source ->
                    Result.tuple2 (doesRegisterExist target controller) (doesRegisterExist source controller)
                        |> Result.ignore

                SetFirst target arg ->
                    Result.tuple2 (doesRegisterExist target controller) (checkArg arg)
                        |> Result.ignore

                SetSecond target arg ->
                    Result.tuple2 (doesRegisterExist target controller) (checkArg arg)
                        |> Result.ignore

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


setMemoryStateOfMachine : MemoryState -> Machine -> Machine
setMemoryStateOfMachine memoryState ({ memory } as machine) =
    case memory.memoryInUse of
        Zero ->
            { machine | memory = { memory | memoryState0 = memoryState } }

        One ->
            { machine | memory = { memory | memoryState1 = memoryState } }


currentMemoryState : Machine -> MemoryState
currentMemoryState machine =
    case machine.memory.memoryInUse of
        Zero ->
            machine.memory.memoryState0

        One ->
            machine.memory.memoryState1


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
                                (machine |> currentMemoryState |> MemoryState.new ( value0, value1 ))
                                    |> Result.mapError MemoryError
                                    |> Result.map
                                        (\( newPairAddress, newMemoryState ) ->
                                            { isFinished = False
                                            , machine =
                                                machine
                                                    |> setMemoryStateOfMachine newMemoryState
                                                    |> updateRegister target (Pair newPairAddress)
                                                    |> advanceInstructionPointer
                                            }
                                        )
                            )

                First target source ->
                    machine
                        |> getMemoryAddressAtRegister source
                        |> Result.andThen
                            (\pointer ->
                                machine
                                    |> currentMemoryState
                                    |> MemoryState.get pointer
                                    |> Result.mapError MemoryError
                                    |> Result.map
                                        (\( a, _ ) ->
                                            { isFinished = False
                                            , machine =
                                                machine
                                                    |> updateRegister target a
                                                    |> advanceInstructionPointer
                                            }
                                        )
                            )

                Second target source ->
                    machine
                        |> getMemoryAddressAtRegister source
                        |> Result.andThen
                            (\pointer ->
                                machine
                                    |> currentMemoryState
                                    |> MemoryState.get pointer
                                    |> Result.mapError MemoryError
                                    |> Result.map
                                        (\( _, b ) ->
                                            { isFinished = False
                                            , machine =
                                                machine
                                                    |> updateRegister target b
                                                    |> advanceInstructionPointer
                                            }
                                        )
                            )

                SetFirst register arg ->
                    Result.tuple2 (machine |> getValueFromArgument arg) (machine |> getMemoryAddressAtRegister register)
                        |> Result.map
                            (\( val, pointer ) ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> setMemoryStateOfMachine
                                            (MemoryState.update pointer (\( _, b ) -> ( val, b )) (machine |> currentMemoryState))
                                        |> advanceInstructionPointer
                                }
                            )

                SetSecond register arg ->
                    Result.tuple2 (machine |> getValueFromArgument arg) (machine |> getMemoryAddressAtRegister register)
                        |> Result.map
                            (\( val, pointer ) ->
                                { isFinished = False
                                , machine =
                                    machine
                                        |> setMemoryStateOfMachine
                                            (MemoryState.update pointer (\( a, _ ) -> ( a, val )) (machine |> currentMemoryState))
                                        |> advanceInstructionPointer
                                }
                            )

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
