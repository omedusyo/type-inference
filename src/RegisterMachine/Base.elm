module RegisterMachine.Base exposing (..)

-- sequence of instructions should start with a label

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Lib.Result as Result
import Set exposing (Set)


type alias Label =
    String


type alias Register =
    String


type Value
    = ConstantValue Constant
    | Pair MemoryAddress
    | InstructionAddress InstructionAddress


type Constant
    = Num Int
    | Nil


type alias InstructionAddress =
    Int


type alias OperationName =
    String



-- TODO: Is this going to be used somewhere? Atleast for basic type checking?


type alias OperationArity =
    Int



-- TODO: Change the third argument to `Operation` to from `Register` to `OperationArgument`
-- TODO: What about labels as arguments to operations?


type OperationArgument
    = Register Register
    | Constant Constant


type OperationApplication
    = Operation OperationName (List OperationArgument)


type
    Instruction
    -- b <- $a
    -- b <- :foo
    -- a <- op($x, $y)
    -- a <- constant
    -- jump :foo
    -- jump $a
    -- if $a jump :foo
    -- if $a jump $b // is a register that contains a labelb
    -- halt
    -- push $a
    -- a <- stack
    --
    -- TODO: Define the memory instructions
    -- a <- new 123
    -- b <- new nil
    -- p <- new pair($a, $b)
    --
    -- update(p, 123)
    -- update(p, nil)
    -- update(p, pair($a, $b)
    --
    -- a <- get(p) ???
    --
    -- nil?
    -- num?
    -- pair?
    = -- assignment
      AssignRegister Register Register
    | AssignLabel Register Label
    | AssignOperation Register OperationApplication
    | AssignConstant Register Constant
      -- jumping
    | JumpToLabel Label
    | JumpToLabelAtRegister Register
    | JumpToLabelIf Register Label
    | JumpToLabelAtRegisterIf Register Register -- first register is the test register, second register is the register containing the label
    | Halt
      -- stack
    | PushRegister Register
    | PushConstant Constant
    | PushLabel Label
    | Pop Register
      -- calling procedure
    | AssignCallAtLabel Register Label
    | AssignCallAtRegister Register Register


type LabelOrInstruction
    = Perform Instruction
    | Label Label


type alias InstructionBlock =
    List LabelOrInstruction



-- A controller is a description of a register machine.
-- Given a controller, we should be able to construct a register machine that is governed by said controller.


type alias Controller =
    -- TODO: Should we have typed registers? Int/Bool? This will complicate the operations though...
    { registers : Set Register
    , instructions : InstructionBlock
    }


type alias MachineInstructions =
    { labels : Set Label
    , labelToPosition : Dict Label InstructionAddress
    , instructions : Array Instruction
    }


type TranslationError
    = LabelUsedMoreThanOnce Label
    | UnknownRegister Register


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
                            |> List.map
                                (\argument ->
                                    case argument of
                                        Register register ->
                                            doesRegisterExist register controller

                                        _ ->
                                            Ok ()
                                )
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



-- === Stack ===


type alias Stack =
    List Value


emptyStack : Stack
emptyStack =
    []


stackToList : Stack -> List Value
stackToList stack =
    stack


pushStack : Value -> Stack -> Stack
pushStack val stack =
    val :: stack


popStack : Stack -> Maybe ( Value, Stack )
popStack stack0 =
    case stack0 of
        [] ->
            Nothing

        val :: stack1 ->
            Just ( val, stack1 )



-- === Memory ===


type alias MemoryAddress =
    Int


type alias MemoryCell =
    ( Value, Value )


type alias MemoryState =
    { memory : Array MemoryCell
    , maxSize : Int
    , nextFreePointer : MemoryAddress
    }


type MemoryError
    = MemoryExceeded
    | InvalidMemoryAccessAt MemoryAddress
    | ExpectedNumAt MemoryAddress
    | ExpectedPairAt MemoryAddress
    | ExpectedNilAt MemoryAddress


emptyMemoryState : Int -> MemoryState
emptyMemoryState maxSize =
    { memory = Array.empty
    , maxSize = maxSize
    , nextFreePointer = 0
    }


memory_example0 : MemoryState
memory_example0 =
    -- ((1, (2, nil)), (5, (6, nil)))
    -- [[1 2] 5 6]
    { memory =
        Array.fromList
            [ ( ConstantValue (Num 32), ConstantValue Nil )
            , ( ConstantValue (Num 16), Pair 0 )
            , ( ConstantValue (Num 128), ConstantValue Nil )
            , ( ConstantValue (Num 64), Pair 2 )
            , ( Pair 1, Pair 3 )
            ]
    , maxSize = 4096
    , nextFreePointer = 5
    }


get : MemoryAddress -> MemoryState -> Result MemoryError MemoryCell
get pointer ({ memory } as memoryState) =
    case memory |> Array.get pointer of
        Just memoryCell ->
            Ok memoryCell

        Nothing ->
            Err (InvalidMemoryAccessAt pointer)


set : MemoryAddress -> MemoryCell -> MemoryState -> MemoryState
set pointer cell ({ memory } as memoryState) =
    { memoryState
        | memory = memory |> Array.set pointer cell
    }


new : MemoryCell -> MemoryState -> Result MemoryError ( MemoryAddress, MemoryState )
new memoryCell ({ memory, maxSize, nextFreePointer } as memoryState) =
    if nextFreePointer + 1 < maxSize then
        Ok
            ( nextFreePointer
            , { memoryState
                | memory = memory |> Array.set nextFreePointer memoryCell
                , nextFreePointer = nextFreePointer + 1
              }
            )

    else
        Err MemoryExceeded



-- === Machine ===


type alias RegisterEnvironment =
    Dict Register Value


type alias Operation =
    List Value -> Result RuntimeError Value


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


type alias OperationEnvironment =
    Dict OperationName Operation


type alias Machine =
    { env : RegisterEnvironment
    , stack : Stack
    , memoryState : MemoryState
    , operationEnv : OperationEnvironment
    , instructionPointer : InstructionAddress
    , instructions : MachineInstructions
    }


makeMachine : Controller -> RegisterEnvironment -> OperationEnvironment -> Result TranslationError Machine
makeMachine controller env operationsEnv =
    parse controller
        |> Result.map
            (\instructions ->
                { env = env
                , operationEnv = operationsEnv
                , memoryState =
                    -- TODO
                    -- emptyMemoryState 4096
                    memory_example0
                , stack = emptyStack
                , instructionPointer = 0
                , instructions = instructions
                }
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


updateRegister : Register -> Value -> Machine -> Machine
updateRegister register val machine =
    { machine
        | env = Dict.insert register val machine.env
    }


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
        | stack = pushStack val machine.stack
    }


pop : Machine -> Result RuntimeError ( Value, Machine )
pop machine =
    case popStack machine.stack of
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
                                |> List.map
                                    (\argument ->
                                        case argument of
                                            Register register ->
                                                getRegister register machine

                                            Constant val ->
                                                Ok (ConstantValue val)
                                    )
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


showConstant : Constant -> String
showConstant val =
    case val of
        Num x ->
            String.fromInt x

        Nil ->
            "nil"


showLabel : Label -> String
showLabel label =
    ":" ++ label


showRegisterUse : Register -> String
showRegisterUse register =
    "$" ++ register


showAssignment : String -> String -> String
showAssignment source target =
    source ++ " <- " ++ target


showInstruction : Instruction -> String
showInstruction instruction =
    case instruction of
        AssignRegister target source ->
            showAssignment target (showRegisterUse source)

        AssignLabel target label ->
            showAssignment target (showLabel label)

        AssignOperation target (Operation opName arguments) ->
            showAssignment
                target
                (String.concat
                    [ opName
                    , "("
                    , arguments
                        |> List.map
                            (\argument ->
                                case argument of
                                    Register register ->
                                        showRegisterUse register

                                    Constant val ->
                                        showConstant val
                            )
                        |> String.join ", "
                    , ")"
                    ]
                )

        AssignConstant target val ->
            showAssignment target (showConstant val)

        JumpToLabel label ->
            String.concat [ "jump ", showLabel label ]

        JumpToLabelAtRegister target ->
            String.concat [ "jump ", showRegisterUse target ]

        JumpToLabelIf testRegister label ->
            String.concat [ "if ", showRegisterUse testRegister, " jump ", showLabel label ]

        JumpToLabelAtRegisterIf testRegister target ->
            String.concat [ "if ", showRegisterUse testRegister, " jump ", showRegisterUse target ]

        Halt ->
            "halt"

        PushRegister register ->
            String.concat [ "push ", showRegisterUse register ]

        PushConstant val ->
            String.concat [ "push ", showConstant val ]

        PushLabel label ->
            String.concat [ "push ", showLabel label ]

        Pop target ->
            showAssignment target "pop-stack"

        AssignCallAtLabel target label ->
            showAssignment target (showLabel label)

        AssignCallAtRegister target labelRegister ->
            showAssignment target (showRegisterUse labelRegister)


showInstructions : List LabelOrInstruction -> String
showInstructions instructions =
    instructions
        |> List.map
            (\labelOrInstruction ->
                case labelOrInstruction of
                    Label label ->
                        label ++ ":"

                    Perform instruction ->
                        "  " ++ showInstruction instruction
            )
        |> String.join "\n"
