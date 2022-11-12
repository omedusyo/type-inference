module RegisterMachine.Machine exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lib.Result as Result
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), Instruction(..), InstructionPointer, Label, MemoryPointer, OperationApplication, OperationArgument(..), OperationName, Register, Value(..))
import RegisterMachine.MemoryState as MemoryState exposing (MemoryError, MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Set exposing (Set)


type alias ControllerExample =
    { name : String
    , controller : Controller
    , initialRegisterEnvironment : RegisterEnvironment
    }


type alias MachineWithInstructions =
    { machineState : MachineState, instructionsState : InstructionsState }


type alias MachineState =
    { env : RegisterEnvironment
    , stack : Stack
    , memory : MachineMemory
    , operationEnv : OperationEnvironment
    }


type alias InstructionsState =
    { instructionPointer : InstructionPointer
    , instructions : MachineInstructions
    }


type alias MachineInstructions =
    { labels : Set Label
    , labelToInstructionPointer : Dict Label InstructionPointer
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


twoFlip : Two -> Two
twoFlip twoVal =
    case twoVal of
        Zero ->
            One

        One ->
            Zero


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
                AssignRegister { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                AssignLabel { targetRegister } ->
                    controller |> checkRegisters [ targetRegister ] []

                AssignOperation { targetRegister, operationApplication } ->
                    controller |> checkRegisters [] operationApplication.arguments

                AssignConstant { targetRegister } ->
                    controller |> checkRegisters [ targetRegister ] []

                JumpToLabel { label } ->
                    Ok ()

                JumpToLabelAtRegister { labelRegister } ->
                    controller |> checkRegisters [ labelRegister ] []

                JumpToLabelIf { testRegister } ->
                    controller |> checkRegisters [ testRegister ] []

                JumpToLabelAtRegisterIf { testRegister, labelRegister } ->
                    controller |> checkRegisters [ testRegister, labelRegister ] []

                Halt _ ->
                    Ok ()

                PushRegister { sourceRegister } ->
                    controller |> checkRegisters [ sourceRegister ] []

                PushConstant _ ->
                    Ok ()

                PushLabel { label } ->
                    Ok ()

                Pop { targetRegister } ->
                    controller |> checkRegisters [ targetRegister ] []

                AssignCallAtLabel { targetRegister } ->
                    controller |> checkRegisters [ targetRegister ] []

                AssignCallAtRegister { targetRegister, labelRegister } ->
                    controller |> checkRegisters [ targetRegister, labelRegister ] []

                ConstructPair { targetRegister, operationArgument0, operationArgument1 } ->
                    controller |> checkRegisters [ targetRegister ] [ operationArgument0, operationArgument1 ]

                First { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                Second { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                SetFirst { targetRegister, operationArgument } ->
                    controller |> checkRegisters [ targetRegister ] [ operationArgument ]

                SetSecond { targetRegister, operationArgument } ->
                    controller |> checkRegisters [ targetRegister ] [ operationArgument ]

                DualFirst { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                DualSecond { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                DualSetFirst { targetRegister, operationArgument } ->
                    controller |> checkRegisters [ targetRegister ] [ operationArgument ]

                DualSetSecond { targetRegister, operationArgument } ->
                    controller |> checkRegisters [ targetRegister ] [ operationArgument ]

                MoveToDual { targetRegister, sourceRegister } ->
                    controller |> checkRegisters [ targetRegister, sourceRegister ] []

                MarkAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } ->
                    controller |> checkRegisters [ toBeCollectedFromRegister, referenceToDualMemoryRegister ] []

                SwapMemory _ ->
                    Ok ()

        initMachineInstructions : ( InstructionPointer, MachineInstructions )
        initMachineInstructions =
            ( 0
            , { labels = Set.empty
              , labelToInstructionPointer = Dict.empty
              , instructions = Array.empty
              }
            )

        update : LabelOrInstruction -> ( InstructionPointer, MachineInstructions ) -> Result TranslationError ( InstructionPointer, MachineInstructions )
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
                                , labelToInstructionPointer =
                                    Dict.insert label pointer machineInstructions.labelToInstructionPointer
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
-- =Operations=


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



-- =Instructions=


getInstruction : InstructionsState -> Maybe Instruction
getInstruction instructionsState =
    Array.get instructionsState.instructionPointer instructionsState.instructions.instructions


advanceInstructionPointer : InstructionsState -> InstructionsState
advanceInstructionPointer instructionsState =
    { instructionsState
        | instructionPointer = instructionsState.instructionPointer + 1
    }


getInstructionPointerFromLabelOld : Label -> InstructionsState -> Maybe InstructionPointer
getInstructionPointerFromLabelOld label instructionsState =
    Dict.get label instructionsState.instructions.labelToInstructionPointer



-- TODO: This one is new. Remove the one above


getInstructionPointerFromLabel : Label -> Dict Label InstructionPointer -> Result RuntimeError InstructionPointer
getInstructionPointerFromLabel label labelToInstructionPointer =
    Dict.get label labelToInstructionPointer
        |> Result.fromMaybe (LabelPointsToNothing label)


jump : Label -> InstructionsState -> InstructionsState
jump label instructionsState =
    case getInstructionPointerFromLabelOld label instructionsState of
        Just pointer ->
            pointerJump pointer instructionsState

        Nothing ->
            instructionsState


pointerJump : InstructionPointer -> InstructionsState -> InstructionsState
pointerJump pointer instructionsState =
    { instructionsState
        | instructionPointer = pointer
    }



-- =MachineState=


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


getInstructionPointerAtRegister : Register -> MachineState -> Result RuntimeError InstructionPointer
getInstructionPointerAtRegister register machine =
    machine
        |> getRegister register
        |> Result.andThen
            (\value ->
                case value of
                    InstructionPointer pointer ->
                        Ok pointer

                    _ ->
                        Err ExpectedInstructionPointerInRegister
            )


getMemoryPointerAtRegister : Register -> MachineState -> Result RuntimeError MemoryPointer
getMemoryPointerAtRegister register machine =
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


getOperation : OperationName -> MachineState -> Result RuntimeError (List Value -> Result RuntimeError Value)
getOperation operationName machine =
    case Dict.get operationName machine.operationEnv of
        Just op ->
            Ok op

        Nothing ->
            Err (UndefinedOperation operationName)


haltComplex : MachineWithInstructions -> { isFinished : Bool, machine : MachineWithInstructions }
haltComplex machine =
    { isFinished = True, machine = machine }



-- =Stack=


push : Value -> MachineState -> MachineState
push val machine =
    { machine
        | stack = Stack.push val machine.stack
    }


popComplex : MachineState -> Result RuntimeError ( Value, MachineState )
popComplex machine =
    case Stack.pop machine.stack of
        Just ( val, stack ) ->
            Ok ( val, { machine | stack = stack } )

        Nothing ->
            Err PoppingEmptyStack



-- =Memory=


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


accessPairComplex : MemoryCellComponent -> MemoryType -> Register -> Register -> MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
accessPairComplex memoryCellComponent memoryType target source { machineState, instructionsState } =
    machineState
        |> getMemoryPointerAtRegister source
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


setPairComplex : MemoryCellComponent -> MemoryType -> Register -> OperationArgument -> MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
setPairComplex memoryCellComponent memoryType register arg { machineState, instructionsState } =
    Result.tuple2 (machineState |> getValueFromArgument arg) (machineState |> getMemoryPointerAtRegister register)
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


swapMemoryComplex : MachineState -> MachineState
swapMemoryComplex ({ memory } as machine) =
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



-- ===Running the Machine===


type RuntimeError
    = UndefinedRegister Register
    | UndefinedOperation OperationName
    | WrongNumberOfArgumentsGivenToOperationExpected Int
    | TheOperationExpectsIntegerArguments
    | LabelPointsToNothing Label
    | PoppingEmptyStack
    | ExpectedInstructionPointerInRegister
    | ExpectedPairInRegister
    | MemoryError MemoryError


type OneComputationStepState
    = RuntimeError RuntimeError
    | Halted
    | Continue MachineState ControllerChange


type ControllerChange
    = AdvanceInstructionPointer
    | JumpTo InstructionPointer


advance : MachineState -> OneComputationStepState
advance machineState =
    Continue machineState AdvanceInstructionPointer


jumpTo : InstructionPointer -> MachineState -> OneComputationStepState
jumpTo instructionPointer machineState =
    Continue machineState (JumpTo instructionPointer)


sequenceResult : (a -> OneComputationStepState) -> Result RuntimeError a -> OneComputationStepState
sequenceResult f result =
    case result of
        Ok a ->
            f a

        Err e ->
            RuntimeError e



-- ===START individual actions===
-- assignment


assignRegister : RegisterMachine.AssignRegisterInput -> MachineState -> OneComputationStepState
assignRegister { targetRegister, sourceRegister } machineState =
    getRegister sourceRegister machineState
        |> sequenceResult (\val -> advance (machineState |> updateRegister targetRegister val))


assignLabel : RegisterMachine.AssignLabelInput -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
assignLabel { targetRegister, label } labelToInstructionPointer machineState =
    -- TODO: Would it be reasonable to have this labelToPosition dictionary in the MachineState?
    getInstructionPointerFromLabel label labelToInstructionPointer
        |> sequenceResult (\pointer -> advance (machineState |> updateRegister targetRegister (InstructionPointer pointer)))


assignOperation : RegisterMachine.AssignOperationInput -> MachineState -> OneComputationStepState
assignOperation { targetRegister, operationApplication } machineState =
    getOperation operationApplication.name machineState
        |> sequenceResult
            (\op ->
                operationApplication.arguments
                    |> List.map (\argument -> machineState |> getValueFromArgument argument)
                    |> Result.sequence
                    |> Result.andThen op
                    |> sequenceResult (\output -> advance (machineState |> updateRegister targetRegister output))
            )


assignConstant : RegisterMachine.AssignConstantInput -> MachineState -> OneComputationStepState
assignConstant { targetRegister, constant } machineState =
    advance (machineState |> updateRegister targetRegister (ConstantValue constant))



-- jumping


jumpToLabel : RegisterMachine.JumpToLabelInput -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
jumpToLabel { label } labelToInstructionPointer machineState =
    getInstructionPointerFromLabel label labelToInstructionPointer
        |> sequenceResult (\instructionPointer -> jumpTo instructionPointer machineState)


jumpToLabelAtRegister : RegisterMachine.JumpToLabelAtRegisterInput -> MachineState -> OneComputationStepState
jumpToLabelAtRegister { labelRegister } machineState =
    getInstructionPointerAtRegister labelRegister machineState
        |> sequenceResult (\pointer -> jumpTo pointer machineState)


jumpToLabelIf : RegisterMachine.JumpToLabelIfInput -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
jumpToLabelIf { testRegister, label } labelToInstructionPointer machineState =
    getRegister testRegister machineState
        |> sequenceResult
            (\val ->
                if val == ConstantValue (Num 1) then
                    getInstructionPointerFromLabel label labelToInstructionPointer
                        |> sequenceResult (\instructionPointer -> jumpTo instructionPointer machineState)

                else
                    advance machineState
            )


jumpToLabelAtRegisterIf : RegisterMachine.JumpToLabelAtRegisterIfInput -> MachineState -> OneComputationStepState
jumpToLabelAtRegisterIf { testRegister, labelRegister } machineState =
    getRegister testRegister machineState
        |> sequenceResult
            (\val ->
                if val == ConstantValue (Num 1) then
                    getInstructionPointerAtRegister labelRegister machineState
                        |> sequenceResult (\instructionPointer -> jumpTo instructionPointer machineState)

                else
                    advance machineState
            )


halt : RegisterMachine.HaltInput -> MachineState -> OneComputationStepState
halt _ _ =
    Halted



-- stack


pushRegister : RegisterMachine.PushRegisterInput -> MachineState -> OneComputationStepState
pushRegister { sourceRegister } machineState =
    getRegister sourceRegister machineState
        |> sequenceResult (\val -> advance (machineState |> push val))


pushConstant : RegisterMachine.PushConstantInput -> MachineState -> OneComputationStepState
pushConstant { constant } machineState =
    advance (machineState |> push (ConstantValue constant))


pushLabel : RegisterMachine.PushLabelInput -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
pushLabel { label } labelToInstructionPointer machineState =
    getInstructionPointerFromLabel label labelToInstructionPointer
        |> sequenceResult (\instructionPointer -> advance (machineState |> push (InstructionPointer instructionPointer)))


pop : RegisterMachine.PopInput -> MachineState -> OneComputationStepState
pop { targetRegister } machineState =
    Stack.pop machineState.stack
        |> Result.fromMaybe PoppingEmptyStack
        |> sequenceResult (\( val, stack ) -> advance ({ machineState | stack = stack } |> updateRegister targetRegister val))



-- calling procedure
-- TODO: New phenomenon... I need to know current instruction position, so I can compute the next one.
--       I would prefer to not have this instruction.


assignCallAtLabel : RegisterMachine.AssignCallAtLabelInput -> InstructionPointer -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
assignCallAtLabel { targetRegister, label } currentInstructionPointer labelToInstructionPointer machineState =
    -- target <- $ip + 1
    -- ip <- :label
    getInstructionPointerFromLabel label labelToInstructionPointer
        |> sequenceResult
            (\instructionPosition ->
                jumpTo
                    instructionPosition
                    (machineState |> updateRegister targetRegister (InstructionPointer (currentInstructionPointer + 1)))
            )


assignCallAtRegister : RegisterMachine.AssignCallAtRegisterInput -> InstructionPointer -> MachineState -> OneComputationStepState
assignCallAtRegister { targetRegister, labelRegister } currentInstructionPointer machineState =
    -- target <- $ip + 1
    -- ip <- $labelRegister
    getInstructionPointerAtRegister labelRegister machineState
        |> sequenceResult
            (\instructionPointer ->
                jumpTo instructionPointer
                    (machineState |> updateRegister targetRegister (InstructionPointer (currentInstructionPointer + 1)))
            )



-- memory


constructPair : RegisterMachine.ConstructPairInput -> MemoryType -> MachineState -> OneComputationStepState
constructPair { targetRegister, operationArgument0, operationArgument1 } memoryType machineState =
    Result.tuple2 (machineState |> getValueFromArgument operationArgument0) (machineState |> getValueFromArgument operationArgument1)
        |> sequenceResult
            (\( value0, value1 ) ->
                (currentMemoryState Main machineState |> MemoryState.new ( value0, value1 ))
                    |> Result.mapError MemoryError
                    |> sequenceResult
                        (\( newPairPointer, newMemoryState ) ->
                            advance
                                (machineState
                                    |> setMemoryStateOfMachine memoryType newMemoryState
                                    |> updateRegister targetRegister (Pair newPairPointer)
                                )
                        )
            )


accessPair : MemoryCellComponent -> MemoryType -> Register -> Register -> MachineState -> OneComputationStepState
accessPair memoryCellComponent memoryType target source machineState =
    machineState
        |> getMemoryPointerAtRegister source
        |> sequenceResult
            (\pointer ->
                machineState
                    |> currentMemoryState memoryType
                    |> MemoryState.get pointer
                    |> Result.mapError MemoryError
                    |> sequenceResult
                        (\( a, b ) ->
                            advance
                                (machineState
                                    |> (case memoryCellComponent of
                                            FirstComponent ->
                                                updateRegister target a

                                            SecondComponent ->
                                                updateRegister target b
                                       )
                                )
                        )
            )


setPair : MemoryCellComponent -> MemoryType -> Register -> OperationArgument -> MachineState -> OneComputationStepState
setPair memoryCellComponent memoryType register arg machineState =
    Result.tuple2 (machineState |> getValueFromArgument arg) (machineState |> getMemoryPointerAtRegister register)
        |> sequenceResult
            (\( val, pointer ) ->
                advance
                    (machineState
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
                    )
            )



-- memory


first : RegisterMachine.FirstInput -> MemoryType -> MachineState -> OneComputationStepState
first { targetRegister, sourceRegister } memoryType machineState =
    machineState |> accessPair FirstComponent memoryType targetRegister sourceRegister


second : RegisterMachine.SecondInput -> MemoryType -> MachineState -> OneComputationStepState
second { targetRegister, sourceRegister } memoryType machineState =
    machineState |> accessPair SecondComponent memoryType targetRegister sourceRegister


setFirst : RegisterMachine.SetFirstInput -> MemoryType -> MachineState -> OneComputationStepState
setFirst { targetRegister, operationArgument } memoryType machineState =
    machineState |> setPair FirstComponent memoryType targetRegister operationArgument


setSecond : RegisterMachine.SetSecondInput -> MemoryType -> MachineState -> OneComputationStepState
setSecond { targetRegister, operationArgument } memoryType machineState =
    machineState |> setPair SecondComponent memoryType targetRegister operationArgument



-- garbage collection


moveToDual : RegisterMachine.MoveToDualInput -> MachineState -> OneComputationStepState
moveToDual { targetRegister, sourceRegister } machineState =
    machineState
        |> getMemoryPointerAtRegister sourceRegister
        |> sequenceResult
            (\sourcePointer ->
                machineState
                    |> currentMemoryState Main
                    |> MemoryState.get sourcePointer
                    |> Result.mapError MemoryError
                    |> Result.andThen
                        (\memoryCell ->
                            machineState
                                |> currentMemoryState Dual
                                |> MemoryState.new memoryCell
                                |> Result.mapError MemoryError
                        )
                    |> sequenceResult
                        (\( newPairPointer, newDualMemoryState ) ->
                            advance
                                (machineState
                                    |> setDualMemoryStateOfMachine newDualMemoryState
                                    |> updateRegister targetRegister (Pair newPairPointer)
                                )
                        )
            )


markAsMoved : RegisterMachine.MarkAsMovedInput -> MachineState -> OneComputationStepState
markAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } machineState =
    Result.tuple2 (machineState |> getMemoryPointerAtRegister toBeCollectedFromRegister) (machineState |> getMemoryPointerAtRegister referenceToDualMemoryRegister)
        |> sequenceResult
            (\( pointerToBeCollected, pointerToDualMemory ) ->
                advance
                    (machineState
                        |> setMemoryStateOfMachine Main
                            (machineState
                                |> currentMemoryState Main
                                |> MemoryState.set pointerToBeCollected ( Moved, Pair pointerToDualMemory )
                            )
                    )
            )


swapMemory : RegisterMachine.SwapMemoryInput -> MachineState -> OneComputationStepState
swapMemory _ ({ memory } as machineState) =
    advance { machineState | memory = { memory | memoryInUse = twoFlip memory.memoryInUse } }



-- ===END individual actions===


act : Instruction -> InstructionPointer -> Dict Label InstructionPointer -> MachineState -> OneComputationStepState
act instruction currentInstructionPointer labelToPosition machineState =
    case instruction of
        AssignRegister input ->
            assignRegister input machineState

        AssignLabel input ->
            assignLabel input labelToPosition machineState

        AssignOperation input ->
            assignOperation input machineState

        AssignConstant input ->
            assignConstant input machineState

        JumpToLabel input ->
            jumpToLabel input labelToPosition machineState

        JumpToLabelAtRegister input ->
            jumpToLabelAtRegister input machineState

        JumpToLabelIf input ->
            jumpToLabelIf input labelToPosition machineState

        JumpToLabelAtRegisterIf input ->
            jumpToLabelAtRegisterIf input machineState

        Halt input ->
            halt input machineState

        PushRegister input ->
            pushRegister input machineState

        PushConstant input ->
            pushConstant input machineState

        PushLabel input ->
            pushLabel input labelToPosition machineState

        Pop input ->
            pop input machineState

        AssignCallAtLabel input ->
            assignCallAtLabel input currentInstructionPointer labelToPosition machineState

        AssignCallAtRegister input ->
            assignCallAtRegister input currentInstructionPointer machineState

        ConstructPair input ->
            constructPair input Main machineState

        First input ->
            first input Main machineState

        Second input ->
            second input Main machineState

        SetFirst input ->
            setFirst input Main machineState

        SetSecond input ->
            setSecond input Main machineState

        DualFirst input ->
            first input Dual machineState

        DualSecond input ->
            second input Dual machineState

        DualSetFirst input ->
            setFirst input Dual machineState

        DualSetSecond input ->
            setSecond input Dual machineState

        MoveToDual input ->
            moveToDual input machineState

        MarkAsMoved input ->
            markAsMoved input machineState

        SwapMemory input ->
            swapMemory input machineState


runOneStep : MachineWithInstructions -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
runOneStep ({ machineState, instructionsState } as machine) =
    case getInstruction instructionsState of
        Just instruction ->
            case instruction of
                AssignRegister { targetRegister, sourceRegister } ->
                    getRegister sourceRegister machineState
                        |> Result.map
                            (\val ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister targetRegister val
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                AssignLabel { targetRegister, label } ->
                    -- TODO: There could be an off-by-one error, where the label points to Nothing, but actually it is the halting label?
                    case getInstructionPointerFromLabelOld label instructionsState of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister targetRegister (InstructionPointer pointer)
                                    , instructionsState = instructionsState |> advanceInstructionPointer
                                    }
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                AssignOperation { targetRegister, operationApplication } ->
                    let
                        applyOp : Operation -> Result RuntimeError { isFinished : Bool, machine : MachineWithInstructions }
                        applyOp op =
                            operationApplication.arguments
                                |> List.map (\argument -> machineState |> getValueFromArgument argument)
                                |> Result.sequence
                                |> Result.andThen op
                                |> Result.map
                                    (\output ->
                                        { isFinished = False
                                        , machine =
                                            { machineState =
                                                machineState
                                                    |> updateRegister targetRegister output
                                            , instructionsState = instructionsState |> advanceInstructionPointer
                                            }
                                        }
                                    )
                    in
                    getOperation operationApplication.name machineState
                        |> Result.andThen applyOp

                AssignConstant { targetRegister, constant } ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> updateRegister targetRegister (ConstantValue constant)
                            , instructionsState = instructionsState |> advanceInstructionPointer
                            }
                        }

                JumpToLabel { label } ->
                    Ok { isFinished = False, machine = { machineState = machineState, instructionsState = jump label instructionsState } }

                JumpToLabelAtRegister { labelRegister } ->
                    getInstructionPointerAtRegister labelRegister machineState
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine = { machineState = machineState, instructionsState = pointerJump pointer instructionsState }
                                }
                            )

                JumpToLabelIf { testRegister, label } ->
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

                JumpToLabelAtRegisterIf { testRegister, labelRegister } ->
                    getRegister testRegister machineState
                        |> Result.andThen
                            (\val ->
                                if val == ConstantValue (Num 1) then
                                    getInstructionPointerAtRegister labelRegister machineState
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

                Halt _ ->
                    Ok (haltComplex machine)

                PushRegister { sourceRegister } ->
                    getRegister sourceRegister machineState
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

                PushConstant { constant } ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> push (ConstantValue constant)
                            , instructionsState =
                                instructionsState |> advanceInstructionPointer
                            }
                        }

                PushLabel { label } ->
                    case getInstructionPointerFromLabelOld label instructionsState of
                        Just pointer ->
                            Ok
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> push (InstructionPointer pointer)
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                Pop { targetRegister } ->
                    popComplex machineState
                        |> Result.map
                            (\( val, newMachineState ) ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        newMachineState
                                            |> updateRegister targetRegister val
                                    , instructionsState =
                                        instructionsState |> advanceInstructionPointer
                                    }
                                }
                            )

                AssignCallAtLabel { targetRegister, label } ->
                    -- target <- $ip + 1
                    -- ip <- :label
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> updateRegister targetRegister (InstructionPointer (instructionsState.instructionPointer + 1))
                            , instructionsState =
                                instructionsState |> jump label
                            }
                        }

                AssignCallAtRegister { targetRegister, labelRegister } ->
                    -- target <- $ip + 1
                    -- ip <- $labelRegister
                    getInstructionPointerAtRegister labelRegister machineState
                        |> Result.map
                            (\pointer ->
                                { isFinished = False
                                , machine =
                                    { machineState =
                                        machineState
                                            |> updateRegister targetRegister (InstructionPointer (instructionsState.instructionPointer + 1))
                                    , instructionsState =
                                        instructionsState |> pointerJump pointer
                                    }
                                }
                            )

                ConstructPair { targetRegister, operationArgument0, operationArgument1 } ->
                    Result.tuple2 (machineState |> getValueFromArgument operationArgument0) (machineState |> getValueFromArgument operationArgument1)
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
                                                        |> updateRegister targetRegister (Pair newPairAddress)
                                                , instructionsState =
                                                    instructionsState |> advanceInstructionPointer
                                                }
                                            }
                                        )
                            )

                First { targetRegister, sourceRegister } ->
                    machine |> accessPairComplex FirstComponent Main targetRegister sourceRegister

                Second { targetRegister, sourceRegister } ->
                    machine |> accessPairComplex SecondComponent Main targetRegister sourceRegister

                SetFirst { targetRegister, operationArgument } ->
                    machine |> setPairComplex FirstComponent Main targetRegister operationArgument

                SetSecond { targetRegister, operationArgument } ->
                    machine |> setPairComplex SecondComponent Main targetRegister operationArgument

                DualFirst { targetRegister, sourceRegister } ->
                    machine |> accessPairComplex FirstComponent Dual targetRegister sourceRegister

                DualSecond { targetRegister, sourceRegister } ->
                    machine |> accessPairComplex SecondComponent Dual targetRegister sourceRegister

                DualSetFirst { targetRegister, operationArgument } ->
                    machine |> setPairComplex FirstComponent Dual targetRegister operationArgument

                DualSetSecond { targetRegister, operationArgument } ->
                    machine |> setPairComplex SecondComponent Dual targetRegister operationArgument

                MoveToDual { targetRegister, sourceRegister } ->
                    machineState
                        |> getMemoryPointerAtRegister sourceRegister
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
                                                        |> updateRegister targetRegister (Pair addressOfNewPair)
                                                , instructionsState =
                                                    instructionsState |> advanceInstructionPointer
                                                }
                                            }
                                        )
                            )

                MarkAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } ->
                    Result.tuple2 (machineState |> getMemoryPointerAtRegister toBeCollectedFromRegister) (machineState |> getMemoryPointerAtRegister referenceToDualMemoryRegister)
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

                SwapMemory _ ->
                    Ok
                        { isFinished = False
                        , machine =
                            { machineState =
                                machineState
                                    |> swapMemoryComplex
                            , instructionsState =
                                instructionsState |> advanceInstructionPointer
                            }
                        }

        Nothing ->
            Ok (haltComplex machine)


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
