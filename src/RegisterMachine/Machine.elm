module RegisterMachine.Machine exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lib.Result as Result
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), Instruction(..), InstructionPointer, Label, MachineInstruction(..), MemoryPointer, OperationArgument(..), OperationName, Register, Value(..))
import RegisterMachine.MemoryState as MemoryState exposing (MemoryError, MemoryState)
import RegisterMachine.Stack as Stack exposing (Stack)
import Set exposing (Set)


type alias ControllerExample =
    { name : String
    , initialRegisters : List ( String, Value )
    , instructionBlock : InstructionBlock
    }


type alias ControlledMachineState =
    { machineState : MachineState
    , controllerState : ControllerState
    }


type alias ControllerState =
    { instructions : Array MachineInstruction
    , currentInstructionPointer : InstructionPointer
    }


type ControllerChange
    = AdvanceInstructionPointer
    | JumpTo InstructionPointer


controllerChangeAction : ControllerChange -> ControllerState -> ControllerState
controllerChangeAction change state =
    case change of
        AdvanceInstructionPointer ->
            { state | currentInstructionPointer = state.currentInstructionPointer + 1 }

        JumpTo instructionPointer ->
            { state | currentInstructionPointer = instructionPointer }


fetchInstruction : ControllerState -> ComputationStep r MachineInstruction
fetchInstruction { instructions, currentInstructionPointer } =
    case Array.get currentInstructionPointer instructions of
        Just instruction ->
            Continue instruction

        Nothing ->
            RuntimeError (InstructionPointerOutOfBounds { instructionPointer = currentInstructionPointer, numberOfInstructions = Array.length instructions })


type alias MachineState =
    { registerEnv : RegisterEnvironment
    , stack : Stack
    , memory : MachineMemory
    , operationEnv : OperationEnvironment
    }


type LabelOrInstruction
    = Perform Instruction
    | Label Label


type alias InstructionBlock =
    List LabelOrInstruction


type alias RegisterEnvironment =
    Dict Register Value


type alias LabelEnvironment =
    -- Poor's man invertible dictionary
    ( Dict Label InstructionPointer, Dict InstructionPointer Label )


type alias Operation =
    List Value -> Result RuntimeError Value


type alias OperationEnvironment =
    Dict OperationName Operation


emptyLabelEnv : LabelEnvironment
emptyLabelEnv =
    ( Dict.empty, Dict.empty )


instructionPointerFromLabel : Label -> LabelEnvironment -> Maybe InstructionPointer
instructionPointerFromLabel label ( labelToInstructionPointer, _ ) =
    Dict.get label labelToInstructionPointer


labelFromInstructionPointer : InstructionPointer -> LabelEnvironment -> Maybe Label
labelFromInstructionPointer instructionPointer ( _, instructionPointerToLabel ) =
    Dict.get instructionPointer instructionPointerToLabel


bindLabel : Label -> InstructionPointer -> LabelEnvironment -> LabelEnvironment
bindLabel label instructionPointer ( labelToInstructionPointer, instructionPointerToLabel ) =
    ( Dict.insert label instructionPointer labelToInstructionPointer
    , Dict.insert instructionPointer label instructionPointerToLabel
    )


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


type CompilationError
    = LabelUsedMoreThanOnce Label
    | LabelDoesNotExist Label
    | UnknownRegister Register



-- A controller is a description of a register machine.
-- Given a controller, we should be able to construct a register machine that is governed by said controller.
-- ===Machine Construction===
-- helper


foldlMayFail : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlMayFail update state actions0 =
    case actions0 of
        [] ->
            Ok state

        a :: actions1 ->
            update a state
                |> Result.andThen (\newState -> foldlMayFail update newState actions1)



-- ===Compilation===


translateLabelToInstructionPointer : LabelEnvironment -> Label -> Result CompilationError InstructionPointer
translateLabelToInstructionPointer labelEnv label =
    instructionPointerFromLabel label labelEnv
        |> Result.fromMaybe (LabelDoesNotExist label)


translateInstructionToMachineInstruction : LabelEnvironment -> Instruction -> Result CompilationError MachineInstruction
translateInstructionToMachineInstruction labelEnv instruction =
    case instruction of
        AssignRegister input ->
            Ok (MAssignRegister input)

        AssignLabel { targetRegister, label } ->
            translateLabelToInstructionPointer labelEnv label
                |> Result.map (\instructionPointer -> MAssignInstructionPointer { targetRegister = targetRegister, instructionPointer = instructionPointer })

        AssignOperation input ->
            Ok (MAssignOperation input)

        AssignConstant input ->
            Ok (MAssignConstant input)

        JumpToLabel { label } ->
            translateLabelToInstructionPointer labelEnv label
                |> Result.map (\instructionPointer -> MJumpToInstructionPointer { instructionPointer = instructionPointer })

        JumpToInstructionPointerAtRegister input ->
            Ok (MJumpToInstructionPointerAtRegister input)

        JumpToLabelIf { testRegister, label } ->
            translateLabelToInstructionPointer labelEnv label
                |> Result.map (\instructionPointer -> MJumpToInstructionPointerIf { testRegister = testRegister, instructionPointer = instructionPointer })

        JumpToInstructionPointerAtRegisterIf input ->
            Ok (MJumpToInstructionPointerAtRegisterIf input)

        Halt input ->
            Ok (MHalt input)

        PushRegister input ->
            Ok (MPushRegister input)

        PushConstant input ->
            Ok (MPushConstant input)

        PushLabel { label } ->
            translateLabelToInstructionPointer labelEnv label
                |> Result.map (\instructionPointer -> MPushInstructionPointer { instructionPointer = instructionPointer })

        Pop input ->
            Ok (MPop input)

        ConstructPair input ->
            Ok (MConstructPair input)

        First input ->
            Ok (MFirst input)

        Second input ->
            Ok (MSecond input)

        SetFirst input ->
            Ok (MSetFirst input)

        SetSecond input ->
            Ok (MSetSecond input)

        DualFirst input ->
            Ok (MDualFirst input)

        DualSecond input ->
            Ok (MDualSecond input)

        DualSetFirst input ->
            Ok (MDualSetFirst input)

        DualSetSecond input ->
            Ok (MDualSetSecond input)

        MoveToDual input ->
            Ok (MMoveToDual input)

        MarkAsMoved input ->
            Ok (MMarkAsMoved input)

        SwapMemory input ->
            Ok (MSwapMemory input)


translateInstructionsToMachineInstructions : InstructionBlock -> Result CompilationError ( LabelEnvironment, Array MachineInstruction )
translateInstructionsToMachineInstructions instructionBlock =
    let
        -- Note that the list of instructiosn returned is in a reversed order
        constructLabelEnvironmentAndFilterOutLabels : InstructionPointer -> InstructionBlock -> LabelEnvironment -> List Instruction -> Result CompilationError ( LabelEnvironment, List Instruction )
        constructLabelEnvironmentAndFilterOutLabels nextInstructionPointer instructionBlock0 labelEnv filteredInstructionsReversed =
            case instructionBlock0 of
                [] ->
                    Ok ( labelEnv, filteredInstructionsReversed )

                labelOrInstruction :: instructionBlock1 ->
                    case labelOrInstruction of
                        Perform instruction ->
                            constructLabelEnvironmentAndFilterOutLabels (nextInstructionPointer + 1) instructionBlock1 labelEnv (instruction :: filteredInstructionsReversed)

                        Label label ->
                            constructLabelEnvironmentAndFilterOutLabels nextInstructionPointer instructionBlock1 (labelEnv |> bindLabel label nextInstructionPointer) filteredInstructionsReversed

        -- Note that this assumes the instructions reversed as input
        translate : LabelEnvironment -> List Instruction -> Result CompilationError (List MachineInstruction)
        translate labelEnv instructionsReversed =
            foldlMayFail
                (\instruction machineInstructions ->
                    translateInstructionToMachineInstruction labelEnv instruction
                        |> Result.map (\machineInstruction -> machineInstruction :: machineInstructions)
                )
                []
                instructionsReversed
    in
    constructLabelEnvironmentAndFilterOutLabels 0 instructionBlock emptyLabelEnv []
        |> Result.andThen
            (\( labelEnv, reversedInstructions ) ->
                translate labelEnv reversedInstructions
                    |> Result.map (\machineInstructions -> ( labelEnv, machineInstructions |> Array.fromList ))
            )


compileMachine : InstructionBlock -> List ( Register, Value ) -> OperationEnvironment -> Result CompilationError ( LabelEnvironment, ControlledMachineState )
compileMachine instructionBlock initialRegisters operationEnv =
    translateInstructionsToMachineInstructions instructionBlock
        |> Result.andThen
            (\( labelEnv, machineInstructions ) ->
                -- TODO: Here you should check that the machineInstructions use the registers in the initial register environment
                --       Do that after you replace Register Names with a special RegisterPointers
                Ok
                    ( labelEnv
                    , { machineState =
                            { registerEnv = Dict.fromList initialRegisters
                            , operationEnv = operationEnv
                            , memory =
                                let
                                    memorySize =
                                        4096
                                in
                                { memoryState0 = MemoryState.empty memorySize
                                , memoryState1 = MemoryState.empty memorySize
                                , memoryInUse = Zero
                                }
                            , stack = Stack.empty
                            }
                      , controllerState =
                            { instructions = machineInstructions
                            , currentInstructionPointer = 0
                            }
                      }
                    )
            )


compileMachineFromControllerExample : ControllerExample -> OperationEnvironment -> Result CompilationError ( LabelEnvironment, ControlledMachineState )
compileMachineFromControllerExample { instructionBlock, initialRegisters } operationEnv =
    compileMachine instructionBlock initialRegisters operationEnv



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



-- =MachineState=


getRegister : Register -> MachineState -> Result RuntimeError Value
getRegister register machine =
    case Dict.get register machine.registerEnv of
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
    { machine | registerEnv = Dict.insert register val machine.registerEnv }


getOperation : OperationName -> MachineState -> Result RuntimeError (List Value -> Result RuntimeError Value)
getOperation operationName machine =
    case Dict.get operationName machine.operationEnv of
        Just op ->
            Ok op

        Nothing ->
            Err (UndefinedOperation operationName)



-- =Stack=


push : Value -> MachineState -> MachineState
push val machine =
    { machine | stack = Stack.push val machine.stack }



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



-- ===Running the Machine===


type RuntimeError
    = UndefinedRegister Register
    | UndefinedOperation OperationName
    | WrongNumberOfArgumentsGivenToOperationExpected Int
    | TheOperationExpectsIntegerArguments
    | PoppingEmptyStack
    | ExpectedInstructionPointerInRegister
    | ExpectedPairInRegister
    | MemoryError MemoryError
    | InstructionPointerOutOfBounds { instructionPointer : InstructionPointer, numberOfInstructions : Int }


type ComputationStep r a
    = RuntimeError RuntimeError
    | Halted r
    | Continue a


advance : MachineState -> ComputationStep r ( MachineState, ControllerChange )
advance machineState =
    Continue ( machineState, AdvanceInstructionPointer )


jumpTo : InstructionPointer -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
jumpTo instructionPointer machineState =
    Continue ( machineState, JumpTo instructionPointer )


fromResultAndThen : (a -> ComputationStep r b) -> Result RuntimeError a -> ComputationStep r b
fromResultAndThen f result =
    case result of
        Ok a ->
            f a

        Err e ->
            RuntimeError e


andThen : (a -> ComputationStep r b) -> ComputationStep r a -> ComputationStep r b
andThen f result =
    case result of
        RuntimeError e ->
            RuntimeError e

        Halted r ->
            Halted r

        Continue a ->
            f a


map : (a -> b) -> ComputationStep r a -> ComputationStep r b
map f result =
    andThen (f >> Continue) result


mapHalted : (r0 -> r1) -> ComputationStep r0 a -> ComputationStep r1 a
mapHalted f result =
    case result of
        RuntimeError e ->
            RuntimeError e

        Halted r ->
            Halted (f r)

        Continue a ->
            Continue a



-- ===START individual actions===
-- assignment


assignRegister : RegisterMachine.AssignRegisterInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
assignRegister { targetRegister, sourceRegister } machineState =
    getRegister sourceRegister machineState
        |> fromResultAndThen (\val -> advance (machineState |> updateRegister targetRegister val))


assignInstructionPointer : RegisterMachine.AssignInstructionPointerInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
assignInstructionPointer { targetRegister, instructionPointer } machineState =
    advance (machineState |> updateRegister targetRegister (InstructionPointer instructionPointer))


assignOperation : RegisterMachine.AssignOperationInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
assignOperation { targetRegister, operationApplication } machineState =
    getOperation operationApplication.name machineState
        |> fromResultAndThen
            (\op ->
                operationApplication.arguments
                    |> List.map (\argument -> machineState |> getValueFromArgument argument)
                    |> Result.sequence
                    |> Result.andThen op
                    |> fromResultAndThen (\output -> advance (machineState |> updateRegister targetRegister output))
            )


assignConstant : RegisterMachine.AssignConstantInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
assignConstant { targetRegister, constant } machineState =
    advance (machineState |> updateRegister targetRegister (ConstantValue constant))



-- jumping


jumpToInstructionPointer : RegisterMachine.JumpToInstructionPointerInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
jumpToInstructionPointer { instructionPointer } machineState =
    jumpTo instructionPointer machineState


jumpToInstructionPointerAtRegister : RegisterMachine.JumpToInstructionPointerAtRegisterInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
jumpToInstructionPointerAtRegister { instructionPointerRegister } machineState =
    getInstructionPointerAtRegister instructionPointerRegister machineState
        |> fromResultAndThen (\pointer -> jumpTo pointer machineState)


jumpToInstructionPointerIf : RegisterMachine.JumpToInstructionPointerIfInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
jumpToInstructionPointerIf { testRegister, instructionPointer } machineState =
    getRegister testRegister machineState
        |> fromResultAndThen
            (\val ->
                if val == ConstantValue (Num 1) then
                    jumpTo instructionPointer machineState

                else
                    advance machineState
            )


jumpToInstructionPointerAtRegisterIf : RegisterMachine.JumpToInstructionPointerAtRegisterIfInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
jumpToInstructionPointerAtRegisterIf { testRegister, instructionPointerRegister } machineState =
    getRegister testRegister machineState
        |> fromResultAndThen
            (\val ->
                if val == ConstantValue (Num 1) then
                    getInstructionPointerAtRegister instructionPointerRegister machineState
                        |> fromResultAndThen (\instructionPointer -> jumpTo instructionPointer machineState)

                else
                    advance machineState
            )


halt : RegisterMachine.HaltInput -> MachineState -> ComputationStep MachineState a
halt _ machineState =
    Halted machineState



-- stack


pushRegister : RegisterMachine.PushRegisterInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
pushRegister { sourceRegister } machineState =
    getRegister sourceRegister machineState
        |> fromResultAndThen (\val -> advance (machineState |> push val))


pushConstant : RegisterMachine.PushConstantInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
pushConstant { constant } machineState =
    advance (machineState |> push (ConstantValue constant))


pushInstructionPointer : RegisterMachine.PushInstructionPointerInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
pushInstructionPointer { instructionPointer } machineState =
    advance (machineState |> push (InstructionPointer instructionPointer))


pop : RegisterMachine.PopInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
pop { targetRegister } machineState =
    Stack.pop machineState.stack
        |> Result.fromMaybe PoppingEmptyStack
        |> fromResultAndThen (\( val, stack ) -> advance ({ machineState | stack = stack } |> updateRegister targetRegister val))



-- memory


constructPair : RegisterMachine.ConstructPairInput -> MemoryType -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
constructPair { targetRegister, operationArgument0, operationArgument1 } memoryType machineState =
    Result.tuple2 (machineState |> getValueFromArgument operationArgument0) (machineState |> getValueFromArgument operationArgument1)
        |> fromResultAndThen
            (\( value0, value1 ) ->
                (currentMemoryState Main machineState |> MemoryState.new ( value0, value1 ))
                    |> Result.mapError MemoryError
                    |> fromResultAndThen
                        (\( newPairPointer, newMemoryState ) ->
                            advance
                                (machineState
                                    |> setMemoryStateOfMachine memoryType newMemoryState
                                    |> updateRegister targetRegister (Pair newPairPointer)
                                )
                        )
            )


accessPair : MemoryCellComponent -> MemoryType -> Register -> Register -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
accessPair memoryCellComponent memoryType target source machineState =
    machineState
        |> getMemoryPointerAtRegister source
        |> fromResultAndThen
            (\pointer ->
                machineState
                    |> currentMemoryState memoryType
                    |> MemoryState.get pointer
                    |> Result.mapError MemoryError
                    |> fromResultAndThen
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


setPair : MemoryCellComponent -> MemoryType -> Register -> OperationArgument -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
setPair memoryCellComponent memoryType register arg machineState =
    Result.tuple2 (machineState |> getValueFromArgument arg) (machineState |> getMemoryPointerAtRegister register)
        |> fromResultAndThen
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


first : RegisterMachine.FirstInput -> MemoryType -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
first { targetRegister, sourceRegister } memoryType machineState =
    machineState |> accessPair FirstComponent memoryType targetRegister sourceRegister


second : RegisterMachine.SecondInput -> MemoryType -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
second { targetRegister, sourceRegister } memoryType machineState =
    machineState |> accessPair SecondComponent memoryType targetRegister sourceRegister


setFirst : RegisterMachine.SetFirstInput -> MemoryType -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
setFirst { targetRegister, operationArgument } memoryType machineState =
    machineState |> setPair FirstComponent memoryType targetRegister operationArgument


setSecond : RegisterMachine.SetSecondInput -> MemoryType -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
setSecond { targetRegister, operationArgument } memoryType machineState =
    machineState |> setPair SecondComponent memoryType targetRegister operationArgument



-- garbage collection


moveToDual : RegisterMachine.MoveToDualInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
moveToDual { targetRegister, sourceRegister } machineState =
    machineState
        |> getMemoryPointerAtRegister sourceRegister
        |> fromResultAndThen
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
                    |> fromResultAndThen
                        (\( newPairPointer, newDualMemoryState ) ->
                            advance
                                (machineState
                                    |> setDualMemoryStateOfMachine newDualMemoryState
                                    |> updateRegister targetRegister (Pair newPairPointer)
                                )
                        )
            )


markAsMoved : RegisterMachine.MarkAsMovedInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
markAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } machineState =
    Result.tuple2 (machineState |> getMemoryPointerAtRegister toBeCollectedFromRegister) (machineState |> getMemoryPointerAtRegister referenceToDualMemoryRegister)
        |> fromResultAndThen
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


swapMemory : RegisterMachine.SwapMemoryInput -> MachineState -> ComputationStep r ( MachineState, ControllerChange )
swapMemory _ ({ memory } as machineState) =
    advance { machineState | memory = { memory | memoryInUse = twoFlip memory.memoryInUse } }



-- ===END individual actions===


instructionAction : MachineInstruction -> MachineState -> ComputationStep MachineState ( MachineState, ControllerChange )
instructionAction instruction machineState =
    case instruction of
        MAssignRegister input ->
            assignRegister input machineState

        MAssignInstructionPointer input ->
            assignInstructionPointer input machineState

        MAssignOperation input ->
            assignOperation input machineState

        MAssignConstant input ->
            assignConstant input machineState

        MJumpToInstructionPointer input ->
            jumpToInstructionPointer input machineState

        MJumpToInstructionPointerAtRegister input ->
            jumpToInstructionPointerAtRegister input machineState

        MJumpToInstructionPointerIf input ->
            jumpToInstructionPointerIf input machineState

        MJumpToInstructionPointerAtRegisterIf input ->
            jumpToInstructionPointerAtRegisterIf input machineState

        MHalt input ->
            halt input machineState

        MPushRegister input ->
            pushRegister input machineState

        MPushConstant input ->
            pushConstant input machineState

        MPushInstructionPointer input ->
            pushInstructionPointer input machineState

        MPop input ->
            pop input machineState

        MConstructPair input ->
            constructPair input Main machineState

        MFirst input ->
            first input Main machineState

        MSecond input ->
            second input Main machineState

        MSetFirst input ->
            setFirst input Main machineState

        MSetSecond input ->
            setSecond input Main machineState

        MDualFirst input ->
            first input Dual machineState

        MDualSecond input ->
            second input Dual machineState

        MDualSetFirst input ->
            setFirst input Dual machineState

        MDualSetSecond input ->
            setSecond input Dual machineState

        MMoveToDual input ->
            moveToDual input machineState

        MMarkAsMoved input ->
            markAsMoved input machineState

        MSwapMemory input ->
            swapMemory input machineState


step : ControlledMachineState -> ComputationStep ControlledMachineState ControlledMachineState
step ({ machineState, controllerState } as machine) =
    fetchInstruction controllerState
        |> andThen
            (\instruction ->
                instructionAction instruction machineState
                    |> mapHalted
                        (\_ -> machine)
                    |> map
                        (\( newMachineState, controllerChange ) ->
                            { machine
                                | machineState = newMachineState
                                , controllerState = controllerChangeAction controllerChange controllerState
                            }
                        )
            )


isJumpInstruction : MachineInstruction -> Bool
isJumpInstruction instruction =
    case instruction of
        MJumpToInstructionPointer _ ->
            True

        MJumpToInstructionPointerAtRegister _ ->
            True

        MJumpToInstructionPointerIf _ ->
            True

        MJumpToInstructionPointerAtRegisterIf _ ->
            True

        _ ->
            False


stepUntilNextJump : ControlledMachineState -> ComputationStep ControlledMachineState ControlledMachineState
stepUntilNextJump machine0 =
    step machine0
        |> andThen
            (\({ controllerState } as machine1) ->
                fetchInstruction controllerState
                    |> andThen
                        (\instruction ->
                            if isJumpInstruction instruction then
                                Continue machine1

                            else
                                stepUntilNextJump machine1
                        )
            )


stepUntilHalted : ControlledMachineState -> ComputationStep ControlledMachineState ControlledMachineState
stepUntilHalted machine0 =
    step machine0 |> andThen (\machine1 -> stepUntilHalted machine1)


currentInstructionPointerFromComputationStep : ComputationStep ControlledMachineState ControlledMachineState -> Maybe InstructionPointer
currentInstructionPointerFromComputationStep result =
    case result of
        Continue machine ->
            Just machine.controllerState.currentInstructionPointer

        Halted machine ->
            Just machine.controllerState.currentInstructionPointer

        _ ->
            Nothing
