module RegisterMachine.Base exposing (..)

-- sequence of instructions should start with a label

import Array exposing (Array)
import Dict exposing (Dict)
import Lib.Result as Result
import Set exposing (Set)


type alias Label =
    String


type alias Register =
    String


type alias Value =
    Int


type alias Pointer =
    Int


type OperationApplication
    = Remainder Register Register
    | IsZero Register


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
    = -- assignment
      AssignRegister Register Register
    | AssignLabel Register Label
    | AssignOperation Register OperationApplication
    | AssignConstant Register Value
      -- jumping
    | JumpToLabel Label
    | JumpToLabelAtRegister Register
    | JumpToLabelIf Register Label
    | JumpToLabelAtRegisterIf Register Register -- first register is the test register, second register is the register containing the label
    | Halt


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
    , labelToPosition : Dict Label Pointer
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


parse : Controller -> Result TranslationError MachineInstructions
parse controller =
    let
        checkRegisterUse : Instruction -> Result TranslationError ()
        checkRegisterUse instruction =
            case instruction of
                AssignRegister target source ->
                    if Set.member target controller.registers then
                        if Set.member source controller.registers then
                            Ok ()

                        else
                            Err (UnknownRegister source)

                    else
                        Err (UnknownRegister target)

                AssignLabel target _ ->
                    if Set.member target controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister target)

                AssignOperation target _ ->
                    if Set.member target controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister target)

                AssignConstant target _ ->
                    if Set.member target controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister target)

                JumpToLabel label ->
                    Ok ()

                JumpToLabelAtRegister target ->
                    if Set.member target controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister target)

                JumpToLabelIf testRegister _ ->
                    if Set.member testRegister controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister testRegister)

                JumpToLabelAtRegisterIf testRegister target ->
                    if Set.member testRegister controller.registers then
                        if Set.member target controller.registers then
                            Ok ()

                        else
                            Err (UnknownRegister target)

                    else
                        Err (UnknownRegister testRegister)

                Halt ->
                    Ok ()

        initMachineInstructions : ( Pointer, MachineInstructions )
        initMachineInstructions =
            ( 0
            , { labels = Set.empty
              , labelToPosition = Dict.empty
              , instructions = Array.empty
              }
            )

        update : LabelOrInstruction -> ( Pointer, MachineInstructions ) -> Result TranslationError ( Pointer, MachineInstructions )
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


emptyStack : Stack
emptyStack =
    []



-- === Machine ===


type alias Machine =
    { env : Dict Register Value
    , stack : Stack
    , instructionPointer : Pointer
    , instructions : MachineInstructions
    }


makeMachine : Controller -> Dict Register Value -> Result TranslationError Machine
makeMachine controller env =
    parse controller
        |> Result.map
            (\instructions ->
                { env = env
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


updateRegister : Register -> Value -> Machine -> Machine
updateRegister register val machine =
    { machine
        | env = Dict.insert register val machine.env
    }


getLabelPosition : Label -> Machine -> Maybe Pointer
getLabelPosition label machine =
    Dict.get label machine.instructions.labelToPosition


jump : Label -> Machine -> Machine
jump label machine =
    case getLabelPosition label machine of
        Just pointer ->
            pointerJump pointer machine

        Nothing ->
            machine


pointerJump : Pointer -> Machine -> Machine
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
    | LabelPointsToNothing Label
    | PoppingEmptyStack


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
                        Just i ->
                            Ok
                                { isFinished = False
                                , machine =
                                    machine
                                        |> updateRegister target i
                                        |> advanceInstructionPointer
                                }

                        Nothing ->
                            Err (LabelPointsToNothing label)

                AssignOperation target operationApplication ->
                    case operationApplication of
                        Remainder reg_a reg_b ->
                            Result.tuple2 (getRegister reg_a machine) (getRegister reg_b machine)
                                |> Result.map
                                    (\( a, b ) ->
                                        { isFinished = False
                                        , machine =
                                            machine
                                                |> updateRegister target (modBy b a)
                                                |> advanceInstructionPointer
                                        }
                                    )

                        IsZero reg_a ->
                            getRegister reg_a machine
                                |> Result.map
                                    (\a ->
                                        { isFinished = False
                                        , machine =
                                            machine
                                                |> updateRegister target
                                                    (if a == 0 then
                                                        1

                                                     else
                                                        0
                                                    )
                                                |> advanceInstructionPointer
                                        }
                                    )

                AssignConstant target x ->
                    Ok
                        { isFinished = False
                        , machine =
                            machine
                                |> updateRegister target x
                                |> advanceInstructionPointer
                        }

                JumpToLabel label ->
                    Ok { isFinished = False, machine = jump label machine }

                JumpToLabelAtRegister register ->
                    getRegister register machine
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
                                    if val == 1 then
                                        jump label machine

                                    else
                                        advanceInstructionPointer machine
                                }
                            )

                JumpToLabelAtRegisterIf testRegister target ->
                    getRegister testRegister machine
                        |> Result.andThen
                            (\val ->
                                if val == 1 then
                                    getRegister target machine
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


controller0_gcd : Controller
controller0_gcd =
    -- registers: a : Int, b : Int, tmp : Int, is-zero-b? : Bool
    -- labels: loop, done
    --
    -- label loop:
    --   is-b-zero? <- zero?(b)
    --   tmp <- remainder(a, b)
    --   a <- b
    --   b <- tmp
    --   jump loop
    -- done:
    --
    -- label loop -> 0
    -- done -> 6
    --   0: is-b-zero? <- zero? b
    --   1: jump-if is-b-zero? done
    --   2: tmp <- remainder(a b)
    --   3: a <- b
    --   4: b <- tmp
    --   5: jump loop
    --   6: halt
    --
    { registers = Set.fromList [ "a", "b", "tmp", "is-b-zero?", "label-test" ]
    , instructions =
        [ Label "loop"
        , Perform (AssignOperation "is-b-zero?" (IsZero "b"))
        , Perform (JumpToLabelIf "is-b-zero?" "done")
        , Perform (AssignOperation "tmp" (Remainder "a" "b"))
        , Perform (AssignRegister "a" "b")
        , Perform (AssignRegister "b" "tmp")
        , Perform (JumpToLabel "loop")
        , Label "done"
        , Perform Halt
        ]
    }


controller1_remainder : Controller
controller1_remainder =
    { registers = Set.fromList [ "a", "b", "label-place" ]
    , instructions =
        [ Perform (AssignLabel "label-place" "done")
        , Perform (AssignConstant "a" 1)
        , Perform (JumpToLabelAtRegisterIf "a" "label-place")
        , Perform (AssignConstant "b" 321)
        , Label "done"
        , Perform Halt
        ]
    }


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

        AssignOperation target operation ->
            showAssignment
                target
                (case operation of
                    Remainder a b ->
                        String.concat [ "remainder(", showRegisterUse a, ",", showRegisterUse b, ")" ]

                    IsZero a ->
                        String.concat [ "is-zero?(", showRegisterUse a, ")" ]
                )

        AssignConstant target x ->
            showAssignment target (String.fromInt x)

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
