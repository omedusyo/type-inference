module RegisterMachine.Base exposing (..)

-- sequence of instructions should start with a label

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Label =
    String


type alias Register =
    String


type OperationApplication
    = Remainder Register Register
    | IsZero Register


type Instruction
    = Assign Register Register
    | AssignOperation Register OperationApplication
    | JumpIf Register Label
    | Jump Label
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
    , labelToPosition : Dict Label Int
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
                Assign target source ->
                    if Set.member target controller.registers then
                        if Set.member source controller.registers then
                            Ok ()

                        else
                            Err (UnknownRegister source)

                    else
                        Err (UnknownRegister target)

                AssignOperation target _ ->
                    if Set.member target controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister target)

                JumpIf source _ ->
                    if Set.member source controller.registers then
                        Ok ()

                    else
                        Err (UnknownRegister source)

                Jump label ->
                    Ok ()

                Halt ->
                    Ok ()

        initMachineInstructions : ( Int, MachineInstructions )
        initMachineInstructions =
            ( 0
            , { labels = Set.empty
              , labelToPosition = Dict.empty
              , instructions = Array.empty
              }
            )

        update : LabelOrInstruction -> ( Int, MachineInstructions ) -> Result TranslationError ( Int, MachineInstructions )
        update labelOrInstruction ( i, machineInstructions ) =
            case labelOrInstruction of
                Label label ->
                    if Set.member label machineInstructions.labels then
                        Err (LabelUsedMoreThanOnce label)

                    else
                        Ok
                            ( i
                            , { machineInstructions
                                | labels = Set.insert label machineInstructions.labels
                                , labelToPosition =
                                    Dict.insert label i machineInstructions.labelToPosition
                              }
                            )

                Perform instruction ->
                    Ok
                        ( i + 1
                        , { machineInstructions
                            | instructions =
                                Array.push instruction machineInstructions.instructions
                          }
                        )
    in
    foldlMayFail update initMachineInstructions controller.instructions
        |> Result.map Tuple.second


type alias Machine =
    { env : Dict Register Int
    , instructionPointer : Int
    , instructions : MachineInstructions
    }


makeMachine : Controller -> Dict Register Int -> Dict Register Bool -> Result TranslationError Machine
makeMachine controller initIntEnv initBoolEnv =
    parse controller
        |> Result.map
            (\instructions ->
                { env = initIntEnv
                , instructionPointer = 0
                , instructions = instructions
                }
            )


updateMachine : Machine -> Machine
updateMachine machine =
    Debug.todo ""


controller0 : Controller
controller0 =
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
    --
    { registers = Set.fromList [ "a", "b", "tmp", "is-b-zero?" ]
    , instructions =
        [ Label "loop"
        , Perform (AssignOperation "is-b-zero?" (IsZero "b"))
        , Perform (JumpIf "is-b-zero?" "done")
        , Perform (AssignOperation "tmp" (Remainder "a" "b"))
        , Perform (Assign "a" "b")
        , Perform (Assign "b" "tmp")
        , Perform (Jump "loop")
        , Label "done"
        ]
    }


showInstruction : Instruction -> String
showInstruction instruction =
    case instruction of
        Assign target source ->
            target ++ " <- " ++ source

        AssignOperation target operation ->
            String.concat
                [ target
                , " <- "
                , case operation of
                    Remainder a b ->
                        String.concat [ "remainder(", a, ",", b, ")" ]

                    IsZero a ->
                        String.concat [ "is-zero?(", a, ")" ]
                ]

        JumpIf source label ->
            String.concat [ "jump-if ", source, " ", label ]

        Jump label ->
            String.concat [ "jump ", label ]

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
