module RegisterMachine.GarbageCollector exposing (..)

import Dict
import RegisterMachine.Base as RegisterMachine
    exposing
        ( Constant(..)
        , Instruction(..)
        , Label
        , OperationArgument(..)
        , Register
        , Value(..)
        )
import RegisterMachine.Machine as RegisterMachine exposing (ControllerExample, InstructionBlock, LabelOrInstruction(..))
import Set


init : String -> List Register -> InstructionBlock -> ControllerExample
init name registers instructions =
    { name = name
    , controller =
        { registers = registers
        , instructions = instructions
        }
    , initialRegisterEnvironment = Dict.fromList (List.map (\register -> ( register, Uninitialized )) registers)
    }


controller : ControllerExample
controller =
    init "garbage-collector"
        [ "xs", "ys", "main-pair", "dual-pair", "tmp", "root", "test", "continue" ]
        [ Label "initialization"

        -- xs <- list(10, 20, 30, 40)
        -- , Perform (ConstructPair "xs" (Constant (Num 40)) (Constant Nil))
        -- , Perform (ConstructPair "xs" (Constant (Num 30)) (Register "xs"))
        -- , Perform (ConstructPair "xs" (Constant (Num 20)) (Register "xs"))
        -- , Perform (ConstructPair "xs" (Constant (Num 10)) (Register "xs"))
        -- xs <- ((10, nil), (20, nil))
        -- , Perform (ConstructPair "xs" (Constant (Num 10)) (Constant Nil))
        -- , Perform (ConstructPair "ys" (Constant (Num 20)) (Constant Nil))
        -- , Perform (ConstructPair "xs" (Register "xs") (Register "ys"))
        -- ys <- (16, nil)
        -- xs <- (32, ys)
        -- root <- list(ys, xs)
        , Perform (ConstructPair { targetRegister = "ys", operationArgument0 = Constant (Num 16), operationArgument1 = Constant Nil })
        , Perform (ConstructPair { targetRegister = "xs", operationArgument0 = Constant (Num 32), operationArgument1 = Register "ys" })
        , Label "initializing root"
        , Perform (ConstructPair { targetRegister = "root", operationArgument0 = Register "xs", operationArgument1 = Constant Nil })
        , Perform (ConstructPair { targetRegister = "root", operationArgument0 = Register "ys", operationArgument1 = Register "root" })
        , Perform (AssignRegister { targetRegister = "to-be-moved", sourceRegister = "root" })
        , Label "start garbage collection"
        , Perform (AssignOperation { targetRegister = "test", operationApplication = { name = "nil?", arguments = [ Register "to-be-moved" ] } })
        , Perform (JumpToLabelIf { testRegister = "test", label = "refresh root" })
        , Perform (First { targetRegister = "main-pair", sourceRegister = "to-be-moved" })
        , Perform (Second { targetRegister = "to-be-moved", sourceRegister = "to-be-moved" })
        , Perform (AssignLabel { targetRegister = "continue", label = "start garbage collection" })

        -- BEGIN PROCEDURE: move-pair
        --   dual-pair <- move-pair($main-pair, $continue)
        , Label "move-pair"
        , Perform (First { targetRegister = "tmp", sourceRegister = "main-pair" })
        , Perform (AssignOperation { targetRegister = "test", operationApplication = { name = "moved?", arguments = [ Register "tmp" ] } })
        , Perform (JumpToLabelIf { testRegister = "test", label = "already moved" })
        , Perform (MoveToDual { targetRegister = "dual-pair", sourceRegister = "main-pair" })
        , Perform (MarkAsMoved { toBeCollectedFromRegister = "main-pair", referenceToDualMemoryRegister = "dual-pair" })

        -- The following label isn't actually needed
        , Label "attempt to move first component"
        , Perform (AssignRegister { targetRegister = "main-pair", sourceRegister = "tmp" })
        , Perform (AssignOperation { targetRegister = "test", operationApplication = { name = "pair?", arguments = [ Register "main-pair" ] } })
        , Perform (JumpToLabelIf { testRegister = "test", label = "move first component" })
        , Label "attempt to move second component"
        , Perform (DualSecond { targetRegister = "main-pair", sourceRegister = "dual-pair" })
        , Perform (AssignOperation { targetRegister = "test", operationApplication = { name = "pair?", arguments = [ Register "main-pair" ] } })
        , Perform (JumpToLabelIf { testRegister = "test", label = "move second component" })
        , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })
        , Label "move second component"
        , Perform (PushRegister { sourceRegister = "dual-pair" })
        , Perform (PushRegister { sourceRegister = "continue" })
        , Perform (AssignLabel { targetRegister = "continue", label = "after second call" })
        , Perform (JumpToLabel { label = "move-pair" })
        , Label "after second call"
        , Perform (AssignRegister { targetRegister = "tmp", sourceRegister = "dual-pair" })
        , Perform (Pop { targetRegister = "continue" })
        , Perform (Pop { targetRegister = "dual-pair" })
        , Perform (DualSetSecond { targetRegister = "dual-pair", operationArgument = Register "tmp" })
        , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })
        , Label "move first component"
        , Perform (PushRegister { sourceRegister = "dual-pair" })
        , Perform (PushRegister { sourceRegister = "continue" })
        , Perform (AssignLabel { targetRegister = "continue", label = "after first call" })
        , Perform (JumpToLabel { label = "move-pair" })
        , Label "after first call"
        , Perform (AssignRegister { targetRegister = "tmp", sourceRegister = "dual-pair" })
        , Perform (Pop { targetRegister = "continue" })
        , Perform (Pop { targetRegister = "dual-pair" })
        , Perform (DualSetFirst { targetRegister = "dual-pair", operationArgument = Register "tmp" })
        , Perform (JumpToLabel { label = "attempt to move second component" })
        , Label "already moved"
        , Perform (Second { targetRegister = "dual-pair", sourceRegister = "main-pair" })
        , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })

        -- END PROCEDURE: move-pair
        , Label "refresh root"

        -- TODO
        , Label "done"
        , Perform (SwapMemory {})
        , Perform (Halt {})
        ]
