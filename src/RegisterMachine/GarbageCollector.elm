module RegisterMachine.GarbageCollector exposing (..)

import Dict exposing (Dict)
import RegisterMachine.Base as RegisterMachine
    exposing
        ( Constant(..)
        , Instruction(..)
        , Label
        , OperationApplication(..)
        , OperationArgument(..)
        , Register
        , Value(..)
        )
import RegisterMachine.Machine as RegisterMachine exposing (Controller, InstructionBlock, LabelOrInstruction(..), RegisterEnvironment)
import Set exposing (Set)


init : List Register -> InstructionBlock -> ( Controller, RegisterEnvironment )
init registers instructions =
    ( { registers = Set.fromList registers
      , instructions = instructions
      }
    , Dict.fromList (List.map (\register -> ( register, Uninitialized )) registers)
    )


controller =
    init [ "xs", "ys", "main-pair", "dual-pair", "tmp", "root", "test", "continue" ]
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
        , Perform (ConstructPair "ys" (Constant (Num 16)) (Constant Nil))
        , Perform (ConstructPair "xs" (Constant (Num 16)) (Register "ys"))
        , Label "initializing root"
        , Perform (ConstructPair "root" (Register "xs") (Constant Nil))
        , Perform (ConstructPair "root" (Register "ys") (Register "root"))
        , Perform (AssignRegister "to-be-moved" "root")
        , Label "start garbage collection"
        , Perform (AssignOperation "test" (Operation "nil?" [ Register "to-be-moved" ]))
        , Perform (JumpToLabelIf "test" "refresh root")
        , Perform (First "main-pair" "to-be-moved")
        , Perform (Second "to-be-moved" "to-be-moved")
        , Perform (AssignLabel "continue" "start garbage collection")

        -- BEGIN PROCEDURE: move-pair
        --   dual-pair <- move-pair($main-pair, $continue)
        , Label "move-pair"
        , Perform (MoveToDual "dual-pair" "main-pair")
        , Perform (MarkAsMoved "main-pair" "dual-pair")

        -- The following label isn't actually needed
        , Label "attempt to move first component"
        , Perform (DualFirst "main-pair" "dual-pair")
        , Perform (AssignOperation "test" (Operation "pair?" [ Register "main-pair" ]))
        , Perform (JumpToLabelIf "test" "move first component")
        , Label "attempt to move second component"
        , Perform (DualSecond "main-pair" "dual-pair")
        , Perform (AssignOperation "test" (Operation "pair?" [ Register "main-pair" ]))
        , Perform (JumpToLabelIf "test" "move second component")
        , Perform (JumpToLabelAtRegister "continue")
        , Label "move second component"
        , Perform (PushRegister "dual-pair")
        , Perform (PushRegister "continue")
        , Perform (AssignLabel "continue" "after second call")
        , Perform (JumpToLabel "move-pair")
        , Label "after second call"
        , Perform (AssignRegister "tmp" "dual-pair")
        , Perform (Pop "continue")
        , Perform (Pop "dual-pair")
        , Perform (DualSetSecond "dual-pair" (Register "tmp"))
        , Perform (JumpToLabelAtRegister "continue")
        , Label "move first component"
        , Perform (PushRegister "dual-pair")
        , Perform (PushRegister "continue")
        , Perform (AssignLabel "continue" "after first call")
        , Perform (JumpToLabel "move-pair")
        , Label "after first call"
        , Perform (AssignRegister "tmp" "dual-pair")
        , Perform (Pop "continue")
        , Perform (Pop "dual-pair")
        , Perform (DualSetFirst "dual-pair" (Register "tmp"))
        , Perform (JumpToLabel "attempt to move second component")

        -- END PROCEDURE: move-pair
        , Label "refresh root"

        -- TODO
        , Label "done"
        , Perform SwapMemory
        , Perform Halt
        ]
