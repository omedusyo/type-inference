module RegisterMachine.Controllers exposing (..)

import RegisterMachine.Base as RegisterMachine
    exposing
        ( Controller
        , Instruction(..)
        , Label
        , LabelOrInstruction(..)
        , OperationApplication(..)
        , Register
        )
import Set exposing (Set)


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
        , Perform (AssignOperation "is-b-zero?" (Operation "is-zero?" [ "b" ]))
        , Perform (JumpToLabelIf "is-b-zero?" "done")
        , Perform (AssignOperation "tmp" (Operation "remainder" [ "a", "b" ]))
        , Perform (AssignRegister "a" "b")
        , Perform (AssignRegister "b" "tmp")
        , Perform (JumpToLabel "loop")
        , Label "done"
        , Perform Halt
        ]
    }


controller1_remainder : Controller
controller1_remainder =
    -- What's the algorithm?
    -- subtract $b from $a and assign it into $a, until $a <- $b is true. Then halt. The result is in the register $a.
    --
    -- registers: a, b, is-finished?
    -- start:
    --   is-finished <- less-than?($a, $b)
    --   if $is-finished? jump :done
    --   $a <- sub($a, $b)
    --   jump $start
    -- done:
    --   halt
    { registers = Set.fromList [ "a", "b", "is-finished?" ]
    , instructions =
        [ Perform (AssignConstant "a" 16)
        , Perform (AssignConstant "b" 3)
        , Label "start"
        , Perform (AssignOperation "is-finished?" (Operation "less-than?" [ "a", "b" ]))
        , Perform (JumpToLabelIf "is-finished?" "done")
        , Perform (AssignOperation "a" (Operation "sub" [ "a", "b" ]))
        , Perform (JumpToLabel "start")
        , Label "done"
        , Perform Halt
        ]
    }
