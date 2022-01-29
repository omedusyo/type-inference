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
    -- registers: a, b, done?
    -- start:
    --   done <- less-than?($a, $b)
    --   if $done? jump :done
    --   $a <- sub($a, $b)
    --   jump $start
    -- done:
    --   halt
    { registers = Set.fromList [ "a", "b", "is-finished?" ]
    , instructions =
        [ Perform (AssignConstant "a" 16)
        , Perform (AssignConstant "b" 3)
        , Label "start"
        , Perform (AssignOperation "done?" (Operation "less-than?" [ "a", "b" ]))
        , Perform (JumpToLabelIf "done?" "done")
        , Perform (AssignOperation "a" (Operation "sub" [ "a", "b" ]))
        , Perform (JumpToLabel "start")
        , Label "done"
        , Perform Halt
        ]
    }


controller2_fct_iterative =
    -- registers: counter, state, done?
    --   counter <- 5
    --   state <- 0
    -- label loop
    --   done? <- zero?($counter)
    --   if $done? jump :done
    --   state <- mul($state, $counter)
    --   counter <- decrement($counter)
    --   jump :loop
    -- label done
    --   halt
    { registers = Set.fromList [ "counter", "state", "done?" ]
    , instructions =
        [ Perform (AssignConstant "counter" 5)
        , Perform (AssignConstant "state" 1)
        , Label "loop"
        , Perform (AssignOperation "done?" (Operation "zero?" [ "counter" ]))
        , Perform (JumpToLabelIf "done?" "done")
        , Perform (AssignOperation "state" (Operation "mul" [ "state", "counter" ]))
        , Perform (AssignOperation "counter" (Operation "decrement" [ "counter" ]))
        , Perform (JumpToLabel "loop")
        , Label "done"
        , Perform Halt
        ]
    }


controller3_gcd_with_inlined_remainder =
    -- gcd: a, b, remainder-result, done?, remainder-done?
    --   // I don't really need the `remainder-done?` register, I could reuse `done?`
    --   a <- 15
    --   b <- 12
    -- gcd-loop:
    --   done? <- zero?($b)
    --   if $done? jump :done
    --   // This is the remainder call remainder($remainder-result, $b)
    --   remainder-result <- $a
    --   jump :remainder
    -- after-remainder-done:
    --   a <- $b
    --   b <- $remainder-result
    --   jump :gcd-loop
    -- remainder:
    --   remainder-done? <- less-than?($remainder-result, $b)
    --   if $remainder-done? jump :after-remainder-done
    --   remainder-result <- sub($remainder-result, $b)
    --   jump :remainder
    -- done:
    --   halt
    { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?" ]
    , instructions =
        [ Perform (AssignConstant "a" (3 * 5 * 7))
        , Perform (AssignConstant "b" (3 * 5 * 5))
        , Label "gcd-loop"
        , Perform (AssignOperation "done?" (Operation "zero?" [ "b" ]))
        , Perform (JumpToLabelIf "done?" "done")

        -- BEGIN Calling remainder($remainder-result, $b)
        , Perform (AssignRegister "remainder-result" "a")
        , Perform (JumpToLabel "remainder")
        , Label "after-remainder-done"

        -- END Calling remainder
        , Perform (AssignRegister "a" "b")
        , Perform (AssignRegister "b" "remainder-result")
        , Perform (JumpToLabel "gcd-loop")

        -- This is the remainder procedure
        , Label "remainder"
        , Perform (AssignOperation "remainder-done?" (Operation "less-than?" [ "remainder-result", "b" ]))
        , Perform (JumpToLabelIf "remainder-done?" "after-remainder-done")
        , Perform (AssignOperation "remainder-result" (Operation "sub" [ "remainder-result", "b" ]))
        , Perform (JumpToLabel "remainder")
        , Label "done"
        , Perform Halt
        ]
    }


controller4_gcd_with_inlined_remainder_using_jump =
    -- gcd registers: a, b, remainder-result, done?, remainder-done?, continue
    --   a <- 15
    --   b <- 12
    -- gcd-loop:
    --   done? <- zero?($b)
    --   if $done? jump :done
    --   // This is the remainder call remainder($remainder-result, $b)
    --   remainder-result <- $a
    --   :continue <- call :remainder
    --   a <- $b
    --   b <- $tmp
    --   jump :gcd-loop
    -- remainder:
    --   remainder-done? <- less-than?($remainder-result, $b)
    --   if $remainder-done? jump $continue
    --   remainder-result <- sub($remainder-result, $b)
    --   jump :remainder
    -- done:
    --   halt
    { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?", "continue" ]
    , instructions =
        [ Perform (AssignConstant "a" (3 * 5 * 7))
        , Perform (AssignConstant "b" (3 * 5 * 5))
        , Label "gcd-loop"
        , Perform (AssignOperation "done?" (Operation "zero?" [ "b" ]))
        , Perform (JumpToLabelIf "done?" "done")

        -- BEGIN Calling remainder($remainder-result, $b)
        , Perform (AssignRegister "remainder-result" "a")
        , Perform (AssignCallAtLabel "continue" "remainder")

        -- END Calling remainder
        , Perform (AssignRegister "a" "b")
        , Perform (AssignRegister "b" "remainder-result")
        , Perform (JumpToLabel "gcd-loop")

        -- This is the remainder procedure
        , Label "remainder"
        , Perform (AssignOperation "remainder-done?" (Operation "less-than?" [ "remainder-result", "b" ]))
        , Perform (JumpToLabelAtRegisterIf "remainder-done?" "continue")
        , Perform (AssignOperation "remainder-result" (Operation "sub" [ "remainder-result", "b" ]))
        , Perform (JumpToLabel "remainder")
        , Label "done"
        , Perform Halt
        ]
    }


controller5_sqrt =
    -- TODO: you'd need Float values for this
    -- sqrt x =
    --   let good-enoguh? guess = abs (x - (guess * guess)) < 0.001;
    --   let improve guess = average guess (x / guess);
    --   let sqrt-iter guess =
    --     if good-enough? guess
    --       { true -> guess }
    --       { false -> good-enough? (improve guess) }
    --   ;
    --   sqrt-iter 1.0
    -- registers: x, guess
    --   good-enough?:
    --     tmp <- $guess * $guess
    --     tmp <- $x - $tmp
    --     tmp <- abs $tmp
    --     tmp <- $tmp < 0.001
    --     jump $continue
    --   improve:
    --     tmp <- $x / $guess
    --     tmp <- $guess + $tmp
    --     guess <- $tmp / 2
    --   sqrt-iter:
    --     continue <- jump :good-enough?
    --     if $tmp jump :done
    --     continue <- jump :improve
    --     jump :sqrt-iter
    --   done:
    --     halt
    { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?", "continue" ]
    , instructions =
        [ Perform (AssignConstant "a" 0)
        , Label "done"
        , Perform Halt
        ]
    }
