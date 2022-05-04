module RegisterMachine.Controllers exposing (..)

import Dict exposing (Dict)
import RegisterMachine.Base as RegisterMachine
    exposing
        ( Constant(..)
        , Controller
        , Instruction(..)
        , Label
        , LabelOrInstruction(..)
        , OperationApplication(..)
        , OperationArgument(..)
        , Register
        , RegisterEnvironment
        , Value(..)
        )
import Set exposing (Set)


num x =
    ConstantValue (Num x)


nil =
    ConstantValue Nil


controller0_gcd : ( Controller, RegisterEnvironment )
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
    ( { registers = Set.fromList [ "a", "b", "tmp", "is-b-zero?", "label-test" ]
      , instructions =
            [ Label "loop"
            , Perform (AssignOperation "is-b-zero?" (Operation "zero?" [ Register "b" ]))
            , Perform (JumpToLabelIf "is-b-zero?" "done")
            , Perform (AssignOperation "tmp" (Operation "remainder" [ Register "a", Register "b" ]))
            , Perform (AssignRegister "a" "b")
            , Perform (AssignRegister "b" "tmp")
            , Perform (JumpToLabel "loop")
            , Label "done"
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "a", num (3 * 5 * 7) ), ( "b", num (3 * 5 * 5) ), ( "tmp", num 0 ), ( "is-b-zero?", num 0 ) ]
    )


controller1_remainder : ( Controller, RegisterEnvironment )
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
    ( { registers = Set.fromList [ "a", "b", "is-finished?" ]
      , instructions =
            [ Perform (AssignConstant "a" (Num 16))
            , Perform (AssignConstant "b" (Num 3))
            , Label "start"
            , Perform (AssignOperation "done?" (Operation "less-than?" [ Register "a", Register "b" ]))
            , Perform (JumpToLabelIf "done?" "done")
            , Perform (AssignOperation "a" (Operation "sub" [ Register "a", Register "b" ]))
            , Perform (JumpToLabel "start")
            , Label "done"
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "a", num 0 ), ( "b", num 15 ), ( "done?", num 0 ) ]
    )


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
    ( { registers = Set.fromList [ "counter", "state", "done?" ]
      , instructions =
            [ Perform (AssignConstant "counter" (Num 5))
            , Perform (AssignConstant "state" (Num 1))
            , Label "loop"
            , Perform (AssignOperation "done?" (Operation "zero?" [ Register "counter" ]))
            , Perform (JumpToLabelIf "done?" "done")
            , Perform (AssignOperation "state" (Operation "mul" [ Register "state", Register "counter" ]))
            , Perform (AssignOperation "counter" (Operation "decrement" [ Register "counter" ]))
            , Perform (JumpToLabel "loop")
            , Label "done"
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "counter", num 0 ), ( "state", num 0 ), ( "done?", num 0 ) ]
    )


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
    ( { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?" ]
      , instructions =
            [ Perform (AssignConstant "a" (Num (3 * 5 * 7)))
            , Perform (AssignConstant "b" (Num (3 * 5 * 5)))
            , Label "gcd-loop"
            , Perform (AssignOperation "done?" (Operation "zero?" [ Register "b" ]))
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
            , Perform (AssignOperation "remainder-done?" (Operation "less-than?" [ Register "remainder-result", Register "b" ]))
            , Perform (JumpToLabelIf "remainder-done?" "after-remainder-done")
            , Perform (AssignOperation "remainder-result" (Operation "sub" [ Register "remainder-result", Register "b" ]))
            , Perform (JumpToLabel "remainder")
            , Label "done"
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "a", num 0 ), ( "b", num 0 ), ( "remainder-result", num 0 ), ( "done?", num 0 ), ( "remainder-done?", num 0 ) ]
    )


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
    ( { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?", "continue" ]
      , instructions =
            [ Perform (AssignConstant "a" (Num (3 * 5 * 7)))
            , Perform (AssignConstant "b" (Num (3 * 5 * 5)))
            , Label "gcd-loop"
            , Perform (AssignOperation "done?" (Operation "zero?" [ Register "b" ]))
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
            , Perform (AssignOperation "remainder-done?" (Operation "less-than?" [ Register "remainder-result", Register "b" ]))
            , Perform (JumpToLabelAtRegisterIf "remainder-done?" "continue")
            , Perform (AssignOperation "remainder-result" (Operation "sub" [ Register "remainder-result", Register "b" ]))
            , Perform (JumpToLabel "remainder")
            , Label "done"
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "a", num 0 ), ( "b", num 0 ), ( "remainder-result", num 0 ), ( "done?", num 0 ), ( "remainder-done?", num 0 ), ( "continue", num 0 ) ]
    )


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
    { registers = Set.fromList []
    , instructions =
        []
    }


controller6_fct_recursive =
    -- fct:
    --   done? <- $n == 0
    --   if $test call :done
    --   push $continue
    --   push $n
    --   n <- $n - 1
    --   continue <- call :fct
    --   n <- pop
    --   continue <- pop
    --   result <- $n * $result
    --   jump $continue
    -- done:
    --   result <- 1
    --   jump $continue
    ( { registers = Set.fromList [ "n", "result", "done?" ]
      , instructions =
            [ Perform (AssignConstant "n" (Num 5))
            , Perform (AssignCallAtLabel "continue" "fct")
            , Perform Halt
            , Label "fct"
            , Perform (AssignOperation "done?" (Operation "zero?" [ Register "n" ]))
            , Perform (JumpToLabelIf "done?" "done")
            , Perform (PushRegister "continue")
            , Perform (PushRegister "n")
            , Perform (AssignOperation "n" (Operation "decrement" [ Register "n" ]))
            , Perform (AssignCallAtLabel "continue" "fct")
            , Perform (Pop "n")
            , Perform (Pop "continue")
            , Perform (AssignOperation "result" (Operation "mul" [ Register "n", Register "result" ]))
            , Perform (JumpToLabelAtRegister "continue")
            , Label "done"
            , Perform (AssignConstant "result" (Num 1))
            , Perform (JumpToLabelAtRegister "continue")
            ]
      }
    , Dict.fromList [ ( "n", num 0 ), ( "result", num 0 ), ( "done?", num 0 ), ( "continue", num 0 ) ]
    )


controller7_fibonacci_recursive =
    -- registers: n, result, tmp, done?, continue
    --
    -- fib:
    --   done? <- $n < 2
    --   if $done? jump :done
    --   n <- $n - 1
    --   push $n
    --   push $continue
    --   continue <- jump :fib
    --   continue <- pop
    --   n <- pop
    --   n <- $n - 1
    --   push $result
    --   push $continue
    --   continue <- jump :fib
    --   continue <- pop
    --   tmp <- pop
    --   result <- result + tmp
    --   jump $continue
    -- done:
    --   result <- $n
    --   jump $continue
    ( { registers = Set.fromList [ "n", "result", "tmp", "done?", "continue" ]
      , instructions =
            [ Perform (AssignConstant "n" (Num 8))
            , Perform (AssignCallAtLabel "continue" "fib")
            , Perform Halt
            , Label "fib"
            , Perform (AssignOperation "done?" (Operation "less-than?" [ Register "n", Constant (Num 2) ]))
            , Perform (JumpToLabelIf "done?" "done")

            -- call to fib(n - 1)
            , Perform (AssignOperation "n" (Operation "decrement" [ Register "n" ]))
            , Perform (PushRegister "n")
            , Perform (PushRegister "continue")
            , Perform (AssignCallAtLabel "continue" "fib")
            , Perform (Pop "continue")
            , Perform (Pop "n")

            -- call to fib(n - 2)
            , Perform (AssignOperation "n" (Operation "decrement" [ Register "n" ]))
            , Perform (PushRegister "result")
            , Perform (PushRegister "continue")
            , Perform (AssignCallAtLabel "continue" "fib")
            , Perform (Pop "continue")
            , Perform (Pop "tmp")

            -- returning fib(n - 1) + fib(n - 2)
            , Perform (AssignOperation "result" (Operation "add" [ Register "result", Register "tmp" ]))
            , Perform (JumpToLabelAtRegister "continue")

            -- base case
            , Label "done"
            , Perform (AssignRegister "result" "n")
            , Perform (JumpToLabelAtRegister "continue")
            ]
      }
    , Dict.fromList [ ( "n", num 0 ), ( "result", num 0 ), ( "tmp", num 0 ), ( "done?", num 0 ), ( "continue", num 0 ) ]
    )


controller8_memory_test =
    ( { registers = Set.fromList [ "p", "a", "b", "test" ]
      , instructions =
            [ Label "memory_test"
            , Perform (ConstructPair "p" (Constant (Num 16)) (Constant (Num 32)))
            , Perform (First "a" "p")
            , Perform (Second "b" "p")
            , Perform (SetFirst "p" (Constant (Num 17)))
            , Perform (SetSecond "p" (Constant (Num 33)))
            , Perform (AssignOperation "test" (Operation "pair?" [ Register "p" ]))
            , Perform Halt
            ]
      }
    , Dict.fromList [ ( "p", nil ), ( "a", num 0 ), ( "b", num 0 ), ( "test", num 0 ) ]
    )
