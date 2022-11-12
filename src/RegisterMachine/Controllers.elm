module RegisterMachine.Controllers exposing (..)

import Dict
import RegisterMachine.Base as RegisterMachine
    exposing
        ( Constant(..)
        , Instruction(..)
        , Label
        , OperationApplication
        , OperationArgument(..)
        , Register
        , Value(..)
        )
import RegisterMachine.Machine as RegisterMachine exposing (ControllerExample, LabelOrInstruction(..))
import Set


num : Int -> Value
num x =
    ConstantValue (Num x)


nil : Value
nil =
    ConstantValue Nil


controller0_gcd : ControllerExample
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
    let
        a =
            [ 3, 5, 7 ]

        b =
            [ 3, 5, 11 ]
    in
    { name =
        String.concat
            [ "gcd("
            , a |> List.map String.fromInt |> String.join "*"
            , ", "
            , b |> List.map String.fromInt |> String.join "*"
            , ")"
            ]
    , controller =
        { registers = Set.fromList [ "a", "b", "tmp", "is-b-zero?", "label-test" ]
        , instructions =
            [ Label "loop"
            , Perform (AssignOperation { targetRegister = "is-b-zero?", operationApplication = { name = "zero?", arguments = [ Register "b" ] } })
            , Perform (JumpToLabelIf { testRegister= "is-b-zero?", label= "done"})
            , Perform (AssignOperation { targetRegister = "tmp", operationApplication = { name = "remainder", arguments = [ Register "a", Register "b" ] } })
            , Perform (AssignRegister { targetRegister = "a", sourceRegister = "b" })
            , Perform (AssignRegister { targetRegister = "b", sourceRegister = "tmp" })
            , Perform (JumpToLabel { label = "loop" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "a", num (List.product a) ), ( "b", num (List.product b) ), ( "tmp", num 0 ), ( "is-b-zero?", num 0 ) ]
    }


controller1_remainder : ControllerExample
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
    { name = "remainder"
    , controller =
        { registers = Set.fromList [ "a", "b", "is-finished?" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "a", constant = Num 16 })
            , Perform (AssignConstant { targetRegister = "b", constant = Num 3 })
            , Label "start"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "less-than?", arguments = [ Register "a", Register "b" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})
            , Perform (AssignOperation { targetRegister = "a", operationApplication = { name = "sub", arguments = [ Register "a", Register "b" ] } })
            , Perform (JumpToLabel { label = "start" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "a", num 0 ), ( "b", num 15 ), ( "done?", num 0 ) ]
    }


controller2_fct_iterative : ControllerExample
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
    { name = "fct-iterative"
    , controller =
        { registers = Set.fromList [ "counter", "state", "done?" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "counter", constant = Num 5 })
            , Perform (AssignConstant { targetRegister = "state", constant = Num 1 })
            , Label "loop"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "zero?", arguments = [ Register "counter" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})
            , Perform (AssignOperation { targetRegister = "state", operationApplication = { name = "mul", arguments = [ Register "state", Register "counter" ] } })
            , Perform (AssignOperation { targetRegister = "counter", operationApplication = { name = "decrement", arguments = [ Register "counter" ] } })
            , Perform (JumpToLabel { label = "loop" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "counter", num 0 ), ( "state", num 0 ), ( "done?", num 0 ) ]
    }


controller3_gcd_with_inlined_remainder : ControllerExample
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
    { name = "gcd-with-inlined-remainder"
    , controller =
        { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "a", constant = Num (3 * 5 * 7) })
            , Perform (AssignConstant { targetRegister = "b", constant = Num (3 * 5 * 5) })
            , Label "gcd-loop"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "zero?", arguments = [ Register "b" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})

            -- BEGIN Calling remainder($remainder-result, $b)
            , Perform (AssignRegister { targetRegister = "remainder-result", sourceRegister = "a" })
            , Perform (JumpToLabel { label = "remainder" })
            , Label "after-remainder-done"

            -- END Calling remainder
            , Perform (AssignRegister { targetRegister = "a", sourceRegister = "b" })
            , Perform (AssignRegister { targetRegister = "b", sourceRegister = "remainder-result" })
            , Perform (JumpToLabel { label = "gcd-loop" })

            -- This is the remainder procedure
            , Label "remainder"
            , Perform (AssignOperation { targetRegister = "remainder-done?", operationApplication = { name = "less-than?", arguments = [ Register "remainder-result", Register "b" ] } })
            , Perform (JumpToLabelIf { testRegister= "remainder-done?", label= "after-remainder-done"})
            , Perform (AssignOperation { targetRegister = "remainder-result", operationApplication = { name = "sub", arguments = [ Register "remainder-result", Register "b" ] } })
            , Perform (JumpToLabel { label = "remainder" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "a", num 0 ), ( "b", num 0 ), ( "remainder-result", num 0 ), ( "done?", num 0 ), ( "remainder-done?", num 0 ) ]
    }


controller4_gcd_with_inlined_remainder_using_jump : ControllerExample
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
    { name = "gcd-with-inlined-remainder-using-jump"
    , controller =
        { registers = Set.fromList [ "a", "b", "remainder-result", "done?", "remainder-done?", "continue" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "a", constant = Num (3 * 5 * 7) })
            , Perform (AssignConstant { targetRegister = "b", constant = Num (3 * 5 * 5) })
            , Label "gcd-loop"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "zero?", arguments = [ Register "b" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})

            -- BEGIN Calling remainder($remainder-result, $b)
            , Perform (AssignRegister { targetRegister = "remainder-result", sourceRegister = "a" })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "remainder" })

            -- END Calling remainder
            , Perform (AssignRegister { targetRegister = "a", sourceRegister = "b" })
            , Perform (AssignRegister { targetRegister = "b", sourceRegister = "remainder-result" })
            , Perform (JumpToLabel { label = "gcd-loop" })

            -- This is the remainder procedure
            , Label "remainder"
            , Perform (AssignOperation { targetRegister = "remainder-done?", operationApplication = { name = "less-than?", arguments = [ Register "remainder-result", Register "b" ] } })
            , Perform (JumpToInstructionPointerAtRegisterIf { testRegister = "remainder-done?", instructionPointerRegister= "continue"})
            , Perform (AssignOperation { targetRegister = "remainder-result", operationApplication = { name = "sub", arguments = [ Register "remainder-result", Register "b" ] } })
            , Perform (JumpToLabel { label = "remainder" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "a", num 0 ), ( "b", num 0 ), ( "remainder-result", num 0 ), ( "done?", num 0 ), ( "remainder-done?", num 0 ), ( "continue", num 0 ) ]
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
    { registers = Set.fromList []
    , instructions =
        []
    }


controller6_fct_recursive : ControllerExample
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
    { name = "fct-recursive"
    , controller =
        { registers = Set.fromList [ "n", "result", "done?" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "n", constant = Num 5 })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "fct" })
            , Perform (Halt {})
            , Label "fct"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "zero?", arguments = [ Register "n" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})
            , Perform (PushRegister { sourceRegister = "continue" })
            , Perform (PushRegister { sourceRegister = "n" })
            , Perform (AssignOperation { targetRegister = "n", operationApplication = { name = "decrement", arguments = [ Register "n" ] } })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "fct" })
            , Perform (Pop { targetRegister = "n" })
            , Perform (Pop { targetRegister = "continue" })
            , Perform (AssignOperation { targetRegister = "result", operationApplication = { name = "mul", arguments = [ Register "n", Register "result" ] } })
            , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })
            , Label "done"
            , Perform (AssignConstant { targetRegister = "result", constant = Num 1 })
            , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "n", num 0 ), ( "result", num 0 ), ( "done?", num 0 ), ( "continue", num 0 ) ]
    }


controller7_fibonacci_recursive : ControllerExample
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
    { name = "fibonacci-recursive"
    , controller =
        { registers = Set.fromList [ "n", "result", "tmp", "done?", "continue" ]
        , instructions =
            [ Perform (AssignConstant { targetRegister = "n", constant = Num 8 })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "fib" })
            , Perform (Halt {})
            , Label "fib"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "less-than?", arguments = [ Register "n", Constant (Num 2) ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})

            -- call to fib(n - 1)
            , Perform (AssignOperation { targetRegister = "n", operationApplication = { name = "decrement", arguments = [ Register "n" ] } })
            , Perform (PushRegister { sourceRegister = "n" })
            , Perform (PushRegister { sourceRegister = "continue" })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "fib" })
            , Perform (Pop { targetRegister = "continue" })
            , Perform (Pop { targetRegister = "n" })

            -- call to fib(n - 2)
            , Perform (AssignOperation { targetRegister = "n", operationApplication = { name = "decrement", arguments = [ Register "n" ] } })
            , Perform (PushRegister { sourceRegister = "result" })
            , Perform (PushRegister { sourceRegister = "continue" })
            , Perform (AssignCallAtLabel { targetRegister = "continue", label = "fib" })
            , Perform (Pop { targetRegister = "continue" })
            , Perform (Pop { targetRegister = "tmp" })

            -- returning fib(n - 1) + fib(n - 2)
            , Perform (AssignOperation { targetRegister = "result", operationApplication = { name = "add", arguments = [ Register "result", Register "tmp" ] } })
            , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })

            -- base case
            , Label "done"
            , Perform (AssignRegister { targetRegister = "result", sourceRegister = "n" })
            , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "n", num 0 ), ( "result", num 0 ), ( "tmp", num 0 ), ( "done?", num 0 ), ( "continue", num 0 ) ]
    }


controller8_memory_test : ControllerExample
controller8_memory_test =
    { name = "memory-test"
    , controller =
        { registers = Set.fromList [ "p", "a", "b", "test" ]
        , instructions =
            [ Label "memory_test"
            , Perform (ConstructPair { targetRegister = "p", operationArgument0 =  (Constant (Num 16)), operationArgument1 = (Constant (Num 32)) })
            , Perform (First { targetRegister = "a", sourceRegister = "p" })
            , Perform (Second { targetRegister = "b", sourceRegister = "p" })
            , Perform (SetFirst { targetRegister = "p", operationArgument = (Constant (Num 17)) })
            , Perform (SetSecond { targetRegister = "p", operationArgument = (Constant (Num 33)) })
            , Perform (AssignOperation { targetRegister = "test", operationApplication = { name = "pair?", arguments = [ Register "p" ] } })
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "p", nil ), ( "a", num 0 ), ( "b", num 0 ), ( "test", num 0 ) ]
    }


controller9_range : ControllerExample
controller9_range =
    { name = "range"
    , controller =
        { registers = Set.fromList [ "xs", "n", "done?" ]
        , instructions =
            [ Label "start"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "zero?", arguments = [ Register "n" ] } })
            , Perform (JumpToLabelIf { testRegister= "done?", label= "done"})
            , Perform (ConstructPair { targetRegister = "xs", operationArgument0 =  (Register "n"), operationArgument1 = (Register "xs") })
            , Perform (AssignOperation { targetRegister = "n", operationApplication = { name = "decrement", arguments = [ Register "n" ] } })
            , Perform (JumpToLabel { label = "start" })
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "xs", nil ), ( "n", num 5 ), ( "done?", num 0 ) ]
    }


controller10_append : ControllerExample
controller10_append =
    -- xs <- pair 40 nil
    -- xs <- pair 30 xs
    -- xs <- pair 20 xs
    -- xs <- pair 10 xs
    -- ys <- pair 16 nil
    -- ys <- pair 32 ys
    -- ys <- pair 64 ys
    -- continue <- :done
    -- append:
    --   done? <- pair? $xs
    --   if $done? jump $continue
    --   x <- first $xs
    --   xs <- second $xs
    --   push $x
    --   push $continue
    --   continue <- :append-after-rec-call
    --   jump :append
    -- append-after-rec-call:
    --   continue <- pop-stack
    --   x <- pop-stack
    --   ys <- pair $x $ys
    --   jump $continue
    -- :done
    --   halt
    { name = "append"
    , controller =
        { registers = Set.fromList [ "xs", "ys", "x", "done?", "continue" ]
        , instructions =
            [ -- xs <- list(10, 20, 30, 40)
              Perform (ConstructPair { targetRegister = "xs", operationArgument0 =  (Constant (Num 40)), operationArgument1 = (Constant Nil) })
            , Perform (ConstructPair { targetRegister = "xs", operationArgument0 =  (Constant (Num 30)), operationArgument1 = (Register "xs") })
            , Perform (ConstructPair { targetRegister = "xs", operationArgument0 =  (Constant (Num 20)), operationArgument1 = (Register "xs") })
            , Perform (ConstructPair { targetRegister = "xs", operationArgument0 =  (Constant (Num 10)), operationArgument1 = (Register "xs") })
            , -- ys <- list(64, 32, 16)
              Perform (ConstructPair { targetRegister = "ys", operationArgument0 =  (Constant (Num 16)), operationArgument1 = (Constant Nil) })
            , Perform (ConstructPair { targetRegister = "ys", operationArgument0 =  (Constant (Num 32)), operationArgument1 = (Register "ys") })
            , Perform (ConstructPair { targetRegister = "ys", operationArgument0 =  (Constant (Num 64)), operationArgument1 = (Register "ys") })
            , Perform (AssignLabel { targetRegister = "continue", label = "done" })
            , Label "append"
            , Perform (AssignOperation { targetRegister = "done?", operationApplication = { name = "nil?", arguments = [ Register "xs" ] } })
            , Perform (JumpToInstructionPointerAtRegisterIf { testRegister = "done?", instructionPointerRegister= "continue"})
            , Perform (First { targetRegister = "x", sourceRegister = "xs" })
            , Perform (Second { targetRegister = "xs", sourceRegister = "xs" })
            , Perform (PushRegister { sourceRegister = "x" })
            , Perform (PushRegister { sourceRegister = "continue" })
            , Perform (AssignLabel { targetRegister = "continue", label = "append-after-rec-call" })
            , Perform (JumpToLabel { label = "append" })
            , Label "append-after-rec-call"
            , Perform (Pop { targetRegister = "continue" })
            , Perform (Pop { targetRegister = "x" })
            , Perform (ConstructPair { targetRegister = "ys", operationArgument0 =  (Register "x"), operationArgument1 = (Register "ys") })
            , Perform (JumpToInstructionPointerAtRegister { instructionPointerRegister = "continue" })

            ----
            , Label "done"
            , Perform (Halt {})
            ]
        }
    , initialRegisterEnvironment =
        Dict.fromList [ ( "continue", Uninitialized ), ( "xs", Uninitialized ), ( "ys", Uninitialized ), ( "x", Uninitialized ), ( "done?", Uninitialized ) ]
    }
