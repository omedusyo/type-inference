module RegisterMachine.OperationEnvironment exposing (env)

import Dict
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), Value(..))
import RegisterMachine.Machine as RegisterMachine exposing (CompilationError(..), ComputationStep(..), RuntimeError(..))


env : RegisterMachine.OperationEnvironment
env =
    let
        boolToInt : Bool -> Int
        boolToInt b =
            if b then
                1

            else
                0
    in
    Dict.fromList
        [ ( "sub", RegisterMachine.makeNumOperation2 (\x y -> x - y) )
        , ( "less-than?", RegisterMachine.makeNumOperation2 (\x y -> boolToInt (x < y)) )
        , ( "add", RegisterMachine.makeNumOperation2 (\x y -> x + y) )
        , ( "mul", RegisterMachine.makeNumOperation2 (\x y -> x * y) )
        , ( "zero?", RegisterMachine.makeNumOperation1 (\x -> boolToInt (x == 0)) )
        , ( "eq?", RegisterMachine.makeNumOperation2 (\x y -> boolToInt (x == y)) )
        , ( "not", RegisterMachine.makeNumOperation1 (\x -> boolToInt (x == 0)) )
        , ( "decrement", RegisterMachine.makeNumOperation1 (\x -> x - 1) )
        , ( "increment", RegisterMachine.makeNumOperation1 (\x -> x + 1) )
        , ( "remainder", RegisterMachine.makeNumOperation2 (\x y -> remainderBy y x) )
        , ( "pair?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        Pair _ ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "nil?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        ConstantValue Nil ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "num?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        ConstantValue (Num _) ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        , ( "moved?"
          , RegisterMachine.makeOperation1
                (\val ->
                    case val of
                        Moved ->
                            Ok (ConstantValue (Num 1))

                        _ ->
                            Ok (ConstantValue (Num 0))
                )
          )
        ]
