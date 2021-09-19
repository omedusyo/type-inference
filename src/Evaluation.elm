module Evaluation exposing (..)

import LambdaBasics exposing (..)
import Reader
import StatefulReaderWithErr
import Value exposing (..)



-- Evaluation of Bindings Operators
--   (fn { x . body }) ~> capture current env into a closure. The env together with body expression is the resulting value.
--
--   (product-match e { (product x y) . body })
--   (product-match (product e1 e2) { (product x y) . body }) ~> body in environment x := value of e1, y := value of e2
--
--
--   (if e { e1 } { e2 })
--     (if true  { e1 } { e2 }) ~> e1
--     (if false { e1 } { e2 }) ~> e2
--
--
--
--   (sum-case e { (left x) . e1 } { (right y) . e2 }) ~>
--     (sum-case (left  e) { (left x) . e1 } { (right y) . e2 }) ~> e1 in environment x := value of e
--     (sum-case (right e) { (left x) . e1 } { (right y) . e2 }) ~> e2 in environment y := value of e
--
--   (nat-loop   n initState { i s . body })
--     (nat-loop   0 initState { i s . body }) ~> initState
--     (nat-loop   3 initState { i s . body }) ~>
--            state0 := initState
--            state1 := body in env i := 0, s := state0
--            state2 := body in env i := 1, s := state1
--            state3 := body in env i := 2, s := state2
--            state3
--
--
--   (list-loop xs initState { x s . body })
--     (list-loop empty-list initState { x s . body }) ~> initState
--     (list-loop (cons x0 (cons x1 (cons x2 empty-listA))) initState { x s . body }) ~>
--            state0 := initState
--            state1 := body in env x := x2, s := state0
--            state2 := body in env x := x1, s := state1
--            state3 := body in env x := x0, s := state2
--            state3


type EvalError
    = UndefinedVar String
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat
    | ExpectedList



-- State
-- ===EVALUATION===


eval0 : Term -> Result (List EvalError) Value
eval0 term =
    eval emptyTermEnvironment term


eval : TermEnvironment -> Term -> Result (List EvalError) Value
eval env term =
    case term of
        VarUse varName ->
            case lookupEnvironment varName env of
                Just result ->
                    Ok result

                Nothing ->
                    Err [ UndefinedVar varName ]

        Pair fst snd ->
            let
                evaledFstResult =
                    eval env fst

                evaledSndResult =
                    eval env snd
            in
            -- TODO: you should evaluate the first component, and if the first component is error, then short circuit and don't eval second component
            -- TODO: I guess there are multiple possibilities...?
            case ( evaledFstResult, evaledSndResult ) of
                ( Ok evaledFst, Ok evaledSnd ) ->
                    Ok (PairValue evaledFst evaledSnd)

                ( Err errFst, Ok evaledSnd ) ->
                    Err errFst

                ( Ok evaledFst, Err errSnd ) ->
                    Err errSnd

                ( Err errFst, Err errSnd ) ->
                    Err (errFst ++ errSnd)

        MatchProduct { arg, var0, var1, body } ->
            eval env arg
                |> Result.andThen
                    (\argEvaled ->
                        case argEvaled of
                            PairValue val0 val1 ->
                                let
                                    newEnv =
                                        env
                                            |> extendEnvironment var0 val0
                                            |> extendEnvironment var1 val1
                                in
                                eval newEnv body

                            _ ->
                                Err [ ExpectedPair ]
                    )

        Abstraction var body ->
            Ok (Closure { env = env, var = var, body = body })

        Application fn arg ->
            case eval env fn of
                Ok (Closure ({ var, body } as closure)) ->
                    eval env arg
                        |> Result.andThen
                            (\argEvaled ->
                                let
                                    newEnv =
                                        extendEnvironment var argEvaled closure.env
                                in
                                eval newEnv body
                            )

                Ok _ ->
                    Err [ ExpectedFunction ]

                Err err ->
                    Err err

        Left term1 ->
            eval env term1
                |> Result.map LeftValue

        Right term1 ->
            eval env term1
                |> Result.map RightValue

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            eval env arg
                |> Result.andThen
                    (\argEvaled ->
                        case argEvaled of
                            LeftValue val ->
                                let
                                    newEnv =
                                        extendEnvironment leftVar val env
                                in
                                eval newEnv leftBody

                            RightValue val ->
                                let
                                    newEnv =
                                        extendEnvironment rightVar val env
                                in
                                eval newEnv rightBody

                            _ ->
                                Err [ ExpectedLeftRight ]
                    )

        BoolTrue ->
            Ok TrueValue

        BoolFalse ->
            Ok FalseValue

        IfThenElse arg leftBody rightBody ->
            eval env arg
                |> Result.andThen
                    (\argEvaled ->
                        case argEvaled of
                            TrueValue ->
                                eval env leftBody

                            FalseValue ->
                                eval env rightBody

                            _ ->
                                Err [ ExpectedBoolean ]
                    )

        NatZero ->
            Ok (NatValue NatZeroValue)

        NatSucc term1 ->
            eval env term1
                |> Result.andThen
                    (\argEvaled ->
                        case argEvaled of
                            NatValue natVal ->
                                Ok (NatValue (NatSuccValue natVal))

                            _ ->
                                Err [ ExpectedNat ]
                    )

        NatLoop { base, loop, arg } ->
            -- TODO: error on same var name? loop.indexVar, loop.stateVar
            eval env arg
                |> Result.andThen
                    (\argEvaled ->
                        case argEvaled of
                            NatValue natVal ->
                                let
                                    evalNatLoop natVal0 =
                                        case natVal0 of
                                            NatZeroValue ->
                                                eval env base

                                            NatSuccValue natVal1 ->
                                                evalNatLoop natVal1
                                                    |> Result.andThen
                                                        (\prevVal ->
                                                            let
                                                                newEnv =
                                                                    env
                                                                        |> extendEnvironment loop.indexVar (NatValue natVal1)
                                                                        |> extendEnvironment loop.stateVar prevVal
                                                            in
                                                            eval newEnv loop.body
                                                        )
                                in
                                evalNatLoop natVal

                            _ ->
                                Err [ ExpectedNat ]
                    )

        EmptyList ->
            Ok (ListValue EmptyListValue)

        Cons headTerm tailTerm ->
            eval env headTerm
                |> Result.andThen
                    (\headValue ->
                        eval env tailTerm
                            |> Result.andThen
                                (\tailValue ->
                                    Ok (ListValue (ConsValue headValue tailValue))
                                )
                    )

        ListLoop { initState, loop, arg } ->
            eval env arg
                |> Result.andThen
                    (\argValue ->
                        case argValue of
                            ListValue listValue ->
                                let
                                    evalListLoop listValue0 =
                                        case listValue0 of
                                            EmptyListValue ->
                                                eval env initState

                                            ConsValue headValue restValue ->
                                                case restValue of
                                                    ListValue listValue1 ->
                                                        let
                                                            prevResult =
                                                                evalListLoop listValue1
                                                        in
                                                        prevResult
                                                            |> Result.andThen
                                                                (\prevVal ->
                                                                    let
                                                                        newEnv =
                                                                            env
                                                                                |> extendEnvironment loop.listElementVar headValue
                                                                                |> extendEnvironment loop.stateVar prevVal
                                                                    in
                                                                    eval newEnv loop.body
                                                                )

                                                    _ ->
                                                        Err [ ExpectedList ]
                                in
                                evalListLoop listValue

                            _ ->
                                Err [ ExpectedList ]
                    )

        Delay body ->
            Ok (Thunk { env = env, body = body })

        Force body ->
            -- TODO
            Debug.todo ""

        Let var arg body ->
            eval env arg
                |> Result.andThen
                    (\argVal ->
                        let
                            newEnv =
                                env
                                    |> extendEnvironment var argVal
                        in
                        eval newEnv body
                    )
