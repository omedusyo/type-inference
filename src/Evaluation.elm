module Evaluation exposing (..)

import Dict exposing (Dict)
import LambdaBasics exposing (..)
import StatefulReaderWithErr as State exposing (StatefulReaderWithErr)
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
-- ===Types===


type EvalError
    = UndefinedVar TermVarName
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat
    | ExpectedList
    | FailedToForceThunk ThunkId
    | ExpectedThunkClosure



-- State


type alias MutState =
    { thunkContext : ThunkContext }


type alias ReadOnlyState =
    TermEnvironment


type alias EvalStateful a =
    StatefulReaderWithErr (List EvalError) ReadOnlyState MutState a


initMutState : MutState
initMutState =
    { thunkContext = emptyThunkContext }


initReadOnlyState : ReadOnlyState
initReadOnlyState =
    emptyTermEnvironment



-- ===Thunk Context===


type alias ThunkContext =
    { nextThunkId : ThunkId
    , thunks : Dict ThunkId Thunk
    }


emptyThunkContext : ThunkContext
emptyThunkContext =
    { nextThunkId = 0, thunks = Dict.empty }


storeNewThunk : TermEnvironment -> Term -> EvalStateful ThunkId
storeNewThunk env body =
    State.create
        (\_ ({ thunkContext } as state) ->
            let
                id =
                    thunkContext.nextThunkId
            in
            Ok
                ( { state
                    | thunkContext =
                        { thunkContext
                            | nextThunkId = id + 1
                            , thunks =
                                thunkContext.thunks
                                    |> Dict.insert id (DelayedThunk { env = env, body = body })
                        }
                  }
                , id
                )
        )


forceThunk : ThunkId -> EvalStateful Value
forceThunk thunkId =
    State.get0
        (\_ ({ thunkContext } as state) ->
            let
                maybeThunk : Maybe Thunk
                maybeThunk =
                    thunkContext.thunks |> Dict.get thunkId
            in
            case maybeThunk of
                Just thunk ->
                    case thunk of
                        DelayedThunk { env, body } ->
                            State.withReadOnly (\_ _ -> env)
                                (eval body)

                        ForcedThunk val ->
                            State.return val

                Nothing ->
                    throwEvalError [ FailedToForceThunk thunkId ]
        )



-- helpers


throwEvalError : List EvalError -> EvalStateful a
throwEvalError =
    State.error


varLookup : TermVarName -> EvalStateful Value
varLookup varName =
    State.get0
        (\env _ ->
            case Value.lookupEnvironment varName env of
                Just val ->
                    State.return val

                Nothing ->
                    throwEvalError [ UndefinedVar varName ]
        )



-- ===EVALUATION===


eval0 : Term -> Result (List EvalError) Value
eval0 term =
    State.run (eval term) initReadOnlyState initMutState
        |> Result.map (\( _, value ) -> value)


eval : Term -> EvalStateful Value
eval term =
    case term of
        VarUse varName ->
            varLookup varName

        Pair fst snd ->
            State.map2 PairValue
                (eval fst)
                (eval snd)

        MatchProduct { arg, var0, var1, body } ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            PairValue val0 val1 ->
                                State.withReadOnly
                                    (\env _ ->
                                        env
                                            |> extendEnvironment var0 val0
                                            |> extendEnvironment var1 val1
                                    )
                                    (eval body)

                            _ ->
                                throwEvalError [ ExpectedPair ]
                    )

        Abstraction var body ->
            State.get0
                (\env _ ->
                    State.return (Closure { env = env, var = var, body = body })
                )

        Application fn arg ->
            eval fn
                |> State.andThen
                    (\valFn ->
                        case valFn of
                            Closure ({ var, body } as closure) ->
                                eval arg
                                    |> State.andThen
                                        (\argEvaled ->
                                            State.withReadOnly
                                                (\_ _ ->
                                                    closure.env |> extendEnvironment var argEvaled
                                                )
                                                (eval body)
                                        )

                            _ ->
                                throwEvalError [ ExpectedFunction ]
                    )

        Left term1 ->
            eval term1
                |> State.map LeftValue

        Right term1 ->
            eval term1
                |> State.map RightValue

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            -- Debug.todo ""
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            LeftValue val ->
                                State.withReadOnly (\env _ -> env |> extendEnvironment leftVar val)
                                    (eval leftBody)

                            RightValue val ->
                                State.withReadOnly (\env _ -> env |> extendEnvironment rightVar val)
                                    (eval rightBody)

                            _ ->
                                throwEvalError [ ExpectedLeftRight ]
                    )

        BoolTrue ->
            State.return TrueValue

        BoolFalse ->
            State.return FalseValue

        IfThenElse arg leftBody rightBody ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            TrueValue ->
                                eval leftBody

                            FalseValue ->
                                eval rightBody

                            _ ->
                                throwEvalError [ ExpectedBoolean ]
                    )

        NatZero ->
            State.return (NatValue NatZeroValue)

        NatSucc term1 ->
            eval term1
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            NatValue natVal ->
                                State.return (NatValue (NatSuccValue natVal))

                            _ ->
                                throwEvalError [ ExpectedNat ]
                    )

        NatLoop { base, loop, arg } ->
            -- TODO: error on same var name? loop.indexVar, loop.stateVar
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            NatValue natVal ->
                                let
                                    evalNatLoop natVal0 =
                                        case natVal0 of
                                            NatZeroValue ->
                                                eval base

                                            NatSuccValue natVal1 ->
                                                evalNatLoop natVal1
                                                    |> State.andThen
                                                        (\prevVal ->
                                                            State.withReadOnly
                                                                (\env _ ->
                                                                    env
                                                                        |> extendEnvironment loop.indexVar (NatValue natVal1)
                                                                        |> extendEnvironment loop.stateVar prevVal
                                                                )
                                                                (eval loop.body)
                                                        )
                                in
                                evalNatLoop natVal

                            _ ->
                                throwEvalError [ ExpectedNat ]
                    )

        EmptyList ->
            State.return (ListValue EmptyListValue)

        Cons headTerm tailTerm ->
            eval headTerm
                |> State.andThen
                    (\headValue ->
                        eval tailTerm
                            |> State.map
                                (\tailValue -> ListValue (ConsValue headValue tailValue))
                    )

        ListLoop { initState, loop, arg } ->
            eval arg
                |> State.andThen
                    (\argValue ->
                        case argValue of
                            ListValue listValue ->
                                let
                                    evalListLoop listValue0 =
                                        case listValue0 of
                                            EmptyListValue ->
                                                eval initState

                                            ConsValue headValue restValue ->
                                                case restValue of
                                                    ListValue listValue1 ->
                                                        evalListLoop listValue1
                                                            |> State.andThen
                                                                (\prevVal ->
                                                                    State.withReadOnly
                                                                        (\env _ ->
                                                                            env
                                                                                |> extendEnvironment loop.listElementVar headValue
                                                                                |> extendEnvironment loop.stateVar prevVal
                                                                        )
                                                                        (eval loop.body)
                                                                )

                                                    _ ->
                                                        throwEvalError [ ExpectedList ]
                                in
                                evalListLoop listValue

                            _ ->
                                throwEvalError [ ExpectedList ]
                    )

        Delay body ->
            State.get0
                (\env _ ->
                    storeNewThunk env body
                        |> State.andThen
                            (\thunkId -> State.return (ThunkClosure thunkId))
                )

        Force body ->
            eval body
                |> State.andThen
                    (\val ->
                        case val of
                            ThunkClosure thunkId ->
                                forceThunk thunkId

                            _ ->
                                throwEvalError [ ExpectedThunkClosure ]
                    )

        Let var arg body ->
            eval arg
                |> State.andThen
                    (\argVal ->
                        State.withReadOnly (\env _ -> env |> extendEnvironment var argVal)
                            (eval body)
                    )
