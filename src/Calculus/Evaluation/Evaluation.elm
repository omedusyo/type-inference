module Calculus.Evaluation.Evaluation exposing
    ( EvalError(..)
    , ThunkContext
    , eval
    , eval0
    , eval1
    , evalFunctorApplication
    , evalInModule
    , evalModule
    , evalModule0
    , evalModule1
    , evalModuleLiteral
    , lookupModuleTermField
    , openModule
    )

import Calculus.Base as Base
    exposing
        ( FunctorLiteral
        , FunctorVarName
        , ModuleLetBinding
        , ModuleLiteral
        , ModuleTerm
        , ModuleVarName
        , Term
        , TermVarName
        )
import Calculus.Evaluation.Value as Value
    exposing
        ( Environment
        , ModuleAssignment
        , ModuleValue
        , Thunk
        , ThunkId
        , Value
        )
import Dict exposing (Dict)
import Lib.State.StatefulReaderWithErr as State exposing (StatefulReaderWithErr)



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
    | UndefinedModule ModuleVarName
    | UndefinedFunctor FunctorVarName
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat
    | ExpectedList
    | FailedToForceThunk ThunkId
    | ExpectedThunkClosure
    | UnknownModuleField TermVarName
    | FunctorApplicationNumberOfModuleParametersShouldBeEqualToNumberOfArguments



-- State


type alias MutState =
    { thunkContext : ThunkContext }


type alias ReadOnlyState =
    Environment


type alias EvalStateful a =
    StatefulReaderWithErr (List EvalError) ReadOnlyState MutState a


initMutState : MutState
initMutState =
    { thunkContext = emptyThunkContext }


initReadOnlyState : ReadOnlyState
initReadOnlyState =
    Value.emptyEnvironment



-- ===Thunk Context===


type alias ThunkContext =
    { nextThunkId : ThunkId
    , thunks : Dict ThunkId Thunk
    }


emptyThunkContext : ThunkContext
emptyThunkContext =
    { nextThunkId = 0, thunks = Dict.empty }


storeNewThunk : Environment -> Term -> EvalStateful ThunkId
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
                                    |> Dict.insert id (Value.DelayedThunk { env = env, body = body })
                        }
                  }
                , id
                )
        )


forceThunk : ThunkId -> EvalStateful Value
forceThunk thunkId =
    State.get0
        (\_ { thunkContext } ->
            let
                maybeThunk : Maybe Thunk
                maybeThunk =
                    thunkContext.thunks |> Dict.get thunkId
            in
            case maybeThunk of
                Just thunk ->
                    case thunk of
                        Value.DelayedThunk { env, body } ->
                            State.withReadOnly (\_ _ -> env)
                                (eval body)
                                |> State.andThen
                                    (\thunkVal ->
                                        State.second
                                            (State.update0
                                                (\_ state ->
                                                    { state
                                                        | thunkContext =
                                                            let
                                                                thunkContext1 =
                                                                    state.thunkContext
                                                            in
                                                            { thunkContext1
                                                                | thunks =
                                                                    thunkContext1.thunks
                                                                        |> Dict.insert thunkId (Value.ForcedThunk thunkVal)
                                                            }
                                                    }
                                                )
                                            )
                                            (State.return thunkVal)
                                    )

                        Value.ForcedThunk val ->
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
            case Value.lookupTermEnvironment varName env of
                Just val ->
                    State.return val

                Nothing ->
                    throwEvalError [ UndefinedVar varName ]
        )


moduleLookup : ModuleVarName -> EvalStateful ModuleValue
moduleLookup moduleName =
    State.get0
        (\env _ ->
            case Value.lookupModuleEnvironment moduleName env of
                Just val ->
                    State.return val

                Nothing ->
                    throwEvalError [ UndefinedModule moduleName ]
        )


functorLookup : FunctorVarName -> EvalStateful FunctorLiteral
functorLookup functorName =
    State.get0
        (\env _ ->
            case Value.lookupFunctorEnvironment functorName env of
                Just val ->
                    State.return val

                Nothing ->
                    throwEvalError [ UndefinedFunctor functorName ]
        )



-- ===EVALUATION===


eval0 : Term -> Result (List EvalError) ( ThunkContext, Value )
eval0 term =
    State.run (eval term) initReadOnlyState initMutState
        |> Result.map (\( { thunkContext }, value ) -> ( thunkContext, value ))


eval1 : Environment -> Term -> Result (List EvalError) ( ThunkContext, Value )
eval1 env term =
    State.run (eval term) env initMutState
        |> Result.map (\( { thunkContext }, value ) -> ( thunkContext, value ))


eval : Term -> EvalStateful Value
eval term =
    case term of
        Base.VarUse varName ->
            varLookup varName

        Base.Pair fst snd ->
            State.map2 Value.Pair
                (eval fst)
                (eval snd)

        Base.MatchPair arg { var0, var1, body } ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            Value.Pair val0 val1 ->
                                State.withReadOnly
                                    (\env _ ->
                                        env
                                            |> Value.extendTermEnvironment var0 val0
                                            |> Value.extendTermEnvironment var1 val1
                                    )
                                    (eval body)

                            _ ->
                                throwEvalError [ ExpectedPair ]
                    )

        Base.Abstraction { var, body } ->
            State.get0
                (\env _ ->
                    -- Note that this captures both term and module environments
                    State.return (Value.Closure { env = env, var = var, body = body })
                )

        Base.Application fn arg ->
            eval fn
                |> State.andThen
                    (\valFn ->
                        case valFn of
                            Value.Closure ({ var, body } as closure) ->
                                eval arg
                                    |> State.andThen
                                        (\argEvaled ->
                                            State.withReadOnly
                                                (\_ _ ->
                                                    closure.env |> Value.extendTermEnvironment var argEvaled
                                                )
                                                (eval body)
                                        )

                            _ ->
                                throwEvalError [ ExpectedFunction ]
                    )

        Base.Left term1 ->
            eval term1
                |> State.map Value.Left

        Base.Right term1 ->
            eval term1
                |> State.map Value.Right

        Base.MatchSum arg { leftBranch, rightBranch } ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            Value.Left val ->
                                State.withReadOnly (\env _ -> env |> Value.extendTermEnvironment leftBranch.var val)
                                    (eval leftBranch.body)

                            Value.Right val ->
                                State.withReadOnly (\env _ -> env |> Value.extendTermEnvironment rightBranch.var val)
                                    (eval rightBranch.body)

                            _ ->
                                throwEvalError [ ExpectedLeftRight ]
                    )

        Base.ConstTrue ->
            State.return Value.ConstTrue

        Base.ConstFalse ->
            State.return Value.ConstFalse

        Base.MatchBool arg { trueBranch, falseBranch } ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            Value.ConstTrue ->
                                eval trueBranch.body

                            Value.ConstFalse ->
                                eval falseBranch.body

                            _ ->
                                throwEvalError [ ExpectedBoolean ]
                    )

        Base.ConstZero ->
            State.return (Value.NatValue Value.ConstZero)

        Base.Succ term1 ->
            eval term1
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            Value.NatValue natVal ->
                                State.return (Value.NatValue (Value.Succ natVal))

                            _ ->
                                throwEvalError [ ExpectedNat ]
                    )

        Base.FoldNat arg { zeroBranch, succBranch } ->
            eval arg
                |> State.andThen
                    (\argEvaled ->
                        case argEvaled of
                            Value.NatValue natVal ->
                                let
                                    evalNatLoop natVal0 =
                                        case natVal0 of
                                            Value.ConstZero ->
                                                eval zeroBranch.body

                                            Value.Succ natVal1 ->
                                                evalNatLoop natVal1
                                                    |> State.andThen
                                                        (\prevVal ->
                                                            State.withReadOnly
                                                                (\env _ ->
                                                                    env
                                                                        |> Value.extendTermEnvironment succBranch.var prevVal
                                                                )
                                                                (eval succBranch.body)
                                                        )
                                in
                                evalNatLoop natVal

                            _ ->
                                throwEvalError [ ExpectedNat ]
                    )

        Base.ConstEmpty ->
            State.return (Value.ListValue Value.ConstEmpty)

        Base.Cons headTerm tailTerm ->
            eval headTerm
                |> State.andThen
                    (\headValue ->
                        eval tailTerm
                            |> State.map
                                (\tailValue -> Value.ListValue (Value.Cons headValue tailValue))
                    )

        Base.FoldList arg { emptyBranch, consBranch } ->
            eval arg
                |> State.andThen
                    (\argValue ->
                        case argValue of
                            Value.ListValue listValue ->
                                let
                                    evalListLoop listValue0 =
                                        case listValue0 of
                                            Value.ConstEmpty ->
                                                eval emptyBranch.body

                                            Value.Cons headValue restValue ->
                                                case restValue of
                                                    Value.ListValue listValue1 ->
                                                        evalListLoop listValue1
                                                            |> State.andThen
                                                                (\prevVal ->
                                                                    State.withReadOnly
                                                                        (\env _ ->
                                                                            env
                                                                                |> Value.extendTermEnvironment consBranch.var0 headValue
                                                                                |> Value.extendTermEnvironment consBranch.var1 prevVal
                                                                        )
                                                                        (eval consBranch.body)
                                                                )

                                                    _ ->
                                                        throwEvalError [ ExpectedList ]
                                in
                                evalListLoop listValue

                            _ ->
                                throwEvalError [ ExpectedList ]
                    )

        Base.Delay { body } ->
            State.get0
                (\env _ ->
                    storeNewThunk env body
                        |> State.andThen
                            (\thunkId -> State.return (Value.ThunkClosure thunkId))
                )

        Base.Force body ->
            eval body
                |> State.andThen
                    (\val ->
                        case val of
                            Value.ThunkClosure thunkId ->
                                forceThunk thunkId

                            _ ->
                                throwEvalError [ ExpectedThunkClosure ]
                    )

        Base.LetBe arg { var, body } ->
            eval arg
                |> State.andThen
                    (\argVal ->
                        State.withReadOnly (\env _ -> env |> Value.extendTermEnvironment var argVal)
                            (eval body)
                    )

        Base.ModuleAccess module0 field ->
            evalModule module0
                |> State.andThen
                    (\moduleValue ->
                        moduleValue |> lookupModuleTermField field
                    )


lookupModuleTermField : TermVarName -> ModuleValue -> EvalStateful Value
lookupModuleTermField field moduleValue =
    let
        lookup : List ModuleAssignment -> EvalStateful Value
        lookup assignments0 =
            case assignments0 of
                [] ->
                    throwEvalError [ UnknownModuleField field ]

                assignment :: assignments1 ->
                    case assignment of
                        Value.AssignValue field0 val ->
                            if field0 == field then
                                State.return val

                            else
                                lookup assignments1

                        Value.AssignType _ _ ->
                            lookup assignments1

                        Value.AssignModuleValue _ _ ->
                            -- TODO: Should I be doing something more here?
                            lookup assignments1

                        Value.AssignFunctorLiteral _ _ ->
                            -- TODO: Should I be doing something more here?
                            lookup assignments1
    in
    lookup moduleValue.assignments


evalModule0 : ModuleTerm -> Result (List EvalError) ModuleValue
evalModule0 module0 =
    State.run (evalModule module0) initReadOnlyState initMutState
        |> Result.map (\( {}, value ) -> value)


evalModule1 : Environment -> ModuleTerm -> Result (List EvalError) ModuleValue
evalModule1 env module0 =
    State.run (evalModule module0) env initMutState
        |> Result.map (\( {}, value ) -> value)


evalModule : ModuleTerm -> EvalStateful ModuleValue
evalModule moduleTerm =
    case moduleTerm of
        Base.ModuleLiteralTerm module0 ->
            evalModuleLiteral module0

        Base.ModuleVarUse moduleName ->
            moduleLookup moduleName

        Base.FunctorApplication functorTerm modules ->
            case functorTerm of
                Base.FunctorVarUse functorName ->
                    functorLookup functorName
                        |> State.andThen
                            (\functorLiteral ->
                                evalFunctorApplication functorLiteral modules
                            )

                Base.FunctorLiteralTerm functorLiteral ->
                    evalFunctorApplication functorLiteral modules


evalFunctorApplication : FunctorLiteral -> List ModuleTerm -> EvalStateful ModuleValue
evalFunctorApplication functorLiteral modules =
    let
        -- zips two lists together of the same length together. In case the lists are not of the same length, it throws an error
        zipSame : List a -> List b -> Maybe (List ( a, b ))
        zipSame xs0 ys0 =
            case ( xs0, ys0 ) of
                ( [], [] ) ->
                    Just []

                ( x :: xs1, y :: ys1 ) ->
                    zipSame xs1 ys1 |> Maybe.map (\zs1 -> ( x, y ) :: zs1)

                _ ->
                    Nothing
    in
    -- 1. check and zip together functor literal parameters and modules
    -- 2. evalute each module argument to a module value
    -- 3. then evaluate the module term in a new functor envronment extended with the new module bindings
    case zipSame functorLiteral.parameters modules of
        Just bindingsWithInterfaces ->
            let
                moduleTermBindings : EvalStateful (List ( ModuleVarName, ModuleValue ))
                moduleTermBindings =
                    bindingsWithInterfaces
                        |> List.map
                            (\( ( moduleName, _ ), moduleTerm ) ->
                                evalModule moduleTerm
                                    |> State.map (\moduleValue -> ( moduleName, moduleValue ))
                            )
                        |> State.sequence
            in
            moduleTermBindings
                |> State.andThen
                    (\bindings ->
                        State.withReadOnly (\env _ -> env |> Value.extendModuleEnvironmentWithBindings bindings)
                            (evalModule functorLiteral.body)
                    )

        Nothing ->
            throwEvalError [ FunctorApplicationNumberOfModuleParametersShouldBeEqualToNumberOfArguments ]


evalModuleLiteral : ModuleLiteral -> EvalStateful ModuleValue
evalModuleLiteral module0 =
    let
        evalBindings : List ModuleLetBinding -> EvalStateful (List ModuleAssignment)
        evalBindings bindings0 =
            case bindings0 of
                [] ->
                    State.return []

                binding :: bindings1 ->
                    case binding of
                        Base.LetTerm varName term ->
                            eval term
                                |> State.andThen
                                    (\val ->
                                        State.withReadOnly
                                            (\env _ -> env |> Value.extendTermEnvironment varName val)
                                            (evalBindings bindings1)
                                            |> State.map
                                                (\assignments1 ->
                                                    Value.AssignValue varName val :: assignments1
                                                )
                                    )

                        Base.LetType typeVar type0 ->
                            -- TODO: what to do here? Can't do much yet. Need to make types part of the environment.
                            --       For now just skip it.
                            evalBindings bindings1

                        Base.LetModule moduleName moduleTerm ->
                            evalModule moduleTerm
                                |> State.andThen
                                    (\moduleValue ->
                                        State.withReadOnly
                                            (\env _ -> env |> Value.extendModuleEnvironment moduleName moduleValue)
                                            (evalBindings bindings1)
                                            |> State.map
                                                (\assignments1 ->
                                                    Value.AssignModuleValue moduleName moduleValue :: assignments1
                                                )
                                    )

                        Base.LetFunctor functorName functorLiteral ->
                            State.withReadOnly
                                (\env _ -> env |> Value.extendFunctorEnvironment functorName functorLiteral)
                                (evalBindings bindings1)
                                |> State.map
                                    (\assignments1 ->
                                        Value.AssignFunctorLiteral functorName functorLiteral :: assignments1
                                    )
    in
    evalBindings module0.bindings
        |> State.map (\assignments -> { assignments = assignments })



-- Runs a computation in an environment with opened module


evalInModule : ModuleValue -> EvalStateful a -> EvalStateful a
evalInModule moduleValue st =
    State.withReadOnly
        (\env _ ->
            openModule moduleValue env
        )
        st


openModule : ModuleValue -> Environment -> Environment
openModule moduleValue0 =
    let
        f : List ModuleAssignment -> Environment -> Environment
        f assignments0 env =
            case assignments0 of
                [] ->
                    env

                assignment :: assignments1 ->
                    case assignment of
                        Value.AssignValue varName value ->
                            f assignments1 (Value.extendTermEnvironment varName value env)

                        Value.AssignType typeVar type0 ->
                            f assignments1 env

                        Value.AssignModuleValue moduleName moduleValue1 ->
                            f assignments1 (Value.extendModuleEnvironment moduleName moduleValue1 env)

                        Value.AssignFunctorLiteral functorName functorLiteral ->
                            f assignments1 (Value.extendFunctorEnvironment functorName functorLiteral env)
    in
    f moduleValue0.assignments
