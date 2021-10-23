module Calculus.Evaluation.Value exposing
    ( Environment
    , ListValue(..)
    , ModuleAssignment(..)
    , ModuleEnvironment
    , ModuleValue
    , NatValue(..)
    , TermEnvironment
    , Thunk(..)
    , ThunkId
    , Value(..)
    , emptyEnvironment
    , emptyModuleEnvironment
    , emptyTermEnvironment
    , extendFunctorEnvironment
    , extendModuleEnvironment
    , extendModuleEnvironmentWithBindings
    , extendTermEnvironment
    , lookupFunctorEnvironment
    , lookupModuleEnvironment
    , lookupTermEnvironment
    )

import Calculus.Base as Base exposing (FunctorLiteral, FunctorVarName, ModuleTerm, ModuleVarName, Term, TermVarName, Type, TypeVarName)
import Dict exposing (Dict)


type Value
    = -- ==Cartesian Product==
      Pair Value Value
      -- ==Function Space==
    | Closure { env : Environment, var : TermVarName, body : Term }
      -- ==Coproduct==
    | Left Value
    | Right Value
      -- Booleans
    | ConstTrue
    | ConstFalse
      --==Natural Number Object==
    | NatValue NatValue
    | --==Lists==
      ListValue ListValue
    | --==Frozen==
      ThunkClosure ThunkId


type NatValue
    = ConstZero
    | Succ NatValue


type ListValue
    = ConstEmpty
    | Cons Value Value


type alias ThunkId =
    Int


type Thunk
    = DelayedThunk { env : Environment, body : Term }
    | ForcedThunk Value


type alias ModuleValue =
    { assignments : List ModuleAssignment
    }


type ModuleAssignment
    = AssignValue TermVarName Value
    | AssignType TypeVarName Type
    | AssignModuleValue ModuleVarName ModuleValue
    | AssignFunctorLiteral FunctorVarName FunctorLiteral


type alias Environment =
    { termEnv : TermEnvironment
    , moduleEnv : ModuleEnvironment
    , functorEnv : FunctorEnvironment
    }


emptyEnvironment : Environment
emptyEnvironment =
    { termEnv = emptyTermEnvironment
    , moduleEnv = emptyModuleEnvironment
    , functorEnv = emptyFunctorEnvironment
    }



-- ===TERM ENVIRONMENT===


type alias TermEnvironment =
    -- We have `List Value` instead of `Value` because of shadowing of variables. The list acts as a stack.
    Dict TermVarName (List Value)


emptyTermEnvironment : TermEnvironment
emptyTermEnvironment =
    Dict.empty


lookupTermEnvironment0 : TermVarName -> TermEnvironment -> Maybe Value
lookupTermEnvironment0 varName env =
    Dict.get varName env
        |> Maybe.andThen
            (\terms ->
                case terms of
                    [] ->
                        Nothing

                    term0 :: _ ->
                        Just term0
            )


lookupTermEnvironment : TermVarName -> Environment -> Maybe Value
lookupTermEnvironment varName env =
    lookupTermEnvironment0 varName env.termEnv


extendTermEnvironment0 : TermVarName -> Value -> TermEnvironment -> TermEnvironment
extendTermEnvironment0 varName term env =
    Dict.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just terms ->
                    Just (term :: terms)

                Nothing ->
                    Just [ term ]
        )
        env


extendTermEnvironment : TermVarName -> Value -> Environment -> Environment
extendTermEnvironment varName term env =
    { env
        | termEnv = extendTermEnvironment0 varName term env.termEnv
    }



-- ===Module Environment===
-- TODO: this is basically the same code as for TermEnvironment. Abstract away.


type alias ModuleEnvironment =
    Dict ModuleVarName (List ModuleValue)


emptyModuleEnvironment : ModuleEnvironment
emptyModuleEnvironment =
    Dict.empty


lookupModuleEnvironment0 : ModuleVarName -> ModuleEnvironment -> Maybe ModuleValue
lookupModuleEnvironment0 varName env =
    Dict.get varName env
        |> Maybe.andThen
            (\modules ->
                case modules of
                    [] ->
                        Nothing

                    module0 :: _ ->
                        Just module0
            )


lookupModuleEnvironment : ModuleVarName -> Environment -> Maybe ModuleValue
lookupModuleEnvironment varName env =
    lookupModuleEnvironment0 varName env.moduleEnv


extendModuleEnvironment0 : ModuleVarName -> ModuleValue -> ModuleEnvironment -> ModuleEnvironment
extendModuleEnvironment0 varName module0 env =
    Dict.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just modules ->
                    Just (module0 :: modules)

                Nothing ->
                    Just [ module0 ]
        )
        env


extendModuleEnvironment : ModuleVarName -> ModuleValue -> Environment -> Environment
extendModuleEnvironment varName module0 env =
    { env
        | moduleEnv = extendModuleEnvironment0 varName module0 env.moduleEnv
    }


extendModuleEnvironmentWithBindings : List ( ModuleVarName, ModuleValue ) -> Environment -> Environment
extendModuleEnvironmentWithBindings bindings env0 =
    List.foldl
        (\( moduleName, moduleValue ) env -> extendModuleEnvironment moduleName moduleValue env)
        env0
        bindings



-- ===Functor Environment===


type alias FunctorEnvironment =
    Dict FunctorVarName (List FunctorLiteral)


emptyFunctorEnvironment : FunctorEnvironment
emptyFunctorEnvironment =
    Dict.empty


lookupFunctorEnvironment0 : FunctorVarName -> FunctorEnvironment -> Maybe FunctorLiteral
lookupFunctorEnvironment0 functorName env =
    Dict.get functorName env
        |> Maybe.andThen
            (\functors ->
                case functors of
                    [] ->
                        Nothing

                    functor :: _ ->
                        Just functor
            )


lookupFunctorEnvironment : FunctorVarName -> Environment -> Maybe FunctorLiteral
lookupFunctorEnvironment functorName env =
    lookupFunctorEnvironment0 functorName env.functorEnv


extendFunctorEnvironment0 : FunctorVarName -> FunctorLiteral -> FunctorEnvironment -> FunctorEnvironment
extendFunctorEnvironment0 functorName functor env =
    Dict.update functorName
        (\maybeBinding ->
            case maybeBinding of
                Just functors ->
                    Just (functor :: functors)

                Nothing ->
                    Just [ functor ]
        )
        env


extendFunctorEnvironment : FunctorVarName -> FunctorLiteral -> Environment -> Environment
extendFunctorEnvironment functorName functor env =
    { env
        | functorEnv = extendFunctorEnvironment0 functorName functor env.functorEnv
    }
