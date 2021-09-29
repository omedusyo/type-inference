module Value exposing
    ( Environment
    , ListValue(..)
    , ModuleEnvironment
    , NatValue(..)
    , TermEnvironment
    , Thunk(..)
    , ThunkId
    , Value(..)
    , emptyEnvironment
    , emptyModuleEnvironment
    , emptyTermEnvironment
    , extendModuleEnvironment
    , extendTermEnvironment
    , lookupModuleEnvironment
    , lookupTermEnvironment
    )

import Dict exposing (Dict)
import LambdaBasics exposing (ModuleTerm, ModuleVarName, Term, TermVarName)


type Value
    = -- ==Cartesian Product==
      PairValue Value Value
      -- ==Function Space==
    | Closure { env : Environment, var : TermVarName, body : Term }
      -- ==Coproduct==
    | LeftValue Value
    | RightValue Value
      -- Booleans
    | TrueValue
    | FalseValue
      --==Natural Number Object==
    | NatValue NatValue
    | --==Lists==
      ListValue ListValue
    | --==Frozen==
      ThunkClosure ThunkId


type NatValue
    = NatZeroValue
    | NatSuccValue NatValue


type ListValue
    = EmptyListValue
    | ConsValue Value Value


type alias ThunkId =
    Int


type Thunk
    = DelayedThunk { env : Environment, body : Term }
    | ForcedThunk Value


type alias Environment =
    { termEnv : TermEnvironment
    , moduleEnv : ModuleEnvironment
    }


emptyEnvironment : Environment
emptyEnvironment =
    { termEnv = emptyTermEnvironment
    , moduleEnv = emptyModuleEnvironment
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
    Dict ModuleVarName (List ModuleTerm)


emptyModuleEnvironment : ModuleEnvironment
emptyModuleEnvironment =
    Dict.empty


lookupModuleEnvironment0 : ModuleVarName -> ModuleEnvironment -> Maybe ModuleTerm
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


lookupModuleEnvironment : ModuleVarName -> Environment -> Maybe ModuleTerm
lookupModuleEnvironment varName env =
    lookupModuleEnvironment0 varName env.moduleEnv


extendModuleEnvironment0 : ModuleVarName -> ModuleTerm -> ModuleEnvironment -> ModuleEnvironment
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


extendModuleEnvironment : ModuleVarName -> ModuleTerm -> Environment -> Environment
extendModuleEnvironment varName module0 env =
    { env
        | moduleEnv = extendModuleEnvironment0 varName module0 env.moduleEnv
    }
