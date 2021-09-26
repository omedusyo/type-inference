module Value exposing
    ( ListValue(..)
    , ModuleEnvironment
    , NatValue(..)
    , TermEnvironment
    , Thunk(..)
    , ThunkId
    , Value(..)
    , emptyModuleEnvironment
    , emptyTermEnvironment
    , extendEnvironment
    , extendModuleEnvironment
    , lookupEnvironment
    , lookupModuleEnvironment
    )

import Dict exposing (Dict)
import LambdaBasics exposing (ModuleTerm, ModuleVarName, Term, TermVarName)


type Value
    = -- ==Cartesian Product==
      PairValue Value Value
      -- ==Function Space==
    | Closure { env : TermEnvironment, var : TermVarName, body : Term }
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
    = DelayedThunk { env : TermEnvironment, body : Term }
    | ForcedThunk Value



-- ===TERM ENVIRONMENT===


type alias TermEnvironment =
    -- We have `List Value` instead of `Value` because of shadowing of variables. The list acts as a stack.
    Dict TermVarName (List Value)


emptyTermEnvironment : TermEnvironment
emptyTermEnvironment =
    Dict.empty


lookupEnvironment : TermVarName -> TermEnvironment -> Maybe Value
lookupEnvironment varName env =
    Dict.get varName env
        |> Maybe.andThen
            (\terms ->
                case terms of
                    [] ->
                        Nothing

                    term0 :: _ ->
                        Just term0
            )


extendEnvironment : TermVarName -> Value -> TermEnvironment -> TermEnvironment
extendEnvironment varName term env =
    Dict.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just terms ->
                    Just (term :: terms)

                Nothing ->
                    Just [ term ]
        )
        env



-- ===Module Environment===
-- TODO: this is basically the same code as for TermEnvironment. Abstract away.


type alias ModuleEnvironment =
    Dict ModuleVarName (List ModuleTerm)


emptyModuleEnvironment : ModuleEnvironment
emptyModuleEnvironment =
    Dict.empty


lookupModuleEnvironment : ModuleVarName -> ModuleEnvironment -> Maybe ModuleTerm
lookupModuleEnvironment varName env =
    Dict.get varName env
        |> Maybe.andThen
            (\modules ->
                case modules of
                    [] ->
                        Nothing

                    module0 :: _ ->
                        Just module0
            )


extendModuleEnvironment : ModuleVarName -> ModuleTerm -> ModuleEnvironment -> ModuleEnvironment
extendModuleEnvironment varName module0 env =
    Dict.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just modules ->
                    Just (module0 :: modules)

                Nothing ->
                    Just [ module0 ]
        )
        env
