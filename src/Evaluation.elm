module Evaluation exposing (..)

import AssocList exposing (Dict)
import Main exposing (..)


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


type NatValue
    = NatZeroValue
    | NatSuccValue NatValue


type EvalError
    = UndefinedVar String
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat



-- ===TERM ENVIRONMENT===


type alias TermEnvironment =
    -- We have `List Value` instead of `Value` because of shadowing of variables
    Dict TermVarName (List Value)


emptyTermEnvironment =
    AssocList.empty


lookupEnvironment : TermVarName -> TermEnvironment -> Maybe Value
lookupEnvironment varName env =
    case AssocList.get varName env of
        Just terms ->
            case terms of
                [] ->
                    Nothing

                term0 :: _ ->
                    Just term0

        Nothing ->
            Nothing


extendEnvironment : TermVarName -> Value -> TermEnvironment -> TermEnvironment
extendEnvironment varName term env =
    AssocList.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just terms ->
                    Just (term :: terms)

                Nothing ->
                    Just [ term ]
        )
        env



-- ===EVALUATION===


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

        Fst term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    case evaledTerm of
                        PairValue fst _ ->
                            Ok fst

                        _ ->
                            Err [ ExpectedPair ]

                Err err ->
                    Err err

        Snd term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    case evaledTerm of
                        PairValue _ snd ->
                            Ok snd

                        _ ->
                            Err [ ExpectedPair ]

                Err err ->
                    Err err

        Abstraction var body ->
            Ok (Closure { env = env, var = var, body = body })

        Application fn arg ->
            let
                fnEvaledResult =
                    eval env fn
            in
            case fnEvaledResult of
                Ok (Closure ({ var, body } as closure)) ->
                    let
                        argEvaledResult =
                            eval env arg
                    in
                    case argEvaledResult of
                        Ok argEvaled ->
                            let
                                newEnv =
                                    extendEnvironment var argEvaled closure.env
                            in
                            eval newEnv body

                        Err err ->
                            Err err

                Ok _ ->
                    Err [ ExpectedFunction ]

                Err err ->
                    Err err

        Left term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    Ok (LeftValue evaledTerm)

                Err err ->
                    Err err

        Right term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    Ok (RightValue evaledTerm)

                Err err ->
                    Err err

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
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

                Err errs ->
                    Err errs

        BoolTrue ->
            Ok TrueValue

        BoolFalse ->
            Ok FalseValue

        IfThenElse arg leftBody rightBody ->
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        TrueValue ->
                            eval env leftBody

                        FalseValue ->
                            eval env rightBody

                        _ ->
                            Err [ ExpectedBoolean ]

                Err errs ->
                    Err errs

        NatZero ->
            Ok (NatValue NatZeroValue)

        NatSucc term1 ->
            let
                term1EvaledResult =
                    eval env term1
            in
            case term1EvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        NatValue natVal ->
                            Ok (NatValue (NatSuccValue natVal))

                        _ ->
                            Err [ ExpectedNat ]

                Err errs ->
                    Err errs

        NatLoop { base, loop, arg } ->
            -- TODO: error on same var name? loop.indexVar, loop.stateVar
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        NatValue natVal ->
                            let
                                evalNatLoop natVal0 =
                                    case natVal0 of
                                        NatZeroValue ->
                                            eval env base

                                        NatSuccValue natVal1 ->
                                            let
                                                prevResult =
                                                    evalNatLoop natVal1
                                            in
                                            case prevResult of
                                                Ok prevVal ->
                                                    let
                                                        newEnv =
                                                            env
                                                                |> extendEnvironment loop.indexVar (NatValue natVal1)
                                                                |> extendEnvironment loop.stateVar prevVal
                                                    in
                                                    eval newEnv loop.body

                                                Err errs ->
                                                    Err errs
                            in
                            evalNatLoop natVal

                        _ ->
                            Err [ ExpectedNat ]

                Err errs ->
                    Err errs
