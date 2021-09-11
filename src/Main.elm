module Main exposing (..)

import AssocList exposing (Dict)
import Set exposing (Set)
import StatefulWithErr as State exposing (StatefulWithErr)


type alias TermVarName =
    -- TODO: We assume that variables range over lambda values, so maybe it would be better to call this ValueVarName?
    String


type Term
    = -- ==Variables==
      VarUse TermVarName
      -- ==Cartesian Product==
      -- intro
    | Pair Term Term
      -- elim
    | Fst Term
    | Snd Term
      -- ==Function Space==
      -- intro
    | Abstraction TermVarName Term
      -- elim
      -- first arg is FunctionExpression, the second is the ArgumentExpression
    | Application Term Term
      -- ==Coproduct==
      -- intro
    | Left Term
    | Right Term
      -- elim
    | Case
        { arg : Term
        , leftVar : TermVarName
        , leftBody : Term
        , rightVar : TermVarName
        , rightBody : Term
        }
      -- Booleans
    | BoolTrue
    | BoolFalse
      -- first is the TestExpression then LeftBranch then RightBranch
    | IfThenElse Term Term Term
      --==Natural Number Object==
      -- intro
    | NatZero
    | NatSucc Term
      -- elim
      -- f : Nat -> X
      -- f 0 = ....
      -- f (n + 1) = you can use `f n` here
    | NatLoop
        { base : Term
        , loop :
            { indexVar : TermVarName
            , stateVar : TermVarName -- <- this should be interpreted as `f n`
            , body : Term
            }
        , arg : Term
        }


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


type alias TypeVarName =
    Int


type Type
    = VarType TypeVarName
    | Product Type Type
    | Sum Type Type
    | Arrow Type Type
    | LambdaBool
    | LambdaNat


type alias TermEnvironment =
    -- We have `List Value` instead of `Value` because of shadowing of variables
    Dict TermVarName (List Value)


emptyEnvironment =
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


type EvalError
    = UndefinedVar String
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat


type TypeError
    = ExpectedProductType
    | ExpectedArrowType
    | ExpectedNatType
    | ExpectedSumType
    | ExpectedMatchingTypesInCaseBranches
    | ExpectedBoolType
    | ExpectedMatchingTypesInIfThenElseBranches
    | ExpectedBaseUnifiesWithLoopBodyType
    | InfiniteType Int


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


showTerm : Term -> String
showTerm term =
    case term of
        VarUse varname ->
            String.concat [ "$", varname ]

        Pair fst snd ->
            String.concat [ "(", showTerm fst, ", ", showTerm snd, ")" ]

        Fst term1 ->
            String.concat [ showTerm term1, ".0" ]

        Snd term1 ->
            String.concat [ showTerm term1, ".1" ]

        Abstraction var body ->
            String.concat [ "lam(", var, " -> ", showTerm body, ")" ]

        Application fn arg ->
            String.concat [ "[", showTerm fn, " ", showTerm arg, "]" ]

        Left term1 ->
            String.concat [ "L(", showTerm term1, ")" ]

        Right term1 ->
            String.concat [ "R(", showTerm term1, ")" ]

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            String.concat
                [ "case("
                , showTerm arg
                , ", Left "
                , leftVar
                , " -> "
                , showTerm leftBody
                , ", Right "
                , rightVar
                , " -> "
                , showTerm rightBody
                , ")"
                ]

        BoolTrue ->
            "True"

        BoolFalse ->
            "False"

        IfThenElse arg leftBody rightBody ->
            String.concat
                [ "if("
                , showTerm arg
                , " then "
                , showTerm leftBody
                , " else "
                , showTerm rightBody
                , ")"
                ]

        NatZero ->
            "Z"

        NatSucc term1 ->
            String.concat [ "S(", showTerm term1, ")" ]

        NatLoop { base, loop, arg } ->
            String.concat
                [ "loop("
                , loop.stateVar
                , " := "
                , showTerm base
                , ", for "
                , loop.indexVar
                , " in range("
                , showTerm arg
                , ") do "
                , loop.stateVar
                , " := "
                , showTerm loop.body
                , ")"
                ]


showType : Type -> String
showType type0 =
    case type0 of
        VarType n ->
            String.concat [ "'", String.fromInt n ]

        Product type1 type2 ->
            String.concat
                [ "(", showType type1, " , ", showType type2, ")" ]

        Sum type1 type2 ->
            String.concat
                [ "[", showType type1, " + ", showType type2, "]" ]

        Arrow type1 type2 ->
            String.concat
                [ "(", showType type1, " -> ", showType type2, ")" ]

        LambdaBool ->
            "Bool"

        LambdaNat ->
            "Nat"


newTypeVar : TypeVarName -> ( TypeVarName, Type )
newTypeVar n =
    ( n + 1, VarType n )



-- The list serves as a stack of types
-- - any element in the stack shadows everything below it.


type alias Context =
    Dict TermVarName (List Type)


emptyContext : Context
emptyContext =
    AssocList.empty


popVarFromContext : String -> Context -> Context
popVarFromContext varName context0 =
    AssocList.update varName
        (Maybe.andThen List.tail)
        context0


lookupType : TermVarName -> Context -> Maybe Type
lookupType varName context0 =
    AssocList.get varName context0
        |> Maybe.andThen List.head


pushVarToContext : TermVarName -> Type -> Context -> Context
pushVarToContext varName type0 context0 =
    AssocList.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just types0 ->
                    Just (type0 :: types0)

                Nothing ->
                    Just [ type0 ]
        )
        context0



-- ===EQUATIONS===


type alias Equations =
    Dict TypeVarName Type


emptyEquations : Equations
emptyEquations =
    AssocList.empty


lookupEquations : TypeVarName -> Equations -> Maybe Type
lookupEquations =
    AssocList.get


extendEquations : TypeVarName -> Type -> Equations -> Equations
extendEquations varname type0 eqs =
    AssocList.insert varname type0 eqs



-- ===EXPANSION===


expandType_MAY_INFINITE_CYCLE : Type -> Equations -> Type
expandType_MAY_INFINITE_CYCLE type0 eqs0 =
    case type0 of
        VarType n ->
            let
                maybeType1 =
                    lookupEquations n eqs0
            in
            case maybeType1 of
                Just type1 ->
                    expandType_MAY_INFINITE_CYCLE type1 eqs0

                Nothing ->
                    VarType n

        Product type1 type2 ->
            Product (expandType_MAY_INFINITE_CYCLE type1 eqs0) (expandType_MAY_INFINITE_CYCLE type2 eqs0)

        Sum type1 type2 ->
            Sum (expandType_MAY_INFINITE_CYCLE type1 eqs0) (expandType_MAY_INFINITE_CYCLE type2 eqs0)

        Arrow type1 type2 ->
            Arrow (expandType_MAY_INFINITE_CYCLE type1 eqs0) (expandType_MAY_INFINITE_CYCLE type2 eqs0)

        LambdaBool ->
            LambdaBool

        LambdaNat ->
            LambdaNat



-- This expansion can't loop. It will detect infinite types.


expandType : Type -> Equations -> Result (List TypeError) Type
expandType type0 eqs0 =
    expandTypeWithCycleDetection type0 Set.empty eqs0


expandTypeWithCycleDetection : Type -> Set TypeVarName -> Equations -> Result (List TypeError) Type
expandTypeWithCycleDetection type0 seenVars eqs0 =
    case type0 of
        VarType n ->
            if Set.member n seenVars then
                Err [ InfiniteType n ]

            else
                case lookupEquations n eqs0 of
                    Just type1 ->
                        expandTypeWithCycleDetection type1 (Set.insert n seenVars) eqs0

                    Nothing ->
                        Ok (VarType n)

        Product type1 type2 ->
            Result.map2 Product
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        Sum type1 type2 ->
            Result.map2 Sum
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        Arrow type1 type2 ->
            Result.map2 Arrow
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        LambdaBool ->
            Ok LambdaBool

        LambdaNat ->
            Ok LambdaNat



-- ===UNIFICATION===


unification : Type -> Type -> Equations -> Result (List TypeError) ( Equations, Type )
unification type0Unexpanded type1Unexpanded eqs0 =
    Result.map2
        Tuple.pair
        (expandType type0Unexpanded eqs0)
        (expandType type1Unexpanded eqs0)
        |> Result.andThen
            (\( type0, type1 ) ->
                case ( type0, type1 ) of
                    -- ===TYPE VARS===
                    ( VarType id0, VarType id1 ) ->
                        if id0 == id1 then
                            Ok ( eqs0, VarType id0 )

                        else if id0 < id1 then
                            Ok ( eqs0 |> extendEquations id0 (VarType id1), VarType id1 )

                        else
                            Ok ( eqs0 |> extendEquations id1 (VarType id0), VarType id1 )

                    ( VarType id0, _ ) ->
                        Ok ( eqs0 |> extendEquations id0 type1, type1 )

                    ( _, VarType id1 ) ->
                        Ok ( eqs0 |> extendEquations id1 type0, type0 )

                    -- ===PRODUCT===
                    ( Product type00 type01, Product type10 type11 ) ->
                        let
                            maybeEqs1 =
                                unification type00 type10 eqs0
                        in
                        maybeEqs1
                            |> Result.andThen
                                (\( eqs1, typeFst ) ->
                                    let
                                        maybeEqs2 =
                                            unification type01 type11 eqs1
                                    in
                                    maybeEqs2
                                        |> Result.map
                                            (\( eqs2, typeSnd ) ->
                                                ( eqs2, Product typeFst typeSnd )
                                            )
                                )

                    ( Product _ _, _ ) ->
                        Err [ ExpectedProductType ]

                    -- ===ARROW===
                    ( Arrow type00 type01, Arrow type10 type11 ) ->
                        let
                            maybeEqs1 =
                                unification type00 type10 eqs0
                        in
                        maybeEqs1
                            |> Result.andThen
                                (\( eqs1, typeFst ) ->
                                    let
                                        maybeEqs2 =
                                            unification type01 type11 eqs1
                                    in
                                    maybeEqs2
                                        |> Result.map
                                            (\( eqs2, typeSnd ) ->
                                                ( eqs2, Arrow typeFst typeSnd )
                                            )
                                )

                    ( Arrow _ _, _ ) ->
                        Err [ ExpectedArrowType ]

                    -- ===SUM===
                    ( Sum type00 type01, Sum type10 type11 ) ->
                        let
                            maybeEqs1 =
                                unification type00 type10 eqs0
                        in
                        maybeEqs1
                            |> Result.andThen
                                (\( eqs1, typeFst ) ->
                                    let
                                        maybeEqs2 =
                                            unification type01 type11 eqs1
                                    in
                                    maybeEqs2
                                        |> Result.map
                                            (\( eqs2, typeSnd ) ->
                                                ( eqs2, Sum typeFst typeSnd )
                                            )
                                )

                    ( Sum _ _, _ ) ->
                        Err [ ExpectedSumType ]

                    -- ===BOOL===
                    ( LambdaBool, LambdaBool ) ->
                        Ok ( eqs0, LambdaBool )

                    ( LambdaBool, _ ) ->
                        Err [ ExpectedBoolType ]

                    -- ===NAT===
                    ( LambdaNat, LambdaNat ) ->
                        Ok ( eqs0, LambdaNat )

                    ( LambdaNat, _ ) ->
                        Err [ ExpectedNatType ]
            )



-- ===STATEFUL MONAD INFERENCE===


type alias State =
    { nextTypeVar : TypeVarName
    , context : Context
    , equations : Equations
    }


emptyState : State
emptyState =
    { nextTypeVar = 0
    , context = emptyContext
    , equations = emptyEquations
    }


type alias InferenceContext a =
    StatefulWithErr (List TypeError) State a


generateFreshVar : InferenceContext Type
generateFreshVar =
    \({ nextTypeVar } as state0) ->
        let
            ( nextTypeVar1, type1 ) =
                newTypeVar nextTypeVar
        in
        Ok ( { state0 | nextTypeVar = nextTypeVar1 }, type1 )


getContext : (Context -> InferenceContext a) -> InferenceContext a
getContext f =
    State.get0 (\{ context } -> f context)


getEquations : (Equations -> InferenceContext a) -> InferenceContext a
getEquations f =
    State.get0 (\{ equations } -> f equations)


updateContext : (Context -> Context) -> InferenceContext a -> InferenceContext a
updateContext nextContext =
    State.update (\({ context } as state) -> { state | context = nextContext context })


updateContext0 : (Context -> Context) -> InferenceContext ()
updateContext0 nextContext =
    State.update0 (\({ context } as state) -> { state | context = nextContext context })


updateEquations : (Equations -> Equations) -> InferenceContext a -> InferenceContext a
updateEquations nextEquations =
    State.update (\({ equations } as state) -> { state | equations = nextEquations equations })


setEquations : Equations -> InferenceContext a -> InferenceContext a
setEquations equations0 =
    updateEquations (\_ -> equations0)


throwTypeError : List TypeError -> InferenceContext a
throwTypeError =
    State.error


unify : Type -> Type -> InferenceContext Type
unify type0 type1 =
    getEquations
        (\equations0 ->
            case unification type0 type1 equations0 of
                Ok ( equations1, intersectionType ) ->
                    State.return intersectionType
                        |> setEquations equations1

                Err errs ->
                    throwTypeError errs
        )



-- ===INFERENCE===


infer : Term -> InferenceContext Type
infer term =
    case term of
        VarUse varName ->
            getContext
                (\context0 ->
                    case context0 |> lookupType varName of
                        Just typeVar ->
                            State.return typeVar

                        Nothing ->
                            -- typeVar := generateFreshVar
                            -- context := context |> pushVarToContext varName typeVar
                            -- return typeVar
                            generateFreshVar
                                |> State.andThen
                                    (\typeVar ->
                                        State.return typeVar
                                            |> updateContext
                                                (\context1 -> context1 |> pushVarToContext varName typeVar)
                                    )
                )

        Pair fst snd ->
            -- typeFst := infer fst
            -- typeSnd := infer snd
            -- return (Product typeFst typeSnd)
            State.map2
                (\typeFst typeSnd -> Product typeFst typeSnd)
                (infer fst)
                (infer snd)

        Fst productExp ->
            -- typeProduct0 := infer productExp;
            -- fstTypeVar0  := generateFreshVar;
            -- sndTypeVar0  := generateFreshVar;
            -- typeProduct2 := unify typeProduct0 (Product fstTypeVar0 sndTypeVar0)
            -- case typeProduct1 of
            --     Product fstTypeVar1 _ ->
            --         return fstTypeVar1
            --     _ ->
            --         err ExpectedProductType
            State.andThen3
                (\typeProduct0 fstTypeVar0 sndTypeVar0 ->
                    unify typeProduct0 (Product fstTypeVar0 sndTypeVar0)
                        |> State.andThen
                            (\typeProduct2 ->
                                case typeProduct2 of
                                    Product fstTypeVar1 _ ->
                                        State.return fstTypeVar1

                                    _ ->
                                        throwTypeError [ ExpectedProductType ]
                            )
                )
                (infer productExp)
                generateFreshVar
                generateFreshVar

        Snd productExp ->
            State.andThen3
                (\typeProduct0 fstTypeVar0 sndTypeVar0 ->
                    unify typeProduct0 (Product fstTypeVar0 sndTypeVar0)
                        |> State.andThen
                            (\typeProduct2 ->
                                case typeProduct2 of
                                    Product _ sndTypeVar2 ->
                                        State.return sndTypeVar2

                                    _ ->
                                        throwTypeError [ ExpectedProductType ]
                            )
                )
                (infer productExp)
                generateFreshVar
                generateFreshVar

        Abstraction var body ->
            -- typeVar := generateFreshVar;
            -- updateContext (\context -> context |> pushVarToContext var typeVar);
            -- typeBody := infer body;
            -- updateContext (\context -> context |> popVarFromContext var);
            -- return (Arrow typeVar typeBody)
            generateFreshVar
                |> State.andThen
                    (\typeVar ->
                        State.mid
                            (updateContext0 (\context -> context |> pushVarToContext var typeVar))
                            (infer body)
                            (updateContext0 (\context -> context |> popVarFromContext var))
                            |> State.map
                                (\typeBody ->
                                    Arrow typeVar typeBody
                                )
                    )

        Application fn arg ->
            -- typeFn0 := infer fn ;
            -- typeArg := infer arg ;
            -- resultTypeVar := generateFreshVar;
            -- typeFn1 := unify typeFn0 (Arrow typeArg resultTypeVar);
            -- case typeFn1 of
            --     Arrow _ resultType ->
            --         return resultTypeVar
            --     _ ->
            --         throwTypeError [ ExpectedArrowType ]
            State.andThen3
                (\typeFn0 typeArg resultType0 ->
                    unify typeFn0 (Arrow typeArg resultType0)
                        |> State.andThen
                            (\typeFn1 ->
                                case typeFn1 of
                                    Arrow _ resultType1 ->
                                        State.return resultType1

                                    _ ->
                                        throwTypeError [ ExpectedArrowType ]
                            )
                )
                (infer fn)
                (infer arg)
                generateFreshVar

        Left leftTerm ->
            -- typeLeftTerm := infer leftTerm;
            -- typeRightTerm := generateFreshVar;
            -- return (Sum typeLeftTerm typeRightTerm);
            State.map2 Sum
                (infer leftTerm)
                generateFreshVar

        Right rightTerm ->
            State.map2 Sum
                generateFreshVar
                (infer rightTerm)

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            -- typeArg := infer arg;
            -- leftTypeVar := generateFreshVar;
            -- rightTypeVar := generateFreshVar;
            -- sumType := unify (Sum leftTypeVar rightTypeVar) typeArg;
            -- case sumType of
            --     Sum leftType rightType ->
            --         updateContext (\context -> context |> pushVarToContext leftVar leftType);
            --         typeLeftBody := infer leftBody;
            --         updateContext (\context -> context |> popVarFromContext leftVar);
            --
            --         updateContext (\context -> context |> pushVarToContext rightVar rightType);
            --         typeRightBody := infer rightBody;
            --         updateContext (\context -> context |> popVarFromContext rightVar);
            --
            --         unify typeLeftBody typeRightBody
            --     _ ->
            --         throwTypeError [ ExpectedSumType ]
            State.andThen3
                (\typeArg leftTypeVar rightTypeVar ->
                    unify (Sum leftTypeVar rightTypeVar) typeArg
                        |> State.andThen
                            (\sumType ->
                                case sumType of
                                    Sum leftType rightType ->
                                        State.andThen2
                                            (\typeLeftBody typeRightBody ->
                                                unify typeLeftBody typeRightBody
                                            )
                                            (State.mid
                                                (updateContext0 (\context -> context |> pushVarToContext leftVar leftType))
                                                (infer leftBody)
                                                (updateContext0 (\context -> context |> popVarFromContext leftVar))
                                            )
                                            (State.mid
                                                (updateContext0 (\context -> context |> pushVarToContext rightVar rightType))
                                                (infer rightBody)
                                                (updateContext0 (\context -> context |> popVarFromContext rightVar))
                                            )

                                    _ ->
                                        throwTypeError [ ExpectedSumType ]
                            )
                )
                (infer arg)
                generateFreshVar
                generateFreshVar

        BoolTrue ->
            State.return LambdaBool

        BoolFalse ->
            State.return LambdaBool

        IfThenElse arg leftBody rightBody ->
            -- argType := infer arg;
            -- unify argType LambdaBool;
            -- typeLeftBody := infer leftBody;
            -- typeRightBody := infer rightBody;
            -- unify typeLeftBody typeRightBody;
            State.second
                (infer arg
                    |> State.andThen
                        (\argType ->
                            unify argType LambdaBool
                        )
                )
                (State.andThen2 unify
                    (infer leftBody)
                    (infer rightBody)
                )

        NatZero ->
            State.return LambdaNat

        NatSucc term1 ->
            -- type1 infer term1;
            -- unify type1 LambdaNat;
            infer term1
                |> State.andThen
                    (\type1 ->
                        unify type1 LambdaNat
                    )

        NatLoop { base, loop, arg } ->
            -- argType := infer arg;
            -- unify argType LambdaNat
            --
            -- baseType := infer base;
            --
            -- updateContext
            --   (\context ->
            --       context
            --           |> pushVarToContext loop.indexVar LambdaNat
            --           |> pushVarToContext loop.stateVar baseType
            --   );
            -- loopBodyType := infer loop.body;
            -- updateContext
            --   (\context ->
            --       context
            --           |> popVarFromContext loop.stateVar
            --           |> popVarFromContext loop.indexVar
            --   );
            --
            -- unify loopBodyType baseType;
            let
                argInference =
                    infer arg
                        |> State.andThen
                            (\argType ->
                                unify argType LambdaNat
                            )
            in
            State.second
                argInference
                (infer base
                    |> State.andThen
                        (\baseType ->
                            let
                                loopBodyInference =
                                    State.mid
                                        (updateContext0
                                            (\context ->
                                                context
                                                    |> pushVarToContext loop.indexVar LambdaNat
                                                    |> pushVarToContext loop.stateVar baseType
                                            )
                                        )
                                        (infer loop.body)
                                        (updateContext0
                                            (\context ->
                                                context
                                                    |> popVarFromContext loop.stateVar
                                                    |> popVarFromContext loop.indexVar
                                            )
                                        )
                            in
                            loopBodyInference
                                |> State.andThen
                                    (\loopBodyType ->
                                        unify loopBodyType baseType
                                    )
                        )
                )


infer0 : Term -> Result (List TypeError) ( Context, Equations, Type )
infer0 term =
    State.run (infer term)
        emptyState
        |> Result.map
            (\( state, type0 ) ->
                ( state.context, state.equations, type0 )
            )


showInfer0 : Term -> Result (List TypeError) String
showInfer0 term =
    infer0 term
        |> Result.map
            (\( _, _, type1 ) ->
                showType type1
            )



-- This expands the final type according to equations


showFinalInfer : Term -> Result (List TypeError) String
showFinalInfer term =
    infer0 term
        |> Result.andThen
            (\( _, eqs, type1 ) ->
                -- TODO: you need to show the context, and var bindings
                expandType type1 eqs
                    |> Result.map showType
            )


showValue : Value -> String
showValue val =
    case val of
        PairValue fst snd ->
            String.concat [ "(", showValue fst, ", ", showValue snd, ")" ]

        LeftValue val1 ->
            String.concat [ "L(", showValue val1, ")" ]

        RightValue val1 ->
            String.concat [ "R(", showValue val1, ")" ]

        Closure { env, var, body } ->
            String.concat
                [ "{"
                , showEnvironment env
                , if AssocList.isEmpty env then
                    "lam("

                  else
                    " |- lam("
                , var
                , " -> "
                , showTerm body
                , ")}"
                ]

        TrueValue ->
            "True"

        FalseValue ->
            "False"

        NatValue natVal ->
            showNatValue natVal


natValToInt : NatValue -> Int
natValToInt natVal =
    case natVal of
        NatZeroValue ->
            0

        NatSuccValue val1 ->
            1 + natValToInt val1


intToNatTerm : Int -> Term
intToNatTerm n =
    if n == 0 then
        NatZero

    else
        NatSucc (intToNatTerm (n - 1))


showNatValue : NatValue -> String
showNatValue natVal =
    -- case natVal of
    --     NatZeroVal ->
    --         "Z"
    --     NatSuccVal val1 ->
    --         String.concat [ "S(", showNatValue val1, ")" ]
    natValToInt natVal
        |> String.fromInt


showEnvironment : TermEnvironment -> String
showEnvironment env =
    env
        |> AssocList.toList
        |> List.concatMap
            (\( varname, vals ) ->
                case List.head vals of
                    Just val ->
                        [ String.concat [ varname, " := ", showValue val ] ]

                    Nothing ->
                        []
            )
        |> String.join ", "


showEval : TermEnvironment -> Term -> Result (List EvalError) String
showEval env term =
    eval env term
        |> Result.map
            (\val ->
                String.concat [ showTerm term, "  ~~>  ", showValue val ]
            )
