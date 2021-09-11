module Inference exposing (..)

import AssocList exposing (Dict)
import Main exposing (..)
import Set exposing (Set)
import StatefulWithErr as State exposing (StatefulWithErr)


type TypeError
    = ExpectedProductType
    | ExpectedArrowType
    | ExpectedNatType
    | ExpectedSumType
    | ExpectedMatchingTypesInCaseBranches
    | ExpectedBoolType
    | ExpectedMatchingTypesInIfThenElseBranches
    | ExpectedBaseUnifiesWithLoopBodyType
    | ExpectedListType
    | InfiniteType Int


newTypeVar : TypeVarName -> ( TypeVarName, Type )
newTypeVar n =
    ( n + 1, VarType n )



-- ===CONTEXT===
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

        LambdaList type1 ->
            LambdaList (expandType_MAY_INFINITE_CYCLE type1 eqs0)



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

        LambdaList type1 ->
            expandTypeWithCycleDetection type1 seenVars eqs0
                |> Result.map LambdaList



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

                    ( LambdaList type00, LambdaList type11 ) ->
                        unification type00 type11 eqs0
                            |> Result.map
                                (\( eqs1, typeResult ) ->
                                    ( eqs1, LambdaList typeResult )
                                )

                    ( LambdaList _, _ ) ->
                        Err [ ExpectedListType ]
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

        EmptyList ->
            generateFreshVar
                |> State.map (\var -> LambdaList var)

        Cons headTerm tailTerm ->
            -- headType := infer headTerm;
            -- tailType := infer tailTerm;
            -- case tailType of
            --   ListType typeInner ->
            --     resultType := unify headType typeInner;
            --     return (ListType resultType);
            --   _ ->
            --     throwTypeError [ExpectedListType]
            State.andThen2
                (\headType tailType ->
                    case tailType of
                        LambdaList innerType ->
                            unify headType innerType
                                |> State.andThen
                                    (\resultType ->
                                        State.return (LambdaList resultType)
                                    )

                        _ ->
                            throwTypeError [ ExpectedListType ]
                )
                (infer headTerm)
                (infer tailTerm)

        ListLoop {} ->
            Debug.todo ""


infer0 : Term -> Result (List TypeError) ( Context, Equations, Type )
infer0 term =
    State.run (infer term)
        emptyState
        |> Result.map
            (\( state, type0 ) ->
                ( state.context, state.equations, type0 )
            )
