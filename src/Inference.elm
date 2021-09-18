module Inference exposing (..)

import AssocList exposing (Dict)
import LambdaBasics exposing (..)
import Set exposing (Set)
import StackedSet exposing (StackedSet)
import StatefulWithErr as State exposing (StatefulWithErr)
import TypeVarContext


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
    | CantPopEmptyTypeVarContext


newTypeVar : TypeVarName -> ( TypeVarName, Type )
newTypeVar n =
    ( n + 1, VarType n )



-- ===TERM VAR CONTEXT===
-- The list serves as a stack of types
-- - any element in the stack shadows everything below it.


type alias TermVarContext =
    Dict TermVarName (List Type)


emptyContext : TermVarContext
emptyContext =
    AssocList.empty


popVarFromContext : String -> TermVarContext -> TermVarContext
popVarFromContext varName context0 =
    AssocList.update varName
        (Maybe.andThen List.tail)
        context0


lookupType : TermVarName -> TermVarContext -> Maybe Type
lookupType varName context0 =
    AssocList.get varName context0
        |> Maybe.andThen List.head


pushVarToContext : TermVarName -> Type -> TermVarContext -> TermVarContext
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



-- ===TYPE VAR STACK===


type alias TypeVarStack =
    StackedSet TypeVarName


emptyTypeVarStack : TypeVarStack
emptyTypeVarStack =
    StackedSet.empty


pushTypeVar : TypeVarName -> TypeVarStack -> TypeVarStack
pushTypeVar =
    StackedSet.pushElement


moveTypeVarStackFrame : TypeVarName -> Set TypeVarName -> TypeVarStack -> TypeVarStack
moveTypeVarStackFrame =
    StackedSet.move



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
    -- TODO: This should actually also modify `typeVarStack`
    -- TODO: is it ok if we don't expand `type0` here? Seems to be ok... but that may become false in the future and generate an epic bug.
    --       Seems like this would be a better place to expand
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

        ForAll typeVar type1 ->
            Debug.todo ""



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

        ForAll typeVar type1 ->
            Debug.todo ""



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

                    _ ->
                        Debug.todo ""
            )



-- ===STATEFUL MONAD INFERENCE===


type alias State =
    { nextTypeVar : TypeVarName
    , context : TermVarContext
    , equations : Equations
    , typeVarStack : TypeVarStack
    }


emptyState : State
emptyState =
    { nextTypeVar = 0
    , context = emptyContext
    , equations = emptyEquations
    , typeVarStack = emptyTypeVarStack
    }


type alias InferenceContext a =
    StatefulWithErr (List TypeError) State a


generateFreshVar : InferenceContext Type
generateFreshVar =
    State.create
        (\({ nextTypeVar, typeVarStack } as state0) ->
            let
                ( nextTypeVar1, type1 ) =
                    newTypeVar nextTypeVar
            in
            Ok
                ( { state0
                    | nextTypeVar = nextTypeVar1
                    , typeVarStack = pushTypeVar nextTypeVar typeVarStack
                  }
                , type1
                )
        )


generateFreshVarName : InferenceContext TypeVarName
generateFreshVarName =
    State.create
        (\({ nextTypeVar, typeVarStack } as state0) ->
            let
                ( nextTypeVar1, _ ) =
                    newTypeVar nextTypeVar
            in
            Ok
                ( { state0
                    | nextTypeVar = nextTypeVar1
                    , typeVarStack = pushTypeVar nextTypeVar typeVarStack
                  }
                , nextTypeVar1
                )
        )


getContext : (TermVarContext -> InferenceContext a) -> InferenceContext a
getContext f =
    State.get0 (\{ context } -> f context)


getEquations : (Equations -> InferenceContext a) -> InferenceContext a
getEquations f =
    State.get0 (\{ equations } -> f equations)


updateContext : (TermVarContext -> TermVarContext) -> InferenceContext a -> InferenceContext a
updateContext nextContext =
    State.update (\({ context } as state) -> { state | context = nextContext context })


updateContext0 : (TermVarContext -> TermVarContext) -> InferenceContext ()
updateContext0 nextContext =
    State.update0 (\({ context } as state) -> { state | context = nextContext context })


updateEquations : (Equations -> Equations) -> InferenceContext a -> InferenceContext a
updateEquations nextEquations =
    State.update (\({ equations } as state) -> { state | equations = nextEquations equations })


updateTypeVarStack0 : (TypeVarStack -> TypeVarStack) -> InferenceContext ()
updateTypeVarStack0 nextTypeVarStack =
    State.update0 (\({ typeVarStack } as state) -> { state | typeVarStack = nextTypeVarStack typeVarStack })


updateTypeVarStack : (TypeVarStack -> TypeVarStack) -> InferenceContext a -> InferenceContext a
updateTypeVarStack nextTypeVarStack =
    State.update (\({ typeVarStack } as state) -> { state | typeVarStack = nextTypeVarStack typeVarStack })


pushTypeVarStackFrame0 : InferenceContext ()
pushTypeVarStackFrame0 =
    updateTypeVarStack0 StackedSet.pushFrame


pushTypeVarStackFrame : InferenceContext a -> InferenceContext a
pushTypeVarStackFrame =
    updateTypeVarStack StackedSet.pushFrame


popTypeVarStackFrame : InferenceContext (Set TypeVarName)
popTypeVarStackFrame =
    State.create
        (\({ typeVarStack } as state) ->
            case StackedSet.popFrame typeVarStack of
                Just ( vars, newTypeVarStack ) ->
                    Ok ( { state | typeVarStack = newTypeVarStack }, vars )

                Nothing ->
                    Err [ CantPopEmptyTypeVarContext ]
        )


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



-- ===TYPE-VARIABLE RENAMING===
-- TODO: I actually need a function
--            generateFreshVarTo term
--       that generates a type variable that is fresh to the whole term `term`


replaceTypeVarWithFreshVar : TypeVarName -> Type -> InferenceContext Type
replaceTypeVarWithFreshVar var0 type0 =
    -- WARNING: We assume that `freshVar` is fresh for `var0, type0`
    -- TODO: fuck... I want more than freshness. I actually don't want any local bound variables in `type0` to have name `freshVar`
    case type0 of
        VarType var ->
            if var == var0 then
                generateFreshVarName
                    |> State.andThen
                        (\freshVar ->
                            State.return (VarType freshVar)
                        )

            else
                State.return (VarType var)

        Product type1 type2 ->
            State.map2 Product
                (replaceTypeVarWithFreshVar var0 type1)
                (replaceTypeVarWithFreshVar var0 type2)

        Sum type1 type2 ->
            State.map2 Sum
                (replaceTypeVarWithFreshVar var0 type1)
                (replaceTypeVarWithFreshVar var0 type2)

        Arrow type1 type2 ->
            State.map2 Arrow
                (replaceTypeVarWithFreshVar var0 type1)
                (replaceTypeVarWithFreshVar var0 type2)

        LambdaBool ->
            State.return LambdaBool

        LambdaNat ->
            State.return LambdaNat

        LambdaList type1 ->
            replaceTypeVarWithFreshVar var0 type1
                |> State.map LambdaList

        ForAll var type1 ->
            -- TODO: watch out for shadowing
            if var == var0 then
                State.return (ForAll var type1)

            else
                -- TODO: this is wrong... can still shadow
                replaceTypeVarWithFreshVar var0 type1
                    |> State.map (ForAll var)


instantiateForAll : Type -> InferenceContext Type
instantiateForAll type0 =
    -- This assummes that the `type0` has a core forall-free type wrapped inside with a bunch of foralls
    -- e.g.
    --   ForAll "x" (ForAll "y" (Arrow (VarType "x") (VarType "y")))
    -- but not
    --   ForAll "x" (Arrow (ForAll "y" (VarType "y")) (VarType "x"))
    -- It won't instantiate inner foralls.
    case type0 of
        ForAll var type1 ->
            -- I need to generate a fresh var newVar, and then replace each occurence of var
            -- with newVar in type1
            -- TODO: this is wrong!
            replaceTypeVarWithFreshVar var type1
                |> State.andThen
                    (\type2 ->
                        instantiateForAll type2
                    )

        _ ->
            State.return type0



-- ===INFERENCE===


infer : Term -> InferenceContext Type
infer term =
    case term of
        VarUse varName ->
            getContext
                (\context0 ->
                    case context0 |> lookupType varName of
                        Just type0 ->
                            instantiateForAll type0

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

        MatchProduct { arg, var0, var1, body } ->
            -- argType0 := infer arg
            -- varType0 := generateFreshVar
            -- varType1 := generateFreshVar
            -- unify (Product varType0 varType1) argType
            -- updateContext
            --   (\context ->
            --     context |> pushVarToContext var0 varType0
            --                pushVarToContext var1 varType1
            --   );
            -- infer body
            -- updateContext
            --   (\context ->
            --     context |> popVarFromContext var1
            --                popVarFromContext var0
            --   );
            State.andThen3
                (\argType varType0 varType1 ->
                    State.second
                        (unify (Product varType0 varType1) argType)
                        (State.mid
                            (updateContext0
                                (\context ->
                                    context
                                        |> pushVarToContext var0 varType0
                                        |> pushVarToContext var1 varType1
                                )
                            )
                            (infer body)
                            (updateContext0
                                (\context ->
                                    context
                                        |> popVarFromContext var1
                                        |> popVarFromContext var0
                                )
                            )
                        )
                )
                (infer arg)
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
            -- resultType := unify (LambdaList headType) tailType
            State.andThen2
                (\headType tailType ->
                    unify (LambdaList headType) tailType
                )
                (infer headTerm)
                (infer tailTerm)

        ListLoop { initState, loop, arg } ->
            -- stateType0 := infer initState;
            -- argType0 := infer arg;
            -- innerListVar := generateFreshVar;
            -- argType1 := unify (LambdaList innerListVar) argType0;
            -- case argType1 of
            --   LambdaList innerType0 ->
            --     loopVar := generateFreshVar;
            --     stateVar := generateFreshVar;
            --     innerType1 := unify loopVar innerType0;
            --     stateType1 := unify stateVar stateType0;
            --
            --     updateContext
            --       (\context ->
            --           context
            --               |> pushVarToContext loop.listElementVar innerType1
            --               |> pushVarToContext loop.stateVar stateType1
            --       );
            --     loopBodyType := infer loop.body
            --     updateContext
            --       (\context ->
            --           context
            --               |> popVarToContext loop.listElementVar
            --               |> popVarToContext loop.stateVar
            --       );
            --
            --     unify loopBodyType stateType1
            --
            --   _ ->
            --     throwTypeError [ ExpectedListType ]
            State.andThen4
                (\stateType0 argType0 loopVar stateVar ->
                    generateFreshVar
                        |> State.andThen
                            (\innerListVar ->
                                unify (LambdaList innerListVar) argType0
                                    |> State.andThen
                                        (\argType1 ->
                                            case argType1 of
                                                LambdaList innerType0 ->
                                                    State.andThen2
                                                        (\stateType1 innerType1 ->
                                                            State.mid
                                                                (updateContext0
                                                                    (\context ->
                                                                        context
                                                                            |> pushVarToContext loop.listElementVar innerType1
                                                                            |> pushVarToContext loop.stateVar stateType1
                                                                    )
                                                                )
                                                                (infer loop.body)
                                                                (updateContext0
                                                                    (\context ->
                                                                        context
                                                                            |> popVarFromContext loop.listElementVar
                                                                            |> popVarFromContext loop.stateVar
                                                                    )
                                                                )
                                                                |> State.andThen
                                                                    (\loopBodyType ->
                                                                        unify loopBodyType stateType1
                                                                    )
                                                        )
                                                        (unify stateVar stateType0)
                                                        (unify loopVar innerType0)

                                                _ ->
                                                    throwTypeError [ ExpectedListType ]
                                        )
                            )
                )
                (infer initState)
                (infer arg)
                generateFreshVar
                generateFreshVar

        Let var exp body ->
            -- 1. I need to infer exp and somehow close it's vars
            -- after that... I'll just infer body in a new context (and afterwards I'll remove the new var from context
            -- TODO: this won't work... this will close too much
            inferAndClose exp
                |> State.andThen
                    (\expType ->
                        State.mid
                            (updateContext0 (\context -> context |> pushVarToContext var expType))
                            (infer exp)
                            (updateContext0 (\context -> context |> popVarFromContext var))
                    )


inferAndClose : Term -> InferenceContext Type
inferAndClose term =
    -- TODO
    -- I need to close off the vars
    infer term


infer0 : Term -> Result (List TypeError) ( TermVarContext, Equations, Type )
infer0 term =
    State.run (infer term)
        emptyState
        |> Result.map
            (\( state, type0 ) ->
                ( state.context, state.equations, type0 )
            )
