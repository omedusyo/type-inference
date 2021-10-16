module Inference exposing (..)

import Dict exposing (Dict)
import LambdaBasics exposing (..)
import Set exposing (Set)
import StackedSet exposing (StackedSet)
import StatefulWithErr as State exposing (StatefulWithErr)
import TypeVarContext exposing (TypeError(..), TypeVarContext)



-- ===Types===


type alias State =
    { context : TermVarContext
    , typeVarContext : TypeVarContext
    }


type alias InferenceContext a =
    StatefulWithErr (List TypeError) State a


type alias TermVarContext =
    -- The list serves as a stack of types
    -- - any element in the stack shadows everything below it.
    Dict TermVarName (List Type)



-- ===State===


emptyState : State
emptyState =
    { context = emptyContext
    , typeVarContext = TypeVarContext.emptyContext
    }



-- ===Errors===


throwTypeError : List TypeError -> InferenceContext a
throwTypeError =
    State.error



-- ===Term Var Context===


emptyContext : TermVarContext
emptyContext =
    Dict.empty


lookupType : TermVarName -> TermVarContext -> Maybe Type
lookupType varName context0 =
    Dict.get varName context0
        |> Maybe.andThen List.head


pushVarToContext : TermVarName -> Type -> TermVarContext -> TermVarContext
pushVarToContext varName type0 context0 =
    Dict.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just types0 ->
                    Just (type0 :: types0)

                Nothing ->
                    Just [ type0 ]
        )
        context0


popVarFromContext : String -> TermVarContext -> TermVarContext
popVarFromContext varName context0 =
    Dict.update varName
        (Maybe.andThen List.tail)
        context0


getContext : (TermVarContext -> InferenceContext a) -> InferenceContext a
getContext f =
    State.get0 (\{ context } -> f context)


updateContext : (TermVarContext -> TermVarContext) -> InferenceContext a -> InferenceContext a
updateContext nextContext =
    State.update (\({ context } as state) -> { state | context = nextContext context })


updateContext0 : (TermVarContext -> TermVarContext) -> InferenceContext ()
updateContext0 nextContext =
    State.update0 (\({ context } as state) -> { state | context = nextContext context })



-- ===Type Var Context===


liftUnification : TypeVarContext.UnificationStateful a -> InferenceContext a
liftUnification unificationStateful =
    State.create
        (\({ typeVarContext } as state) ->
            State.run unificationStateful typeVarContext
                |> Result.map
                    (\( typeVarContext1, a ) ->
                        ( { state | typeVarContext = typeVarContext1 }, a )
                    )
        )



-- Generation of Fresh Variables


generateFreshVar : InferenceContext Type
generateFreshVar =
    liftUnification TypeVarContext.generateFreshVar


generateFreshVarName : InferenceContext TypeVarName
generateFreshVarName =
    liftUnification TypeVarContext.generateFreshVarName



-- Type Var Stack


pushTypeVarStackFrame0 : InferenceContext ()
pushTypeVarStackFrame0 =
    liftUnification TypeVarContext.pushTypeVarStackFrame0


pushTypeVarStackFrame : InferenceContext a -> InferenceContext a
pushTypeVarStackFrame inferenceContext =
    State.first inferenceContext pushTypeVarStackFrame0


popTypeVarStackFrame : InferenceContext (Set TypeVarName)
popTypeVarStackFrame =
    liftUnification TypeVarContext.popTypeVarStackFrame


popTypeVarStackFrameAndExpand : Type -> InferenceContext ( Set TypeVarName, Type )
popTypeVarStackFrameAndExpand type0 =
    liftUnification (TypeVarContext.popTypeVarStackFrameAndExpand type0)



-- ===Unification===


unify : Type -> Type -> InferenceContext Type
unify type0 type1 =
    liftUnification (TypeVarContext.unification type0 type1)



-- ===TYPE-VARIABLE RENAMING===
-- TODO: I actually need a function
--            generateFreshVarTo term
--       that generates a type variable that is fresh to the whole term `term`


replaceTypeVarWithFreshVar : TypeVarName -> TypeVarName -> Type -> InferenceContext Type
replaceTypeVarWithFreshVar var0 freshVar type0 =
    -- WARNING: We assume that `freshVar` is fresh for `var0, type0`
    -- TODO: fuck... I want more than freshness. I actually don't want any local bound variables in `type0` to have name `freshVar`
    case type0 of
        VarType var ->
            if var == var0 then
                State.return (VarType freshVar)

            else
                State.return (VarType var)

        Product type1 type2 ->
            State.map2 Product
                (replaceTypeVarWithFreshVar var0 freshVar type1)
                (replaceTypeVarWithFreshVar var0 freshVar type2)

        Sum type1 type2 ->
            State.map2 Sum
                (replaceTypeVarWithFreshVar var0 freshVar type1)
                (replaceTypeVarWithFreshVar var0 freshVar type2)

        Arrow type1 type2 ->
            State.map2 Arrow
                (replaceTypeVarWithFreshVar var0 freshVar type1)
                (replaceTypeVarWithFreshVar var0 freshVar type2)

        LambdaBool ->
            State.return LambdaBool

        LambdaNat ->
            State.return LambdaNat

        LambdaList type1 ->
            replaceTypeVarWithFreshVar var0 freshVar type1
                |> State.map LambdaList

        Frozen type1 ->
            replaceTypeVarWithFreshVar var0 freshVar type1
                |> State.map Frozen

        ForAll var type1 ->
            -- TODO: watch out for shadowing
            if var == var0 then
                State.return (ForAll var type1)

            else
                -- TODO: this is wrong... can still shadow
                replaceTypeVarWithFreshVar var0 freshVar type1
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
            generateFreshVarName
                |> State.andThen
                    (\freshVarName ->
                        replaceTypeVarWithFreshVar var freshVarName type1
                            |> State.andThen
                                (\type2 ->
                                    instantiateForAll type2
                                )
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

        -- ===Freeze===
        Delay body ->
            infer body
                |> State.map Frozen

        Force body ->
            State.andThen2
                (\bodyType0 freshVar ->
                    unify (Frozen freshVar) bodyType0
                        |> State.andThen
                            (\bodyType1 ->
                                case bodyType1 of
                                    Frozen innerType ->
                                        State.return innerType

                                    _ ->
                                        throwTypeError [ ExpectedFrozenType ]
                            )
                )
                (infer body)
                generateFreshVar

        -- ===Let===
        Let var exp body ->
            inferAndClose exp
                |> State.andThen
                    (\closedExpType ->
                        State.mid
                            (updateContext0 (\context -> context |> pushVarToContext var closedExpType))
                            (infer body)
                            (updateContext0 (\context -> context |> popVarFromContext var))
                    )

        -- ===Module Access===
        ModuleAccess module0 var ->
            Debug.todo ""


inferAndClose : Term -> InferenceContext Type
inferAndClose term =
    State.second
        pushTypeVarStackFrame0
        (infer term
            |> State.andThen
                (\type0 ->
                    popTypeVarStackFrameAndExpand type0
                        |> State.andThen
                            (\( varsToBeClosed, expandedType0 ) ->
                                let
                                    initType =
                                        expandedType0

                                    update typeVar t =
                                        ForAll typeVar t

                                    finalType =
                                        -- TODO: foldl or foldr?
                                        List.foldl update initType (Set.toList varsToBeClosed)
                                in
                                State.return finalType
                            )
                )
        )


infer0 : Term -> Result (List TypeError) ( TermVarContext, TypeVarContext, Type )
infer0 term =
    State.run (infer term)
        emptyState
        |> Result.map
            (\( state, type0 ) ->
                ( state.context, state.typeVarContext, type0 )
            )



-- Module Inference


inferInterface : ModuleTerm -> InferenceContext Interface
inferInterface moduleTerm =
    case moduleTerm of
        ModuleLiteralTerm module0 ->
            inferFromModuleLiteral module0

        ModuleVarUse moduleName ->
            -- TODO: What to do here? do we want interface variables?
            Debug.todo ""

        FunctorApplication functor modules ->
            -- TODO: What to do here? do we want interface variables?
            Debug.todo ""


inferFromModuleLiteral : ModuleLiteral -> InferenceContext Interface
inferFromModuleLiteral module0 =
    let
        inferBindings : List ModuleLetBinding -> InferenceContext (List InterfaceAssumption)
        inferBindings bindings0 =
            case bindings0 of
                [] ->
                    State.return []

                binding :: bindings1 ->
                    case binding of
                        LetTerm var term0 ->
                            inferAndClose term0
                                |> State.andThen
                                    (\type0 ->
                                        State.second
                                            (updateContext0
                                                (\context ->
                                                    context
                                                        |> pushVarToContext var type0
                                                )
                                            )
                                            (inferBindings bindings1)
                                            |> State.map
                                                (\assumptions1 ->
                                                    AssumeTerm var type0 :: assumptions1
                                                )
                                    )

                        _ ->
                            Debug.todo ""
    in
    inferBindings module0.bindings
        |> State.map (\assumptions -> { assumptions = assumptions })
