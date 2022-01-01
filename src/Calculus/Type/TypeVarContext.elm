module Calculus.Type.TypeVarContext exposing
    ( Equations
    , TypeError(..)
    , TypeVarContext
    , TypeVarStack
    , UnificationStateful
    , emptyContext
    , expandType
    , generateFreshVar
    , generateFreshVarName
    , popTypeVarStackFrame
    , popTypeVarStackFrameAndExpand
    , pushTypeVarStackFrame0
    , throwTypeError
    , unification
    )

import Calculus.Base as Base exposing (Type, TypeVarName)
import Dict exposing (Dict)
import Lib.StackedSet as StackedSet exposing (StackedSet)
import Lib.State.StatefulWithErr as State exposing (StatefulWithErr)
import Set exposing (Set)



-- ===Types===


type alias TypeVarContext =
    State


type alias State =
    { nextTypeVar : Int
    , typeVarStack : TypeVarStack
    , equations : Equations
    }


type alias UnificationStateful a =
    StatefulWithErr (List TypeError) State a


type alias TypeVarStack =
    StackedSet TypeVarName


type alias Equations =
    Dict TypeVarName Type


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
    | ExpectedFrozenType
    | InfiniteType TypeVarName
    | CantPopEmptyTypeVarContext



-- ===State===


emptyContext : State
emptyContext =
    { nextTypeVar = 0
    , typeVarStack = emptyTypeVarStack
    , equations = emptyEquations
    }



-- ===Errors===


throwTypeError : List TypeError -> UnificationStateful a
throwTypeError =
    State.error



-- ===TYPE VAR STACK===


emptyTypeVarStack : TypeVarStack
emptyTypeVarStack =
    StackedSet.empty


pushTypeVar : Int -> TypeVarStack -> TypeVarStack
pushTypeVar n stack =
    StackedSet.pushElement (String.fromInt n) stack


moveTypeVarStackFrame : TypeVarName -> Set TypeVarName -> TypeVarStack -> TypeVarStack
moveTypeVarStackFrame =
    StackedSet.move


pushTypeVarStackFrame0 : UnificationStateful ()
pushTypeVarStackFrame0 =
    State.update0
        (\({ typeVarStack } as state) ->
            { state | typeVarStack = StackedSet.pushFrame typeVarStack }
        )


popTypeVarStackFrame : UnificationStateful (Set TypeVarName)
popTypeVarStackFrame =
    State.create
        (\({ typeVarStack } as state) ->
            case StackedSet.popFrame typeVarStack of
                Just ( vars, newTypeVarStack ) ->
                    Ok ( { state | typeVarStack = newTypeVarStack }, vars )

                Nothing ->
                    Err [ CantPopEmptyTypeVarContext ]
        )


popTypeVarStackFrameAndExpand : Type -> UnificationStateful ( Set TypeVarName, Type )
popTypeVarStackFrameAndExpand type0 =
    -- TODO: You need to clean up the garbage equations here...
    --       But it shouldn't hurt to not clean them up.
    --       How to mark certain equations as garbage that should be cleaned up on next pop?
    popTypeVarStackFrame
        |> State.andThen
            (\typeVars ->
                expandType type0
                    |> State.map (\expandedType0 -> ( typeVars, expandedType0 ))
            )



--===Fresh Type Vars===


newTypeVar : Int -> ( Int, Type )
newTypeVar n =
    ( n + 1, Base.VarType (String.fromInt n) )


generateFreshVar : UnificationStateful Type
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


generateFreshVarName : UnificationStateful TypeVarName
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
                , String.fromInt nextTypeVar
                )
        )



--===Equations====


emptyEquations : Equations
emptyEquations =
    Dict.empty


lookupEquations : TypeVarName -> Equations -> Maybe Type
lookupEquations =
    Dict.get


extendEquations : TypeVarName -> Type -> UnificationStateful ()
extendEquations typeVarName type0 =
    -- TODO: is it ok if we don't expand `type0` here? Seems to be ok... but that may become false in the future and generate an epic bug.
    --       Seems like this would be a better place to expand
    State.update0
        (\({ equations, typeVarStack } as state) ->
            { state
                | equations = Dict.insert typeVarName type0 equations
                , typeVarStack =
                    typeVarStack
                        |> moveTypeVarStackFrame typeVarName (Base.getTypeVars type0)
            }
        )



-- ===EXPANSION===


expandType_MAY_INFINITE_CYCLE : Type -> UnificationStateful Type
expandType_MAY_INFINITE_CYCLE type0 =
    case type0 of
        Base.VarType n ->
            State.get0
                (\{ equations } ->
                    case lookupEquations n equations of
                        Just type1 ->
                            expandType_MAY_INFINITE_CYCLE type1

                        Nothing ->
                            State.return (Base.VarType n)
                )

        Base.Product type1 type2 ->
            State.map2 Base.Product
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        Base.Sum type1 type2 ->
            State.map2 Base.Sum
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        Base.Arrow type1 type2 ->
            State.map2 Base.Arrow
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        Base.ConstBool ->
            State.return Base.ConstBool

        Base.ConstNat ->
            State.return Base.ConstNat

        Base.List type1 ->
            expandType_MAY_INFINITE_CYCLE type1
                |> State.map Base.List

        Base.Frozen type1 ->
            expandType_MAY_INFINITE_CYCLE type1
                |> State.map Base.Frozen

        Base.ForAll typeVar type1 ->
            -- TODO
            Debug.todo ""


expandType : Type -> UnificationStateful Type
expandType type0 =
    expandTypeWithCycleDetection type0 Set.empty


expandTypeAtTypeVarName : TypeVarName -> UnificationStateful (Maybe Type)
expandTypeAtTypeVarName typeVarName =
    State.get0
        (\state0 ->
            case lookupEquations typeVarName state0.equations of
                Just type0 ->
                    expandType type0
                        |> State.andThen
                            (\expandedType0 ->
                                -- Just being paranoid about `equations` changing during `expandType`
                                State.second
                                    (State.update0
                                        (\state1 ->
                                            -- TODO: Would it be a good idea to use extendEquations here?
                                            { state1 | equations = Dict.insert typeVarName expandedType0 state1.equations }
                                        )
                                    )
                                    (State.return (Just expandedType0))
                            )

                Nothing ->
                    State.return Nothing
        )



-- This expansion can't loop. It will detect infinite types.


expandTypeWithCycleDetection : Type -> Set TypeVarName -> UnificationStateful Type
expandTypeWithCycleDetection type0 seenVars =
    case type0 of
        Base.VarType n ->
            if Set.member n seenVars then
                throwTypeError [ InfiniteType n ]

            else
                State.get0
                    (\{ equations } ->
                        case lookupEquations n equations of
                            Just type1 ->
                                expandTypeWithCycleDetection type1 (Set.insert n seenVars)

                            Nothing ->
                                State.return (Base.VarType n)
                    )

        Base.Product type1 type2 ->
            State.map2 Base.Product
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        Base.Sum type1 type2 ->
            State.map2 Base.Sum
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        Base.Arrow type1 type2 ->
            State.map2 Base.Arrow
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        Base.ConstBool ->
            State.return Base.ConstBool

        Base.ConstNat ->
            State.return Base.ConstNat

        Base.List type1 ->
            expandTypeWithCycleDetection type1 seenVars
                |> State.map Base.List

        Base.Frozen type1 ->
            expandTypeWithCycleDetection type1 seenVars
                |> State.map Base.Frozen

        Base.ForAll typeVar type1 ->
            -- TODO
            Debug.todo ""



-- ===UNIFICATION===


unification : Type -> Type -> UnificationStateful Type
unification type0 type1 =
    case ( type0, type1 ) of
        -- ===TYPE VARS===
        ( Base.VarType id0, Base.VarType id1 ) ->
            State.map2
                Tuple.pair
                (expandTypeAtTypeVarName id0)
                (expandTypeAtTypeVarName id1)
                |> State.andThen
                    (\( maybeExpandedType0, maybeExpandedType1 ) ->
                        case ( maybeExpandedType0, maybeExpandedType1 ) of
                            ( Just expandedType0, Just expandedType1 ) ->
                                unification expandedType0 expandedType1

                            ( Just expandedType0, Nothing ) ->
                                State.second
                                    (extendEquations id1 expandedType0)
                                    (State.return expandedType0)

                            ( Nothing, Just expandedType1 ) ->
                                State.second
                                    (extendEquations id0 expandedType1)
                                    (State.return expandedType1)

                            ( Nothing, Nothing ) ->
                                if id0 == id1 then
                                    State.return (Base.VarType id0)

                                else if id0 < id1 then
                                    State.second
                                        (extendEquations id0 (Base.VarType id1))
                                        (State.return (Base.VarType id1))

                                else
                                    State.second
                                        (extendEquations id1 (Base.VarType id0))
                                        (State.return (Base.VarType id1))
                    )

        ( Base.VarType id0, _ ) ->
            expandTypeAtTypeVarName id0
                |> State.andThen
                    (\maybeExpandedType0 ->
                        case maybeExpandedType0 of
                            Just expandedType0 ->
                                unification expandedType0 type1

                            Nothing ->
                                State.second
                                    (extendEquations id0 type1)
                                    (State.return type1)
                    )

        ( _, Base.VarType id1 ) ->
            expandTypeAtTypeVarName id1
                |> State.andThen
                    (\maybeExpandedType1 ->
                        case maybeExpandedType1 of
                            Just expandedType1 ->
                                unification type0 expandedType1

                            Nothing ->
                                State.second
                                    (extendEquations id1 type0)
                                    (State.return type0)
                    )

        -- ===PRODUCT===
        ( Base.Product type00 type01, Base.Product type10 type11 ) ->
            State.map2 Base.Product
                (unification type00 type10)
                (unification type01 type11)

        ( Base.Product _ _, _ ) ->
            throwTypeError [ ExpectedProductType ]

        -- ===ARROW===
        ( Base.Arrow type00 type01, Base.Arrow type10 type11 ) ->
            State.map2 Base.Arrow
                (unification type00 type10)
                (unification type01 type11)

        ( Base.Arrow _ _, _ ) ->
            throwTypeError [ ExpectedArrowType ]

        -- ===SUM===
        ( Base.Sum type00 type01, Base.Sum type10 type11 ) ->
            State.map2 Base.Sum
                (unification type00 type10)
                (unification type01 type11)

        ( Base.Sum _ _, _ ) ->
            throwTypeError [ ExpectedSumType ]

        -- ===BOOL===
        ( Base.ConstBool, Base.ConstBool ) ->
            State.return Base.ConstBool

        ( Base.ConstBool, _ ) ->
            throwTypeError [ ExpectedBoolType ]

        -- ===NAT===
        ( Base.ConstNat, Base.ConstNat ) ->
            State.return Base.ConstNat

        ( Base.ConstNat, _ ) ->
            throwTypeError [ ExpectedNatType ]

        -- ===List===
        ( Base.List type00, Base.List type11 ) ->
            unification type00 type11
                |> State.map Base.List

        ( Base.List _, _ ) ->
            throwTypeError [ ExpectedListType ]

        -- ===Frozen===
        ( Base.Frozen type00, Base.Frozen type11 ) ->
            unification type00 type11
                |> State.map Base.Frozen

        ( Base.Frozen _, _ ) ->
            throwTypeError [ ExpectedFrozenType ]

        -- Forall
        ( Base.ForAll _ _, _ ) ->
            -- TODO?
            Debug.todo ""
