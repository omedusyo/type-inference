module TypeVarContext exposing
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
    , pushTypeVarStackFrame0
    , throwTypeError
    , unification
    )

import AssocList exposing (Dict)
import LambdaBasics exposing (Type(..), TypeVarName)
import Set exposing (Set)
import StackedSet exposing (StackedSet)
import StatefulWithErr as State exposing (StatefulWithErr)



-- ===Types===


type alias TypeVarContext =
    State


type alias State =
    { nextTypeVar : TypeVarName
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
    | InfiniteType Int
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


pushTypeVar : TypeVarName -> TypeVarStack -> TypeVarStack
pushTypeVar =
    StackedSet.pushElement


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



--===Fresh Type Vars===


newTypeVar : TypeVarName -> ( TypeVarName, Type )
newTypeVar n =
    ( n + 1, VarType n )


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
                , nextTypeVar1
                )
        )



--===Equations====


emptyEquations : Equations
emptyEquations =
    AssocList.empty


lookupEquations : TypeVarName -> Equations -> Maybe Type
lookupEquations =
    AssocList.get


extendEquations : TypeVarName -> Type -> UnificationStateful ()
extendEquations typeVarName type0 =
    -- TODO: is it ok if we don't expand `type0` here? Seems to be ok... but that may become false in the future and generate an epic bug.
    --       Seems like this would be a better place to expand
    State.update0
        (\({ equations, typeVarStack } as state) ->
            { state
                | equations = AssocList.insert typeVarName type0 equations
                , typeVarStack =
                    typeVarStack
                        |> moveTypeVarStackFrame typeVarName (LambdaBasics.getTypeVars type0)
            }
        )



-- ===EXPANSION===


expandType_MAY_INFINITE_CYCLE : Type -> UnificationStateful Type
expandType_MAY_INFINITE_CYCLE type0 =
    case type0 of
        VarType n ->
            State.get0
                (\{ equations } ->
                    case lookupEquations n equations of
                        Just type1 ->
                            expandType_MAY_INFINITE_CYCLE type1

                        Nothing ->
                            State.return (VarType n)
                )

        Product type1 type2 ->
            State.map2 Product
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        Sum type1 type2 ->
            State.map2 Sum
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        Arrow type1 type2 ->
            State.map2 Arrow
                (expandType_MAY_INFINITE_CYCLE type1)
                (expandType_MAY_INFINITE_CYCLE type2)

        LambdaBool ->
            State.return LambdaBool

        LambdaNat ->
            State.return LambdaNat

        LambdaList type1 ->
            expandType_MAY_INFINITE_CYCLE type1
                |> State.map LambdaList

        ForAll typeVar type1 ->
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
                                            { state1 | equations = AssocList.insert typeVarName expandedType0 state1.equations }
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
        VarType n ->
            if Set.member n seenVars then
                throwTypeError [ InfiniteType n ]

            else
                State.get0
                    (\{ equations } ->
                        case lookupEquations n equations of
                            Just type1 ->
                                expandTypeWithCycleDetection type1 (Set.insert n seenVars)

                            Nothing ->
                                State.return (VarType n)
                    )

        Product type1 type2 ->
            State.map2 Product
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        Sum type1 type2 ->
            State.map2 Sum
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        Arrow type1 type2 ->
            State.map2 Arrow
                (expandTypeWithCycleDetection type1 seenVars)
                (expandTypeWithCycleDetection type2 seenVars)

        LambdaBool ->
            State.return LambdaBool

        LambdaNat ->
            State.return LambdaNat

        LambdaList type1 ->
            expandTypeWithCycleDetection type1 seenVars
                |> State.map LambdaList

        ForAll typeVar type1 ->
            -- TODO
            Debug.todo ""



-- ===UNIFICATION===


unification : Type -> Type -> UnificationStateful Type
unification type0 type1 =
    case ( type0, type1 ) of
        -- ===TYPE VARS===
        ( VarType id0, VarType id1 ) ->
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
                                    State.return (VarType id0)

                                else if id0 < id1 then
                                    State.second
                                        (extendEquations id0 (VarType id1))
                                        (State.return (VarType id1))

                                else
                                    State.second
                                        (extendEquations id1 (VarType id0))
                                        (State.return (VarType id1))
                    )

        ( VarType id0, _ ) ->
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

        ( _, VarType id1 ) ->
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
        ( Product type00 type01, Product type10 type11 ) ->
            State.map2 Product
                (unification type00 type10)
                (unification type01 type11)

        ( Product _ _, _ ) ->
            throwTypeError [ ExpectedProductType ]

        -- ===ARROW===
        ( Arrow type00 type01, Arrow type10 type11 ) ->
            State.map2 Arrow
                (unification type00 type10)
                (unification type01 type11)

        ( Arrow _ _, _ ) ->
            throwTypeError [ ExpectedArrowType ]

        -- ===SUM===
        ( Sum type00 type01, Sum type10 type11 ) ->
            State.map2 Sum
                (unification type00 type10)
                (unification type01 type11)

        ( Sum _ _, _ ) ->
            throwTypeError [ ExpectedSumType ]

        -- ===BOOL===
        ( LambdaBool, LambdaBool ) ->
            State.return LambdaBool

        ( LambdaBool, _ ) ->
            throwTypeError [ ExpectedBoolType ]

        -- ===NAT===
        ( LambdaNat, LambdaNat ) ->
            State.return LambdaNat

        ( LambdaNat, _ ) ->
            throwTypeError [ ExpectedNatType ]

        ( LambdaList type00, LambdaList type11 ) ->
            unification type00 type11
                |> State.map LambdaList

        ( LambdaList _, _ ) ->
            throwTypeError [ ExpectedListType ]

        ( ForAll _ _, _ ) ->
            -- TODO?
            Debug.todo ""
