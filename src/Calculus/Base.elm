module Calculus.Base exposing
    ( FunctorLiteral
    , FunctorTerm(..)
    , FunctorType
    , FunctorVarName
    , Interface
    , InterfaceAssumption(..)
    , ModuleLetBinding(..)
    , ModuleLiteral
    , ModuleTerm(..)
    , ModuleVarName
    , Term(..)
    , TermVarName
    , Type(..)
    , TypeVarName
    , getTypeVars
    , intToNatTerm
    )

import Set exposing (Set)


type alias TermVarName =
    -- TODO: We assume that variables range over lambda values, so maybe it would be better to call this ValueVarName?
    String


type Term
    = -- ==Variables==
      VarUse TermVarName
      -- ==Cartesian Product==
      -- TODO: add empty-tuple
      -- intro
    | Pair Term Term
      -- elim
    | MatchProduct
        { arg : Term
        , var0 : TermVarName
        , var1 : TermVarName
        , body : Term
        }
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
    | ConstTrue
    | ConstFalse
      -- first is the TestExpression then LeftBranch then RightBranch
    | IfThenElse Term Term Term
      --==Natural Number Object==
      -- intro
      -- TODO: introduces NatConst Int for efficiency
    | ConstZero
    | Succ Term
      -- elim
      -- f : Nat -> X
      -- f 0 = ....
      -- f (n + 1) = you can use `f n` here
    | NatLoop
        -- TODO: rename to initState var
        { base : Term
        , loop :
            { stateVar : TermVarName -- <- this should be interpreted as `f n`
            , body : Term
            }
        , arg : Term
        }
      -- ==Lists==
    | ConstEmpty
    | Cons Term Term
    | ListLoop
        { initState : Term
        , loop :
            { listElementVar : TermVarName
            , stateVar : TermVarName
            , body : Term
            }
        , arg : Term
        }
    | -- ==Freeze==
      Delay Term
    | Force Term
    | -- ==Let==
      Let TermVarName Term Term
    | -- ==Module Access==
      ModuleAccess ModuleTerm TermVarName


type alias TypeVarName =
    Int


type Type
    = VarType TypeVarName
      -- add unit type
    | Product Type Type
      -- add zero type
    | Sum Type Type
    | Arrow Type Type
    | ConstBool
    | ConstNat
    | List Type
    | Frozen Type
    | ForAll TypeVarName Type


getTypeVars : Type -> Set TypeVarName
getTypeVars type0 =
    case type0 of
        VarType var ->
            Set.singleton var

        Product type1 type2 ->
            Set.union (getTypeVars type1) (getTypeVars type2)

        Sum type1 type2 ->
            Set.union (getTypeVars type1) (getTypeVars type2)

        Arrow type1 type2 ->
            Set.union (getTypeVars type1) (getTypeVars type2)

        ConstBool ->
            Set.empty

        ConstNat ->
            Set.empty

        List type1 ->
            getTypeVars type1

        Frozen type1 ->
            getTypeVars type1

        ForAll var type1 ->
            -- TODO: I probably won't make use of this in let polymorphism
            Debug.todo ""



-- Module


type alias ModuleVarName =
    String



-- TODO:  The general Module Expression could either be
--           Module Form
--        or Module Use (as a variable)
--        or Functor Application to a Module Expression
--       q. Will we have Functor expressions? Hopefully not
--          What sort of operations over functors would I want?


type ModuleTerm
    = ModuleLiteralTerm ModuleLiteral
    | ModuleVarUse ModuleVarName
    | FunctorApplication FunctorTerm (List ModuleTerm)


type alias ModuleLiteral =
    { bindings : List ModuleLetBinding
    }


type ModuleLetBinding
    = LetTerm TermVarName Term
    | LetType TypeVarName Type
    | LetModule ModuleVarName ModuleTerm
    | LetFunctor FunctorVarName FunctorLiteral



-- Interface


type alias Interface =
    { assumptions : List InterfaceAssumption
    }


type InterfaceAssumption
    = AssumeTerm TermVarName Type
    | AssumeType TypeVarName -- There should be a second argument that's called Kind
    | AssumeModule ModuleVarName Interface
    | AssumeFunctor FunctorVarName FunctorType



-- Functors


type alias FunctorVarName =
    String


type FunctorTerm
    = FunctorVarUse FunctorVarName
    | FunctorLiteralTerm FunctorLiteral


type alias FunctorLiteral =
    { parameters : List ( ModuleVarName, Interface )
    , body : ModuleTerm
    }


type alias FunctorType =
    ( List Interface, Interface )



-- helpers


intToNatTerm : Int -> Term
intToNatTerm n =
    if n == 0 then
        ConstZero

    else
        Succ (intToNatTerm (n - 1))
