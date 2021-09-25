module LambdaBasics exposing (..)

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
    | BoolTrue
    | BoolFalse
      -- first is the TestExpression then LeftBranch then RightBranch
    | IfThenElse Term Term Term
      --==Natural Number Object==
      -- intro
      -- TODO: introduces NatConst Int for efficiency
    | NatZero
    | NatSucc Term
      -- elim
      -- f : Nat -> X
      -- f 0 = ....
      -- f (n + 1) = you can use `f n` here
    | NatLoop
        -- TODO: rename to initState var
        { base : Term
        , loop :
            { indexVar : TermVarName
            , stateVar : TermVarName -- <- this should be interpreted as `f n`
            , body : Term
            }
        , arg : Term
        }
      -- ==Lists==
    | EmptyList
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
      -- ==Freeze==
    | Delay Term
    | Force Term
      -- ==Let==
    | Let TermVarName Term Term


type alias TypeVarName =
    Int


type Type
    = VarType TypeVarName
      -- add unit type
    | Product Type Type
      -- add zero type
    | Sum Type Type
    | Arrow Type Type
    | LambdaBool
    | LambdaNat
    | LambdaList Type
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

        LambdaBool ->
            Set.empty

        LambdaNat ->
            Set.empty

        LambdaList type1 ->
            getTypeVars type1

        Frozen type1 ->
            getTypeVars type1

        ForAll var type1 ->
            -- TODO: I probably won't make use of this in let polymorphism
            Debug.todo ""



-- Categories := collections of Structured Types
--   one of such categories is Type
--   another could be TypeWithPrint
--   or Monoid


type BasicCategory
    = SET


type CategoryTerm
    = SETType Type
    | -- This is Sigma intro
      Module Type Term
      -- This is Sigma elim
    | MatchModule { arg : CategoryTerm, var0 : TypeVarName, var1 : TermVarName, body : CategoryTerm }


type Category
    = BasicCategory BasicCategory
    | -- This is Sigma
      Interface TypeVarName Type



-- helpers


intToNatTerm : Int -> Term
intToNatTerm n =
    if n == 0 then
        NatZero

    else
        NatSucc (intToNatTerm (n - 1))
