module LambdaBasics exposing (..)

import AssocList exposing (Dict)
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



-- helpers


intToNatTerm : Int -> Term
intToNatTerm n =
    if n == 0 then
        NatZero

    else
        NatSucc (intToNatTerm (n - 1))
