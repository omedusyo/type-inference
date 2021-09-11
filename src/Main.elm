module Main exposing (..)

import AssocList exposing (Dict)
import Set exposing (Set)


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
      -- ==Lists==
    | EmptyList
    | Cons Term Term
    | ListLoop
        { initState : Term
        , loop :
            { listVar : TermVarName
            , stateVar : TermVarName
            , body : Term
            }
        , arg : Term
        }


type alias TypeVarName =
    Int


type Type
    = VarType TypeVarName
    | Product Type Type
    | Sum Type Type
    | Arrow Type Type
    | LambdaBool
    | LambdaNat
