module Show exposing (..)

import AssocList exposing (Dict)
import Evaluation exposing (..)
import Inference exposing (..)
import LambdaBasics exposing (..)



-- ===TERMS===


showTerm : Term -> String
showTerm term =
    case term of
        VarUse varname ->
            -- $foo
            String.concat [ "$", varname ]

        Pair fst snd ->
            -- (pair e1 e2)
            String.concat [ "(pair ", showTerm fst, " ", showTerm snd, ")" ]

        MatchProduct { arg, var0, var1, body } ->
            String.concat
                [ "(match-pair "
                , showTerm arg
                , " { (pair "
                , var0
                , " "
                , var1
                , ") . "
                , showTerm body
                , " })"
                ]

        Abstraction var body ->
            -- (fn { x . body })
            String.concat [ "(fn { ", var, " . ", showTerm body, " })" ]

        Application fn arg ->
            -- (@ e1 e2)
            String.concat [ "(@ ", showTerm fn, " ", showTerm arg, ")" ]

        Left term1 ->
            -- (left e)
            String.concat [ "(left ", showTerm term1, ")" ]

        Right term1 ->
            -- (right e)
            String.concat [ "(right ", showTerm term1, ")" ]

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            -- (match-sum e { (left x) . e1 } { (right y) . e2 })
            String.concat
                [ "(match-sum "
                , showTerm arg
                , " { (left "
                , leftVar
                , ") . "
                , showTerm leftBody
                , " } { (right "
                , rightVar
                , ") . "
                , showTerm rightBody
                , " })"
                ]

        BoolTrue ->
            "true"

        BoolFalse ->
            "false"

        IfThenElse arg leftBody rightBody ->
            -- (if { x } { y })
            String.concat
                [ "(if "
                , showTerm arg
                , " { "
                , showTerm leftBody
                , " } { "
                , showTerm rightBody
                , " })"
                ]

        NatZero ->
            "0"

        NatSucc term1 ->
            -- (succ e)
            String.concat [ "(succ ", showTerm term1, ")" ]

        NatLoop { base, loop, arg } ->
            -- (loop-nat nExp initStateExp { i state . body })
            String.concat
                [ "(loop-nat "
                , showTerm arg
                , " "
                , showTerm base
                , " { "
                , loop.indexVar
                , " "
                , loop.stateVar
                , " . "
                , showTerm loop.body
                , " })"
                ]

        EmptyList ->
            "empty-list"

        Cons headTerm tailTerm ->
            -- (cons e1 e2)
            String.concat
                [ "(cons "
                , showTerm headTerm
                , " "
                , showTerm tailTerm
                , ")"
                ]

        ListLoop { initState, loop, arg } ->
            -- (loop-list xsExp initStateExp { x state . body } )
            String.concat
                [ "(list-loop "
                , showTerm arg
                , " "
                , showTerm initState
                , " { "
                , loop.listElementVar
                , " "
                , loop.stateVar
                , " . "
                , showTerm loop.body
                , " })"
                ]

        Let var exp body ->
            Debug.todo ""


showTermEnvironment : TermEnvironment -> String
showTermEnvironment env =
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



-- ===VALUES===


showEval : TermEnvironment -> Term -> Result (List EvalError) String
showEval env term =
    eval env term
        |> Result.map
            (\val ->
                String.concat [ showTerm term, "  ~~>  ", showValue val ]
            )


showValue : Value -> String
showValue val =
    case val of
        PairValue fst snd ->
            String.concat [ "(pair ", showValue fst, " ", showValue snd, ")" ]

        LeftValue val1 ->
            String.concat [ "(left ", showValue val1, ")" ]

        RightValue val1 ->
            String.concat [ "(right ", showValue val1, ")" ]

        Closure { env, var, body } ->
            String.concat
                [ "<"
                , showTermEnvironment env
                , if AssocList.isEmpty env then
                    "(fn { "

                  else
                    " |- (fn { "
                , var
                , " . "
                , showTerm body
                , " })>"
                ]

        TrueValue ->
            "true"

        FalseValue ->
            "false"

        NatValue natVal ->
            showNatValue natVal

        ListValue listValue ->
            showListValue listValue


natValToInt : NatValue -> Int
natValToInt natVal =
    case natVal of
        NatZeroValue ->
            0

        NatSuccValue val1 ->
            1 + natValToInt val1


showNatValue : NatValue -> String
showNatValue natVal =
    -- case natVal of
    --     NatZeroVal ->
    --         "Z"
    --     NatSuccVal val1 ->
    --         String.concat [ "S(", showNatValue val1, ")" ]
    natValToInt natVal
        |> String.fromInt


showListValue : ListValue -> String
showListValue listValue =
    case listValue of
        EmptyListValue ->
            "empty-list"

        ConsValue headValue tailValue ->
            String.concat
                [ "(cons "
                , showValue headValue
                , " "
                , showValue tailValue
                , ")"
                ]



-- ===TYPES===


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

        LambdaList type1 ->
            String.concat [ "List(", showType type1, ")" ]



-- ===INFERENCE===


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
