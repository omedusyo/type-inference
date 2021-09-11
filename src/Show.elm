module Show exposing (..)

import AssocList exposing (Dict)
import Evaluation exposing (..)
import Inference exposing (..)
import Main exposing (..)



-- ===TERMS===


showTerm : Term -> String
showTerm term =
    case term of
        VarUse varname ->
            String.concat [ "$", varname ]

        Pair fst snd ->
            String.concat [ "(", showTerm fst, ", ", showTerm snd, ")" ]

        Fst term1 ->
            String.concat [ showTerm term1, ".0" ]

        Snd term1 ->
            String.concat [ showTerm term1, ".1" ]

        Abstraction var body ->
            String.concat [ "lam(", var, " -> ", showTerm body, ")" ]

        Application fn arg ->
            String.concat [ "[", showTerm fn, " ", showTerm arg, "]" ]

        Left term1 ->
            String.concat [ "L(", showTerm term1, ")" ]

        Right term1 ->
            String.concat [ "R(", showTerm term1, ")" ]

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            String.concat
                [ "case("
                , showTerm arg
                , ", Left "
                , leftVar
                , " -> "
                , showTerm leftBody
                , ", Right "
                , rightVar
                , " -> "
                , showTerm rightBody
                , ")"
                ]

        BoolTrue ->
            "True"

        BoolFalse ->
            "False"

        IfThenElse arg leftBody rightBody ->
            String.concat
                [ "if("
                , showTerm arg
                , " then "
                , showTerm leftBody
                , " else "
                , showTerm rightBody
                , ")"
                ]

        NatZero ->
            "Z"

        NatSucc term1 ->
            String.concat [ "S(", showTerm term1, ")" ]

        NatLoop { base, loop, arg } ->
            String.concat
                [ "nat-loop("
                , loop.stateVar
                , " := "
                , showTerm base
                , ", for "
                , loop.indexVar
                , " in range("
                , showTerm arg
                , ") do "
                , loop.stateVar
                , " := "
                , showTerm loop.body
                , ")"
                ]

        EmptyList ->
            "[]"

        Cons headTerm tailTerm ->
            String.concat
                [ "Cons("
                , showTerm headTerm
                , ", "
                , showTerm tailTerm
                , ")"
                ]

        ListLoop { initState, loop, arg } ->
            String.concat
                [ "list-loop("
                , loop.stateVar
                , " := "
                , showTerm initState
                , ", for "
                , loop.listElementVar
                , " in reverse("
                , showTerm arg
                , ") do "
                , loop.stateVar
                , " := "
                , showTerm loop.body
                , ")"
                ]


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
            String.concat [ "(", showValue fst, ", ", showValue snd, ")" ]

        LeftValue val1 ->
            String.concat [ "L(", showValue val1, ")" ]

        RightValue val1 ->
            String.concat [ "R(", showValue val1, ")" ]

        Closure { env, var, body } ->
            String.concat
                [ "{"
                , showTermEnvironment env
                , if AssocList.isEmpty env then
                    "lam("

                  else
                    " |- lam("
                , var
                , " -> "
                , showTerm body
                , ")}"
                ]

        TrueValue ->
            "True"

        FalseValue ->
            "False"

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


intToNatTerm : Int -> Term
intToNatTerm n =
    if n == 0 then
        NatZero

    else
        NatSucc (intToNatTerm (n - 1))


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
            "[]"

        ConsValue headValue tailValue ->
            String.concat
                [ "Cons("
                , showValue headValue
                , ", "
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
