module Show exposing (..)

import AssocList exposing (Dict)
import Main exposing (..)


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
                , showEnvironment env
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


showEnvironment : TermEnvironment -> String
showEnvironment env =
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


showEval : TermEnvironment -> Term -> Result (List EvalError) String
showEval env term =
    eval env term
        |> Result.map
            (\val ->
                String.concat [ showTerm term, "  ~~>  ", showValue val ]
            )