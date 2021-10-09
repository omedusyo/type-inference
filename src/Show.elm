module Show exposing (..)

import Dict
import Evaluation exposing (..)
import Inference exposing (..)
import LambdaBasics exposing (..)
import StackedSet
import StatefulWithErr as State
import TypeVarContext exposing (Equations, TypeError(..), TypeVarContext, TypeVarStack)
import Value exposing (..)



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

        Delay body ->
            String.concat
                [ "(fn { "
                , showTerm body
                , " })"
                ]

        Force term1 ->
            String.concat
                [ "(@ "
                , showTerm term1
                , ")"
                ]

        Let var exp body ->
            String.concat
                [ "(let "
                , showTerm exp
                , " { "
                , var
                , " . "
                , showTerm body
                , " })"
                ]

        ModuleAccess module0 var ->
            Debug.todo ""


showTermEnvironment : TermEnvironment -> String
showTermEnvironment env =
    env
        |> Dict.toList
        |> List.concatMap
            (\( varname, vals ) ->
                case List.head vals of
                    Just val ->
                        [ String.concat [ varname, " := ", showValue val ] ]

                    Nothing ->
                        []
            )
        |> String.join ", "


showModuleEnvironment : ModuleEnvironment -> String
showModuleEnvironment env =
    env
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, modules ) ->
                case List.head modules of
                    Just moduleValue ->
                        [ String.concat [ moduleName, " := ", showModuleValue moduleValue ] ]

                    Nothing ->
                        []
            )
        |> String.join ", "


showEnvironment : Environment -> String
showEnvironment env =
    String.concat
        [ if not (Dict.isEmpty env.moduleEnv) then
            String.concat
                [ "["
                , showModuleEnvironment env.moduleEnv
                , "]"
                ]

          else
            ""
        , if not (Dict.isEmpty env.termEnv) then
            String.concat
                [ "["
                , showTermEnvironment env.termEnv
                , "]"
                ]

          else
            ""
        ]



-- ===Evaluation Errors===


showEvaluationError : EvalError -> String
showEvaluationError error =
    case error of
        UndefinedVar termVarName ->
            String.concat [ "Use of undefined variable $", termVarName ]

        ExpectedPair ->
            "Expected Pair"

        ExpectedFunction ->
            "Expected Function"

        ExpectedLeftRight ->
            "Expected Left/Right"

        ExpectedBoolean ->
            "Expected Boolean"

        ExpectedNat ->
            "Expected Number"

        ExpectedList ->
            "Expected List"

        FailedToForceThunk thunkId ->
            String.concat [ "Failed to force thunk with id := ", String.fromInt thunkId ]

        ExpectedThunkClosure ->
            "Expected Thunk Closure"


showEvaluationErrors : List EvalError -> String
showEvaluationErrors errors =
    errors
        |> List.map showEvaluationError
        |> String.join ", "



-- ===VALUES===


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
                [ "(fn "
                , showEnvironment env
                , " { "
                , var
                , " . "
                , showTerm body
                , " })"
                ]

        TrueValue ->
            "true"

        FalseValue ->
            "false"

        NatValue natVal ->
            showNatValue natVal

        ListValue listValue ->
            showListValue listValue

        ThunkClosure thunkId ->
            String.concat
                [ "<thunk-id := "
                , String.fromInt thunkId
                , ">"
                ]


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


showThunks : ThunkContext -> String
showThunks { thunks } =
    thunks
        |> Dict.toList
        |> List.map
            (\( thunkId, thunk ) ->
                case thunk of
                    DelayedThunk { env, body } ->
                        String.concat
                            [ "<thunk-id(frozen) := "
                            , String.fromInt thunkId
                            , "; "
                            , showEnvironment env
                            , " | "
                            , showTerm body
                            , ">"
                            ]

                    ForcedThunk val ->
                        String.concat
                            [ "<thunk-id(forced) := "
                            , String.fromInt thunkId
                            , " | "
                            , showValue val
                            , ">"
                            ]
            )
        |> String.join ", "



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

        Frozen type1 ->
            String.concat [ "Frozen(", showType type1, ")" ]

        ForAll typeVar type1 ->
            String.concat [ "Forall ", "'" ++ String.fromInt typeVar, " . ", showType type1 ]



-- ===Term Var Context===


showTermVarContext : TermVarContext -> String
showTermVarContext termVarContext =
    termVarContext
        |> Dict.toList
        |> List.concatMap
            (\( varname, typeStack ) ->
                case List.head typeStack of
                    Just type0 ->
                        [ String.concat [ varname, " := ", showType type0 ] ]

                    Nothing ->
                        []
            )
        |> String.join ", "



-- ===Type Var Context===


showTypeError : TypeError -> String
showTypeError typeError =
    case typeError of
        ExpectedProductType ->
            "Expected Produc Type"

        ExpectedArrowType ->
            "Expected Arrow Type"

        ExpectedNatType ->
            "Expected Nat Type"

        ExpectedSumType ->
            "Expected Sum Type"

        ExpectedMatchingTypesInCaseBranches ->
            "Expected matching Types in Case-Expression Branches"

        ExpectedBoolType ->
            "Expected Bool Type"

        ExpectedMatchingTypesInIfThenElseBranches ->
            "Expected matching Types in If-Then-Else-Expression Branches"

        ExpectedBaseUnifiesWithLoopBodyType ->
            "Expected base unifies with loop-body Type"

        ExpectedListType ->
            "Expected List Type"

        ExpectedFrozenType ->
            "Expected Frozen Type"

        InfiniteType typeVarName ->
            "Infinite Type detected: the type var " ++ "'" ++ String.fromInt typeVarName

        CantPopEmptyTypeVarContext ->
            "Cant Pop Empty Type-Var-Context"


showTypeVarStack : TypeVarStack -> String
showTypeVarStack =
    StackedSet.show String.fromInt


showEquations : Equations -> String
showEquations equations =
    equations
        |> Dict.toList
        |> List.map
            (\( typeVarName, type0 ) ->
                String.concat
                    [ "'" ++ String.fromInt typeVarName
                    , " := "
                    , showType type0
                    ]
            )
        |> String.join "; "


showTypeVarContext : TypeVarContext -> String
showTypeVarContext { nextTypeVar, typeVarStack, equations } =
    String.concat
        [ "["
        , "next-type-var := "
        , "'" ++ String.fromInt nextTypeVar
        , "; "
        , "stack := "
        , StackedSet.show String.fromInt typeVarStack
        , "; "
        , "eq := "
        , "{"
        , showEquations equations
        , "}"
        , "]"
        ]



-- ===INFERENCE===


showInfer0 : Term -> Result (List TypeError) String
showInfer0 term =
    infer0 term
        |> Result.map
            (\( _, _, type1 ) ->
                showType type1
            )



-- This expands the final type according to TypeVarContext


showFinalInfer : Term -> String
showFinalInfer term =
    let
        resultString : Result (List TypeError) String
        resultString =
            infer0 term
                |> Result.andThen
                    (\( termVarContext, typeVarContext, type1 ) ->
                        State.run (TypeVarContext.expandType type1) typeVarContext
                            |> Result.map
                                (\( _, type2 ) ->
                                    showType type2
                                )
                    )
    in
    case resultString of
        Ok str ->
            str

        Err typeErrors ->
            typeErrors
                |> List.map showTypeError
                |> String.join ", "



-- Modules


showModuleTerm : ModuleTerm -> String
showModuleTerm moduleTerm =
    case moduleTerm of
        ModuleLiteralTerm module0 ->
            showModuleLiteral module0

        ModuleUse moduleName ->
            String.concat [ "$", moduleName ]

        FunctorApplication ->
            -- TODO
            Debug.todo ""


showModuleLiteral : ModuleLiteral -> String
showModuleLiteral module0 =
    let
        showModuleLetBinding : ModuleLetBinding -> String
        showModuleLetBinding binding =
            case binding of
                LetTerm var term ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showTerm term
                        , ")"
                        ]

                LetModule var module1 ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showModuleTerm module1
                        , ")"
                        ]

                _ ->
                    Debug.todo ""
    in
    String.concat
        [ "(module "
        , module0.bindings
            |> List.map showModuleLetBinding
            |> String.join " "
        , ")"
        ]


showModuleValue : ModuleValue -> String
showModuleValue moduleValue =
    let
        showModuleValueAssignment : ModuleAssignment -> String
        showModuleValueAssignment binding =
            case binding of
                AssignValue var value ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showValue value
                        , ")"
                        ]

                AssignModuleValue var moduleValue1 ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showModuleValue moduleValue1
                        , ")"
                        ]

                _ ->
                    Debug.todo ""
    in
    String.concat
        [ "(module "
        , moduleValue.assignments
            |> List.map showModuleValueAssignment
            |> String.join " "
        , ")"
        ]
