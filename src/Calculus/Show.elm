module Calculus.Show exposing (..)

import Calculus.Base as Base
    exposing
        ( FunctorTerm
        , FunctorType
        , Interface
        , InterfaceAssumption
        , ModuleLetBinding
        , ModuleLiteral
        , ModuleTerm
        , Term
        , Type
        )
import Calculus.Evaluation.Evaluation as Evaluation exposing (EvalError, ThunkContext)
import Calculus.Evaluation.Value as Value
    exposing
        ( Environment
        , ListValue
        , ModuleAssignment
        , ModuleEnvironment
        , ModuleValue
        , NatValue
        , TermEnvironment
        , Value
        )
import Calculus.Type.Inference as TypeInference exposing (TermVarContext)
import Calculus.Type.TypeVarContext as TypeVarContext exposing (Equations, TypeError, TypeVarContext, TypeVarStack)
import Dict
import Lib.StackedSet as StackedSet
import Lib.State.StatefulWithErr as State



-- ===TERMS===


showTerm : Term -> String
showTerm term =
    case term of
        Base.VarUse varname ->
            -- $foo
            String.concat [ "$", varname ]

        Base.Pair fst snd ->
            -- (pair e1 e2)
            String.concat [ "(pair ", showTerm fst, " ", showTerm snd, ")" ]

        Base.MatchPair arg { var0, var1, body } ->
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

        Base.Abstraction { var, body } ->
            -- (fn { x . body })
            String.concat [ "(fn { ", var, " . ", showTerm body, " })" ]

        Base.Application fn arg ->
            -- (@ e1 e2)
            String.concat [ "(@ ", showTerm fn, " ", showTerm arg, ")" ]

        Base.Left term1 ->
            -- (left e)
            String.concat [ "(left ", showTerm term1, ")" ]

        Base.Right term1 ->
            -- (right e)
            String.concat [ "(right ", showTerm term1, ")" ]

        Base.MatchSum arg { leftBranch, rightBranch } ->
            -- (match-sum e { (left x) . e1 } { (right y) . e2 })
            String.concat
                [ "(match-sum "
                , showTerm arg
                , " { (left "
                , leftBranch.var
                , ") . "
                , showTerm leftBranch.body
                , " } { (right "
                , rightBranch.var
                , ") . "
                , showTerm rightBranch.body
                , " })"
                ]

        Base.ConstTrue ->
            "true"

        Base.ConstFalse ->
            "false"

        Base.MatchBool arg { trueBranch, falseBranch } ->
            -- (if { x } { y })
            String.concat
                [ "(if "
                , showTerm arg
                , " { "
                , showTerm trueBranch.body
                , " } { "
                , showTerm falseBranch.body
                , " })"
                ]

        Base.ConstZero ->
            "0"

        Base.Succ term1 ->
            -- (succ e)
            String.concat [ "(succ ", showTerm term1, ")" ]

        Base.FoldNat arg { zeroBranch, succBranch } ->
            -- (loop-nat nExp initStateExp { i state . body })
            String.concat
                [ "(loop-nat "
                , showTerm arg
                , " "
                , showTerm zeroBranch.body
                , " { "
                , succBranch.var
                , " . "
                , showTerm succBranch.body
                , " })"
                ]

        Base.ConstEmpty ->
            "empty-list"

        Base.Cons headTerm tailTerm ->
            -- (cons e1 e2)
            String.concat
                [ "(cons "
                , showTerm headTerm
                , " "
                , showTerm tailTerm
                , ")"
                ]

        Base.FoldList arg { emptyBranch, consBranch } ->
            -- (loop-list xsExp initStateExp { x state . body } )
            String.concat
                [ "(list-loop "
                , showTerm arg
                , " "
                , showTerm emptyBranch.body
                , " { "
                , consBranch.var0
                , " "
                , consBranch.var1
                , " . "
                , showTerm consBranch.body
                , " })"
                ]

        Base.Delay { body } ->
            String.concat
                [ "(fn { "
                , showTerm body
                , " })"
                ]

        Base.Force term1 ->
            String.concat
                [ "(@ "
                , showTerm term1
                , ")"
                ]

        Base.LetBe exp { var, body } ->
            String.concat
                [ "(let "
                , showTerm exp
                , " { "
                , var
                , " . "
                , showTerm body
                , " })"
                ]

        Base.ModuleAccess module0 field ->
            String.concat
                [ "(-> "
                , showModuleTerm module0
                , " "
                , field
                , ")"
                ]


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
        Evaluation.UndefinedVar termVarName ->
            String.concat [ "Use of undefined variable $", termVarName ]

        Evaluation.UndefinedModule moduleVarName ->
            String.concat [ "Use of undefined module variable $", moduleVarName ]

        Evaluation.UndefinedFunctor functorName ->
            String.concat [ "Use of undefined functor variable $", functorName ]

        Evaluation.ExpectedPair ->
            "Expected Pair"

        Evaluation.ExpectedFunction ->
            "Expected Function"

        Evaluation.ExpectedLeftRight ->
            "Expected Left/Right"

        Evaluation.ExpectedBoolean ->
            "Expected Boolean"

        Evaluation.ExpectedNat ->
            "Expected Number"

        Evaluation.ExpectedList ->
            "Expected List"

        Evaluation.FailedToForceThunk thunkId ->
            String.concat [ "Failed to force thunk with id := ", String.fromInt thunkId ]

        Evaluation.ExpectedThunkClosure ->
            "Expected Thunk Closure"

        Evaluation.UnknownModuleField field ->
            String.concat [ "Unknown module-field access := ", field ]

        Evaluation.FunctorApplicationNumberOfModuleParametersShouldBeEqualToNumberOfArguments ->
            "Functor Application Error: Number of parameters is not equal to the number of arguments"


showEvaluationErrors : List EvalError -> String
showEvaluationErrors errors =
    errors
        |> List.map showEvaluationError
        |> String.join ", "



-- ===VALUES===


showValue : Value -> String
showValue val =
    case val of
        Value.Pair fst snd ->
            String.concat [ "(pair ", showValue fst, " ", showValue snd, ")" ]

        Value.Left val1 ->
            String.concat [ "(left ", showValue val1, ")" ]

        Value.Right val1 ->
            String.concat [ "(right ", showValue val1, ")" ]

        Value.Closure { env, var, body } ->
            String.concat
                [ "(fn "

                -- TODO: Expose this when you want to see the whole environment, but for even small programs this is too big.
                -- , showEnvironment env
                , "[...]"
                , " { "
                , var
                , " . "
                , showTerm body
                , " })"
                ]

        Value.ConstTrue ->
            "true"

        Value.ConstFalse ->
            "false"

        Value.NatValue natVal ->
            showNatValue natVal

        Value.ListValue listValue ->
            showListValue listValue

        Value.ThunkClosure thunkId ->
            String.concat
                [ "<thunk-id := "
                , String.fromInt thunkId
                , ">"
                ]


natValToInt : NatValue -> Int
natValToInt natVal =
    case natVal of
        Value.ConstZero ->
            0

        Value.Succ val1 ->
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
        Value.ConstEmpty ->
            "empty-list"

        Value.Cons headValue tailValue ->
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
                    Value.DelayedThunk { env, body } ->
                        String.concat
                            [ "<thunk-id(frozen) := "
                            , String.fromInt thunkId
                            , "; "
                            , showEnvironment env
                            , " | "
                            , showTerm body
                            , ">"
                            ]

                    Value.ForcedThunk val ->
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
        Base.TypeVarUse n ->
            String.concat [ "$", n ]

        Base.Product type1 type2 ->
            String.concat
                [ "(", showType type1, " , ", showType type2, ")" ]

        Base.Sum type1 type2 ->
            String.concat
                [ "[", showType type1, " + ", showType type2, "]" ]

        Base.Arrow type1 type2 ->
            String.concat
                [ "(", showType type1, " -> ", showType type2, ")" ]

        Base.ConstBool ->
            "Bool"

        Base.ConstNat ->
            "Nat"

        Base.List type1 ->
            String.concat [ "List(", showType type1, ")" ]

        Base.Frozen type1 ->
            String.concat [ "Frozen(", showType type1, ")" ]

        Base.ForAll typeVar type1 ->
            String.concat [ "Forall {", typeVar, " . ", showType type1, "}" ]



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
        TypeVarContext.ExpectedProductType ->
            "Expected Produc Type"

        TypeVarContext.ExpectedArrowType ->
            "Expected Arrow Type"

        TypeVarContext.ExpectedNatType ->
            "Expected Nat Type"

        TypeVarContext.ExpectedSumType ->
            "Expected Sum Type"

        TypeVarContext.ExpectedMatchingTypesInCaseBranches ->
            "Expected matching Types in Case-Expression Branches"

        TypeVarContext.ExpectedBoolType ->
            "Expected Bool Type"

        TypeVarContext.ExpectedMatchingTypesInIfThenElseBranches ->
            "Expected matching Types in If-Then-Else-Expression Branches"

        TypeVarContext.ExpectedBaseUnifiesWithLoopBodyType ->
            "Expected base unifies with loop-body Type"

        TypeVarContext.ExpectedListType ->
            "Expected List Type"

        TypeVarContext.ExpectedFrozenType ->
            "Expected Frozen Type"

        TypeVarContext.InfiniteType typeVarName ->
            String.concat [ "Infinite Type detected: the type var ", "`", typeVarName, "`" ]

        TypeVarContext.CantPopEmptyTypeVarContext ->
            "Cant Pop Empty Type-Var-Context"


showTypeVarStack : TypeVarStack -> String
showTypeVarStack =
    StackedSet.show identity


showEquations : Equations -> String
showEquations equations =
    equations
        |> Dict.toList
        |> List.map
            (\( typeVarName, type0 ) ->
                String.concat
                    [ typeVarName
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
        , StackedSet.show identity typeVarStack
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
    TypeInference.infer0 term
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
            TypeInference.infer0 term
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
        Base.ModuleLiteralTerm module0 ->
            showModuleLiteral module0

        Base.ModuleVarUse moduleName ->
            String.concat [ "$", moduleName ]

        Base.FunctorApplication functorTerm modules ->
            String.concat
                [ "(@ "
                , showFunctorTerm functorTerm
                , " "
                , modules
                    |> List.map showModuleTerm
                    |> String.join " "
                , ")"
                ]


showFunctorTerm : FunctorTerm -> String
showFunctorTerm functorTerm =
    case functorTerm of
        Base.FunctorVarUse functorName ->
            String.concat [ "$", functorName ]

        Base.FunctorLiteralTerm ({ parameters, body } as functorLiteral) ->
            String.concat
                [ "(functor { "
                , parameters
                    |> List.map
                        (\( moduleName, interface ) ->
                            String.concat
                                [ "(: "
                                , moduleName
                                , " "
                                , showInterface interface
                                , ")"
                                ]
                        )
                    |> String.join " "
                , " . "
                , showModuleTerm body
                , " })"
                ]


showInterface : Interface -> String
showInterface ({ assumptions } as interface) =
    String.concat
        [ "(interface "
        , assumptions
            |> List.map showInterfaceAssumption
            |> String.join " "
        , ")"
        ]


showInterfaceAssumption : InterfaceAssumption -> String
showInterfaceAssumption assumption =
    case assumption of
        Base.AssumeTerm termVarName type0 ->
            String.concat
                [ "(assume-term "
                , termVarName
                , " "
                , showType type0
                , ")"
                ]

        Base.AssumeType typeVarName ->
            String.concat
                [ "(assume-type "
                , typeVarName
                , ")"
                ]

        Base.AssumeModule moduleVarName interface ->
            String.concat
                [ "(assume-module "
                , moduleVarName
                , " "
                , showInterface interface
                , ")"
                ]

        Base.AssumeFunctor functorName functorType ->
            String.concat
                [ "(assume-functor "
                , functorName
                , " "
                , showFunctorType functorType
                , ")"
                ]


showFunctorType : FunctorType -> String
showFunctorType ( inputInterfaces, outputInterface ) =
    String.concat
        [ "(-> ["
        , inputInterfaces
            |> List.map showInterface
            |> String.join " "
        , "] "
        , showInterface outputInterface
        , ")"
        ]


showModuleLiteral : ModuleLiteral -> String
showModuleLiteral module0 =
    let
        showModuleLetBinding : ModuleLetBinding -> String
        showModuleLetBinding binding =
            case binding of
                Base.LetTerm var term ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showTerm term
                        , ")"
                        ]

                Base.LetModule var module1 ->
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
                Value.AssignValue var value ->
                    String.concat
                        [ "("
                        , var
                        , " "
                        , showValue value
                        , ")"
                        ]

                Value.AssignModuleValue var moduleValue1 ->
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
