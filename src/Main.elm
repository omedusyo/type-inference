module Main exposing (..)

import AssocList exposing (Dict)


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


type Value
    = -- ==Cartesian Product==
      PairValue Value Value
      -- ==Function Space==
    | Closure { env : TermEnvironment, var : TermVarName, body : Term }
      -- ==Coproduct==
    | LeftValue Value
    | RightValue Value
      -- Booleans
    | TrueValue
    | FalseValue
      --==Natural Number Object==
    | NatValue NatValue


type NatValue
    = NatZeroValue
    | NatSuccValue NatValue


type Type
    = VarType Int
    | Product Type Type
    | Sum Type Type
    | Arrow Type Type
    | LambdaBool
    | LambdaNat


type alias TermEnvironment =
    -- We have `List Value` instead of `Value` because of shadowing of variables
    Dict TermVarName (List Value)


emptyEnvironment =
    AssocList.empty


lookupEnvironment : TermVarName -> TermEnvironment -> Maybe Value
lookupEnvironment varName env =
    case AssocList.get varName env of
        Just terms ->
            case terms of
                [] ->
                    Nothing

                term0 :: _ ->
                    Just term0

        Nothing ->
            Nothing


extendEnvironment : TermVarName -> Value -> TermEnvironment -> TermEnvironment
extendEnvironment varName term env =
    AssocList.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just terms ->
                    Just (term :: terms)

                Nothing ->
                    Just [ term ]
        )
        env


type EvalError
    = UndefinedVar String
    | ExpectedPair
    | ExpectedFunction
    | ExpectedLeftRight
    | ExpectedBoolean
    | ExpectedNat


type TypeError
    = ExpectedProductType
    | ExpectedArrowType
    | ExpectedNatType
    | ExpectedSumType
    | ExpectedMatchingTypesInCaseBranches
    | ExpectedBoolType
    | ExpectedMatchingTypesInIfThenElseBranches
    | ExpectedBaseUnifiesWithLoopBodyType


eval : TermEnvironment -> Term -> Result (List EvalError) Value
eval env term =
    case term of
        VarUse varName ->
            case lookupEnvironment varName env of
                Just result ->
                    Ok result

                Nothing ->
                    Err [ UndefinedVar varName ]

        Pair fst snd ->
            let
                evaledFstResult =
                    eval env fst

                evaledSndResult =
                    eval env snd
            in
            -- TODO: you should evaluate the first component, and if the first component is error, then short circuit and don't eval second component
            -- TODO: I guess there are multiple possibilities...?
            case ( evaledFstResult, evaledSndResult ) of
                ( Ok evaledFst, Ok evaledSnd ) ->
                    Ok (PairValue evaledFst evaledSnd)

                ( Err errFst, Ok evaledSnd ) ->
                    Err errFst

                ( Ok evaledFst, Err errSnd ) ->
                    Err errSnd

                ( Err errFst, Err errSnd ) ->
                    Err (errFst ++ errSnd)

        Fst term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    case evaledTerm of
                        PairValue fst _ ->
                            Ok fst

                        _ ->
                            Err [ ExpectedPair ]

                Err err ->
                    Err err

        Snd term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    case evaledTerm of
                        PairValue _ snd ->
                            Ok snd

                        _ ->
                            Err [ ExpectedPair ]

                Err err ->
                    Err err

        Abstraction var body ->
            Ok (Closure { env = env, var = var, body = body })

        Application fn arg ->
            let
                fnEvaledResult =
                    eval env fn
            in
            case fnEvaledResult of
                Ok (Closure ({ var, body } as closure)) ->
                    let
                        argEvaledResult =
                            eval env arg
                    in
                    case argEvaledResult of
                        Ok argEvaled ->
                            let
                                newEnv =
                                    extendEnvironment var argEvaled closure.env
                            in
                            eval newEnv body

                        Err err ->
                            Err err

                Ok _ ->
                    Err [ ExpectedFunction ]

                Err err ->
                    Err err

        Left term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    Ok (LeftValue evaledTerm)

                Err err ->
                    Err err

        Right term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    Ok (RightValue evaledTerm)

                Err err ->
                    Err err

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        LeftValue val ->
                            let
                                newEnv =
                                    extendEnvironment leftVar val env
                            in
                            eval newEnv leftBody

                        RightValue val ->
                            let
                                newEnv =
                                    extendEnvironment rightVar val env
                            in
                            eval newEnv rightBody

                        _ ->
                            Err [ ExpectedLeftRight ]

                Err errs ->
                    Err errs

        BoolTrue ->
            Ok TrueValue

        BoolFalse ->
            Ok FalseValue

        IfThenElse arg leftBody rightBody ->
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        TrueValue ->
                            eval env leftBody

                        FalseValue ->
                            eval env rightBody

                        _ ->
                            Err [ ExpectedBoolean ]

                Err errs ->
                    Err errs

        NatZero ->
            Ok (NatValue NatZeroValue)

        NatSucc term1 ->
            let
                term1EvaledResult =
                    eval env term1
            in
            case term1EvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        NatValue natVal ->
                            Ok (NatValue (NatSuccValue natVal))

                        _ ->
                            Err [ ExpectedNat ]

                Err errs ->
                    Err errs

        NatLoop { base, loop, arg } ->
            -- TODO: error on same var name? loop.indexVar, loop.stateVar
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        NatValue natVal ->
                            let
                                evalNatLoop natVal0 =
                                    case natVal0 of
                                        NatZeroValue ->
                                            eval env base

                                        NatSuccValue natVal1 ->
                                            let
                                                prevResult =
                                                    evalNatLoop natVal1
                                            in
                                            case prevResult of
                                                Ok prevVal ->
                                                    let
                                                        newEnv =
                                                            env
                                                                |> extendEnvironment loop.indexVar (NatValue natVal1)
                                                                |> extendEnvironment loop.stateVar prevVal
                                                    in
                                                    eval newEnv loop.body

                                                Err errs ->
                                                    Err errs
                            in
                            evalNatLoop natVal

                        _ ->
                            Err [ ExpectedNat ]

                Err errs ->
                    Err errs


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
                [ "loop("
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


newTypeVar : Int -> ( Int, Type )
newTypeVar n =
    ( n + 1, VarType n )



-- The list serves as a stack of types
-- - any element in the stack shadows everything below it.


type alias Context =
    Dict TermVarName (List Type)


popVarFromContext : String -> Context -> Context
popVarFromContext varName context0 =
    AssocList.update varName
        (Maybe.andThen List.tail)
        context0


lookupType : TermVarName -> Context -> Maybe Type
lookupType varName context0 =
    AssocList.get varName context0
        |> Maybe.andThen List.head


pushVarToContext : TermVarName -> Type -> Context -> Context
pushVarToContext varName type0 context0 =
    AssocList.update varName
        (\maybeBinding ->
            case maybeBinding of
                Just types0 ->
                    Just (type0 :: types0)

                Nothing ->
                    Just [ type0 ]
        )
        context0


type alias Equations =
    Dict Int Type


emptyEquations : Equations
emptyEquations =
    AssocList.empty


lookupEquations : Int -> Equations -> Maybe Type
lookupEquations =
    AssocList.get


extendEquations : Int -> Type -> Equations -> Equations
extendEquations varname type0 eqs =
    AssocList.insert varname type0 eqs


expandType : Type -> Equations -> Type
expandType type0 eqs0 =
    -- This can loop on itself
    -- TODO: implement cycle detection
    case type0 of
        VarType n ->
            let
                maybeType1 =
                    lookupEquations n eqs0
            in
            case maybeType1 of
                Just type1 ->
                    expandType type1 eqs0

                Nothing ->
                    VarType n

        Product type1 type2 ->
            Product (expandType type1 eqs0) (expandType type2 eqs0)

        Sum type1 type2 ->
            Sum (expandType type1 eqs0) (expandType type2 eqs0)

        Arrow type1 type2 ->
            Arrow (expandType type1 eqs0) (expandType type2 eqs0)

        LambdaBool ->
            LambdaBool

        LambdaNat ->
            LambdaNat


unification : Type -> Type -> Equations -> Maybe ( Equations, Type )
unification type0Unexpanded type1Unexpanded eqs0 =
    let
        type0 =
            expandType type0Unexpanded eqs0

        type1 =
            expandType type1Unexpanded eqs0
    in
    case ( type0, type1 ) of
        -- ===TYPE VARS===
        ( VarType id0, VarType id1 ) ->
            if id0 == id1 then
                Just ( eqs0, VarType id0 )

            else if id0 < id1 then
                Just ( eqs0 |> extendEquations id0 (VarType id1), VarType id1 )

            else
                Just ( eqs0 |> extendEquations id1 (VarType id0), VarType id1 )

        ( VarType id0, _ ) ->
            Just ( eqs0 |> extendEquations id0 type1, type1 )

        ( _, VarType id1 ) ->
            Just ( eqs0 |> extendEquations id1 type0, type0 )

        -- ===PRODUCT===
        ( Product type00 type01, Product type10 type11 ) ->
            let
                maybeEqs1 =
                    unification type00 type10 eqs0
            in
            maybeEqs1
                |> Maybe.andThen
                    (\( eqs1, typeFst ) ->
                        let
                            maybeEqs2 =
                                unification type01 type11 eqs1
                        in
                        maybeEqs2
                            |> Maybe.andThen
                                (\( eqs2, typeSnd ) ->
                                    Just ( eqs2, Product typeFst typeSnd )
                                )
                    )

        ( Product _ _, _ ) ->
            Nothing

        -- ===ARROW===
        ( Arrow type00 type01, Arrow type10 type11 ) ->
            let
                maybeEqs1 =
                    unification type00 type10 eqs0
            in
            maybeEqs1
                |> Maybe.andThen
                    (\( eqs1, typeFst ) ->
                        let
                            maybeEqs2 =
                                unification type01 type11 eqs1
                        in
                        maybeEqs2
                            |> Maybe.andThen
                                (\( eqs2, typeSnd ) ->
                                    Just ( eqs2, Arrow typeFst typeSnd )
                                )
                    )

        ( Arrow _ _, _ ) ->
            Nothing

        -- ===SUM===
        ( Sum type00 type01, Sum type10 type11 ) ->
            let
                maybeEqs1 =
                    unification type00 type10 eqs0
            in
            maybeEqs1
                |> Maybe.andThen
                    (\( eqs1, typeFst ) ->
                        let
                            maybeEqs2 =
                                unification type01 type11 eqs1
                        in
                        maybeEqs2
                            |> Maybe.andThen
                                (\( eqs2, typeSnd ) ->
                                    Just ( eqs2, Sum typeFst typeSnd )
                                )
                    )

        ( Sum _ _, _ ) ->
            Nothing

        -- ===BOOL===
        ( LambdaBool, LambdaBool ) ->
            Just ( eqs0, LambdaBool )

        ( LambdaBool, _ ) ->
            Nothing

        -- ===NAT===
        ( LambdaNat, LambdaNat ) ->
            Just ( eqs0, LambdaNat )

        ( LambdaNat, _ ) ->
            Nothing



-- TODO: abstract
-- type alias State =
--     Int -> Context -> Result (List TypeError) ( Int, Type )


infer : Term -> Int -> Context -> Equations -> Result (List TypeError) ( ( Int, Context, Equations ), Type )
infer term n context0 eqs0 =
    case term of
        VarUse varname ->
            case lookupType varname context0 of
                Just varType ->
                    Ok ( ( n, context0, eqs0 ), varType )

                Nothing ->
                    let
                        ( m, typeVar ) =
                            newTypeVar n
                    in
                    Ok ( ( m, context0 |> pushVarToContext varname typeVar, eqs0 ), typeVar )

        Pair fst snd ->
            let
                typeFstResult =
                    infer fst n context0 eqs0
            in
            typeFstResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeFst ) ->
                        let
                            typeSndResult =
                                infer snd m context1 eqs1
                        in
                        typeSndResult
                            |> Result.andThen
                                (\( ( k, context2, eqs2 ), typeSnd ) ->
                                    Ok ( ( k, context2, eqs2 ), Product typeFst typeSnd )
                                )
                    )

        Fst productExp ->
            let
                typeExpResult =
                    infer productExp n context0 eqs0
            in
            typeExpResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeProduct ) ->
                        let
                            ( k1, fstTypeVar ) =
                                newTypeVar m

                            ( k2, sndTypeVar ) =
                                newTypeVar k1

                            maybeEqs2 =
                                eqs1
                                    |> unification typeProduct (Product fstTypeVar sndTypeVar)
                        in
                        case maybeEqs2 of
                            Just ( eqs2, typeProduct2 ) ->
                                case typeProduct2 of
                                    Product fstTypeVar2 _ ->
                                        Ok ( ( k2, context1, eqs2 ), fstTypeVar2 )

                                    _ ->
                                        Err [ ExpectedProductType ]

                            Nothing ->
                                Err [ ExpectedProductType ]
                    )

        Snd productExp ->
            let
                typeExpResult =
                    infer productExp n context0 eqs0
            in
            typeExpResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeProduct ) ->
                        let
                            ( k1, fstTypeVar ) =
                                newTypeVar m

                            ( k2, sndTypeVar ) =
                                newTypeVar k1

                            maybeEqs2 =
                                eqs1
                                    |> unification typeProduct (Product fstTypeVar sndTypeVar)
                        in
                        case maybeEqs2 of
                            Just ( eqs2, typeProduct2 ) ->
                                case typeProduct2 of
                                    Product _ sndTypeVar2 ->
                                        Ok ( ( k2, context1, eqs2 ), sndTypeVar )

                                    _ ->
                                        Err [ ExpectedProductType ]

                            Nothing ->
                                Err [ ExpectedProductType ]
                    )

        Abstraction var body ->
            let
                ( m, typeVar ) =
                    newTypeVar n

                typeBodyResult =
                    infer body m (context0 |> pushVarToContext var typeVar) eqs0
            in
            typeBodyResult
                |> Result.andThen
                    (\( ( k, context1, eqs1 ), typeBody ) ->
                        let
                            context2 =
                                context1 |> popVarFromContext var
                        in
                        Ok ( ( k, context2, eqs1 ), Arrow typeVar typeBody )
                    )

        Application fn arg ->
            let
                typeFnResult =
                    infer fn n context0 eqs0
            in
            typeFnResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeFn ) ->
                        let
                            typeArgResult =
                                infer arg m context1 eqs1
                        in
                        typeArgResult
                            |> Result.andThen
                                (\( ( k, context2, eqs2 ), typeArg ) ->
                                    let
                                        ( k1, resultTypeVar ) =
                                            newTypeVar k

                                        maybeEqs3 =
                                            eqs2
                                                |> unification typeFn (Arrow typeArg resultTypeVar)
                                    in
                                    case maybeEqs3 of
                                        Just ( eqs3, typeFn3 ) ->
                                            case typeFn3 of
                                                Arrow _ resultTypeVar3 ->
                                                    Ok ( ( k1, context2, eqs3 ), resultTypeVar )

                                                _ ->
                                                    Err [ ExpectedArrowType ]

                                        Nothing ->
                                            Err [ ExpectedArrowType ]
                                )
                    )

        Left leftTerm ->
            let
                typeLeftTermResult =
                    infer leftTerm n context0 eqs0
            in
            typeLeftTermResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeLeftTerm ) ->
                        let
                            ( k, typeRightTerm ) =
                                newTypeVar m
                        in
                        Ok ( ( k, context1, eqs1 ), Sum typeLeftTerm typeRightTerm )
                    )

        Right rightTerm ->
            let
                typeRightTermResult =
                    infer rightTerm n context0 eqs0
            in
            typeRightTermResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeRightTerm ) ->
                        let
                            ( k, typeLeftTerm ) =
                                newTypeVar m
                        in
                        Ok ( ( k, context1, eqs1 ), Sum typeLeftTerm typeRightTerm )
                    )

        Case { arg, leftVar, leftBody, rightVar, rightBody } ->
            let
                typeArgResult =
                    infer arg n context0 eqs0
            in
            typeArgResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeArg ) ->
                        let
                            ( k1, leftTypeVar ) =
                                newTypeVar m

                            ( k2, rightTypeVar ) =
                                newTypeVar k1

                            maybeEqs2 =
                                eqs1 |> unification (Sum leftTypeVar rightTypeVar) typeArg
                        in
                        case maybeEqs2 of
                            Just ( eqs2, sumType ) ->
                                case sumType of
                                    Sum leftType rightType ->
                                        let
                                            typeLeftBodyResult =
                                                infer leftBody k2 (context1 |> pushVarToContext leftVar leftType) eqs2
                                        in
                                        typeLeftBodyResult
                                            |> Result.andThen
                                                (\( ( k3, context2, eqs3 ), typeLeftBody ) ->
                                                    let
                                                        context3 =
                                                            context2 |> popVarFromContext leftVar

                                                        typeRightBodyResult =
                                                            infer rightBody k3 (context3 |> pushVarToContext rightVar rightType) eqs3
                                                    in
                                                    typeRightBodyResult
                                                        |> Result.andThen
                                                            (\( ( k4, context4, eqs4 ), typeRightBody ) ->
                                                                let
                                                                    context5 =
                                                                        context4 |> popVarFromContext rightVar

                                                                    maybeEqs5 =
                                                                        eqs4 |> unification typeLeftBody typeRightBody
                                                                in
                                                                case maybeEqs5 of
                                                                    Just ( eqs5, typeResult ) ->
                                                                        Ok ( ( k4, context5, eqs5 ), typeResult )

                                                                    Nothing ->
                                                                        Err [ ExpectedMatchingTypesInCaseBranches ]
                                                            )
                                                )

                                    _ ->
                                        Err [ ExpectedSumType ]

                            Nothing ->
                                Err [ ExpectedSumType ]
                    )

        BoolTrue ->
            Ok ( ( n, context0, eqs0 ), LambdaBool )

        BoolFalse ->
            Ok ( ( n, context0, eqs0 ), LambdaBool )

        IfThenElse arg leftBody rightBody ->
            let
                typeArgResult =
                    infer arg n context0 eqs0
            in
            typeArgResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeArg ) ->
                        let
                            maybeEqs2 =
                                eqs1 |> unification LambdaBool typeArg
                        in
                        case maybeEqs2 of
                            Just ( eqs2, _ ) ->
                                let
                                    typeLeftBodyResult =
                                        infer leftBody m context1 eqs2
                                in
                                typeLeftBodyResult
                                    |> Result.andThen
                                        (\( ( k1, context2, eqs3 ), typeLeftBody ) ->
                                            let
                                                typeRightBodyResult =
                                                    infer rightBody k1 context2 eqs3
                                            in
                                            typeRightBodyResult
                                                |> Result.andThen
                                                    (\( ( k2, context3, eqs4 ), typeRightBody ) ->
                                                        let
                                                            maybeEqs5 =
                                                                eqs4 |> unification typeLeftBody typeRightBody
                                                        in
                                                        case maybeEqs5 of
                                                            Just ( eqs5, typeResult ) ->
                                                                Ok ( ( k2, context3, eqs5 ), typeResult )

                                                            Nothing ->
                                                                Err [ ExpectedMatchingTypesInIfThenElseBranches ]
                                                    )
                                        )

                            Nothing ->
                                Err [ ExpectedBoolType ]
                    )

        NatZero ->
            Ok ( ( n, context0, eqs0 ), LambdaNat )

        NatSucc term1 ->
            let
                typeTerm1Result =
                    infer term1 n context0 eqs0
            in
            typeTerm1Result
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), type1 ) ->
                        let
                            maybeEqs2 =
                                eqs1 |> unification LambdaNat type1
                        in
                        case maybeEqs2 of
                            Just ( eqs2, _ ) ->
                                Ok ( ( m, context1, eqs2 ), LambdaNat )

                            Nothing ->
                                Err [ ExpectedNatType ]
                    )

        NatLoop { base, loop, arg } ->
            let
                baseTypeResult =
                    infer base n context0 eqs0
            in
            baseTypeResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), baseType ) ->
                        let
                            argTypeResult =
                                infer arg m context1 eqs1
                        in
                        argTypeResult
                            |> Result.andThen
                                (\( ( k1, context2, eqs2 ), argType ) ->
                                    let
                                        maybeEqs3 =
                                            eqs2 |> unification argType LambdaNat
                                    in
                                    case maybeEqs3 of
                                        Just ( eqs3, _ ) ->
                                            let
                                                typeLoopBodyResult =
                                                    infer loop.body
                                                        k1
                                                        (context2
                                                            |> pushVarToContext loop.indexVar LambdaNat
                                                            |> pushVarToContext loop.stateVar baseType
                                                        )
                                                        eqs3
                                            in
                                            typeLoopBodyResult
                                                |> Result.andThen
                                                    (\( ( k2, context3, eqs4 ), typeLoopBody ) ->
                                                        let
                                                            context4 =
                                                                context3
                                                                    |> popVarFromContext loop.stateVar
                                                                    |> popVarFromContext loop.indexVar

                                                            maybeEqs5 =
                                                                eqs4 |> unification typeLoopBody baseType
                                                        in
                                                        case maybeEqs5 of
                                                            Just ( eqs5, stateType ) ->
                                                                Ok ( ( k2, context4, eqs5 ), stateType )

                                                            Nothing ->
                                                                Err [ ExpectedBaseUnifiesWithLoopBodyType ]
                                                    )

                                        Nothing ->
                                            Err [ ExpectedNatType ]
                                )
                    )


infer0 : Term -> Result (List TypeError) ( Equations, Type )
infer0 term =
    infer term 0 emptyEnvironment emptyEquations
        |> Result.map
            (\( ( _, _, eqs ), type1 ) ->
                ( eqs, type1 )
            )


showInfer0 : Term -> Maybe String
showInfer0 term =
    case infer0 term of
        Ok ( _, type1 ) ->
            Just (showType type1)

        Err _ ->
            Nothing



-- This expands the final type according to equations


showFinalInfer : Term -> Maybe String
showFinalInfer term =
    case infer0 term of
        Ok ( eqs, type1 ) ->
            Just (showType (expandType type1 eqs))

        Err _ ->
            Nothing


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
