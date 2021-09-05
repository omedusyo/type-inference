module Main exposing (..)

import AssocList exposing (Dict)


type alias VarName =
    String


type LambdaTerm
    = -- ==Variables==
      VarUse VarName
      -- ==Cartesian Product==
      -- intro
    | Tuple { fst : LambdaTerm, snd : LambdaTerm }
      -- elim
    | Fst LambdaTerm
    | Snd LambdaTerm
      -- ==Function Space==
      -- intro
    | Abstraction { var : VarName, body : LambdaTerm }
      -- elim
    | Application { fn : LambdaTerm, arg : LambdaTerm }
      -- ==Coproduct==
      -- intro
    | Left LambdaTerm
    | Right LambdaTerm
      -- elim
    | Case
        { arg : LambdaTerm
        , leftVar : VarName
        , leftBody : LambdaTerm
        , rightVar : VarName
        , rightBody : LambdaTerm
        }
      -- Booleans
    | BoolTrue
    | BoolFalse
    | IfThenElse
        { arg : LambdaTerm
        , leftBody : LambdaTerm
        , rightBody : LambdaTerm
        }
      --==Natural Number Object==
      -- intro
    | NatZero
    | NatSucc LambdaTerm
      -- elim
      -- f : Nat -> X
      -- f 0 = ....
      -- f (n + 1) = you can use `f n` here
    | NatLoop
        { base : LambdaTerm
        , loop :
            { indexVar : VarName
            , stateVar : VarName -- <- this should be interpreted as `f n`
            , body : LambdaTerm
            }
        , arg : LambdaTerm
        }


type LambdaVal
    = -- ==Cartesian Product==
      TupleVal { fst : LambdaVal, snd : LambdaVal }
      -- ==Function Space==
    | Closure { env : LambdaEnv, var : VarName, body : LambdaTerm }
      -- ==Coproduct==
    | LeftVal LambdaVal
    | RightVal LambdaVal
      -- Booleans
    | TrueVal
    | FalseVal
      --==Natural Number Object==
    | NatVal NatVal


type NatVal
    = NatZeroVal
    | NatSuccVal NatVal


type LambdaType
    = VarType Int
    | Product LambdaType LambdaType
    | Sum LambdaType LambdaType
    | Arrow LambdaType LambdaType
    | LambdaBool
    | LambdaNat
      -- forall alpha (... alpha ...)
    | Forall
        { name : VarName
        , body : LambdaType
        }


type alias LambdaEnv =
    -- we have `List LambdaTerm` because of shadowing of variables
    Dict VarName (List LambdaVal)


emptyEnv =
    AssocList.empty


lookup : VarName -> LambdaEnv -> Maybe LambdaVal
lookup varName env =
    case AssocList.get varName env of
        Just lambdaTerms ->
            case lambdaTerms of
                [] ->
                    Nothing

                lambdaTerm :: _ ->
                    Just lambdaTerm

        Nothing ->
            Nothing


extend : VarName -> LambdaVal -> LambdaEnv -> LambdaEnv
extend varName term env =
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
    | ExpectedTuple
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


eval : LambdaEnv -> LambdaTerm -> Result (List EvalError) LambdaVal
eval env term =
    case term of
        VarUse varName ->
            case lookup varName env of
                Just result ->
                    Ok result

                Nothing ->
                    Err [ UndefinedVar varName ]

        Tuple { fst, snd } ->
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
                    Ok (TupleVal { fst = evaledFst, snd = evaledSnd })

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
                        TupleVal { fst, snd } ->
                            Ok fst

                        _ ->
                            Err [ ExpectedTuple ]

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
                        TupleVal { fst, snd } ->
                            Ok snd

                        _ ->
                            Err [ ExpectedTuple ]

                Err err ->
                    Err err

        Abstraction { var, body } ->
            Ok (Closure { env = env, var = var, body = body })

        Application { fn, arg } ->
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
                                    extend var argEvaled closure.env
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
                    Ok (LeftVal evaledTerm)

                Err err ->
                    Err err

        Right term1 ->
            let
                evaledTermResult =
                    eval env term1
            in
            case evaledTermResult of
                Ok evaledTerm ->
                    Ok (RightVal evaledTerm)

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
                        LeftVal val ->
                            let
                                newEnv =
                                    extend leftVar val env
                            in
                            eval newEnv leftBody

                        RightVal val ->
                            let
                                newEnv =
                                    extend rightVar val env
                            in
                            eval newEnv rightBody

                        _ ->
                            Err [ ExpectedLeftRight ]

                Err errs ->
                    Err errs

        BoolTrue ->
            Ok TrueVal

        BoolFalse ->
            Ok FalseVal

        IfThenElse { arg, leftBody, rightBody } ->
            let
                argEvaledResult =
                    eval env arg
            in
            case argEvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        TrueVal ->
                            eval env leftBody

                        FalseVal ->
                            eval env rightBody

                        _ ->
                            Err [ ExpectedBoolean ]

                Err errs ->
                    Err errs

        NatZero ->
            Ok (NatVal NatZeroVal)

        NatSucc term1 ->
            let
                term1EvaledResult =
                    eval env term1
            in
            case term1EvaledResult of
                Ok argEvaled ->
                    case argEvaled of
                        NatVal natVal ->
                            Ok (NatVal (NatSuccVal natVal))

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
                        NatVal natVal ->
                            let
                                evalNatLoop natVal0 =
                                    case natVal0 of
                                        NatZeroVal ->
                                            eval env base

                                        NatSuccVal natVal1 ->
                                            let
                                                prevResult =
                                                    evalNatLoop natVal1
                                            in
                                            case prevResult of
                                                Ok prevVal ->
                                                    let
                                                        newEnv =
                                                            env
                                                                |> extend loop.indexVar (NatVal natVal1)
                                                                |> extend loop.stateVar prevVal
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


showTerm : LambdaTerm -> String
showTerm term =
    case term of
        VarUse varname ->
            String.concat [ "$", varname ]

        Tuple { fst, snd } ->
            String.concat [ "(", showTerm fst, ", ", showTerm snd, ")" ]

        Fst term1 ->
            String.concat [ showTerm term1, ".0" ]

        Snd term1 ->
            String.concat [ showTerm term1, ".1" ]

        Abstraction { var, body } ->
            String.concat [ "lam(", var, " -> ", showTerm body, ")" ]

        Application { fn, arg } ->
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

        IfThenElse { arg, leftBody, rightBody } ->
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


showType : LambdaType -> String
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

        _ ->
            Debug.todo ""


newTypeVar : Int -> ( Int, LambdaType )
newTypeVar n =
    ( n + 1, VarType n )



-- The list serves as a stack of types
-- - any element in the stack shadows everything below it.


type alias Context =
    Dict VarName (List LambdaType)


popVarFromContext : String -> Context -> Context
popVarFromContext varName context0 =
    AssocList.update varName
        (Maybe.andThen List.tail)
        context0


lookupType : VarName -> Context -> Maybe LambdaType
lookupType varName context0 =
    AssocList.get varName context0
        |> Maybe.andThen List.head


extendContext : VarName -> LambdaType -> Context -> Context
extendContext varName type0 context0 =
    -- TODO: rename to pushVarToContext
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
    Dict Int LambdaType


emptyEqs : Equations
emptyEqs =
    AssocList.empty


lookupTypeVar : Int -> Equations -> Maybe LambdaType
lookupTypeVar =
    AssocList.get


extendEquations : Int -> LambdaType -> Equations -> Equations
extendEquations varname type0 eqs =
    AssocList.insert varname type0 eqs


expandType : LambdaType -> Equations -> LambdaType
expandType type0 eqs0 =
    -- This can loop on itself
    -- TODO: implement cycle detection
    case type0 of
        VarType n ->
            let
                maybeType1 =
                    lookupTypeVar n eqs0
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

        _ ->
            Debug.todo ""


unification : LambdaType -> LambdaType -> Equations -> Maybe ( Equations, LambdaType )
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

        -- TODO: Forall? maybe just don't have Forall?
        _ ->
            Debug.todo ""



-- TODO: abstract
-- type alias State =
--     Int -> Context -> Result (List TypeError) ( Int, LambdaType )


infer : LambdaTerm -> Int -> Context -> Equations -> Result (List TypeError) ( ( Int, Context, Equations ), LambdaType )
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
                    Ok ( ( m, context0 |> extendContext varname typeVar, eqs0 ), typeVar )

        Tuple { fst, snd } ->
            let
                typeFstResult =
                    infer fst n context0 eqs0
            in
            typeFstResult
                |> Result.andThen
                    (\( ( m, context1, eqs1 ), typeFst ) ->
                        let
                            typeSndResult =
                                -- TODO: Should I here use context0 and not context1 here?
                                infer snd m context1 eqs1
                        in
                        typeSndResult
                            |> Result.andThen
                                (\( ( k, context2, eqs2 ), typeSnd ) ->
                                    -- TODO: Should I merge contexts... `context1` and `context2`?
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

        Abstraction { var, body } ->
            -- TODO
            let
                ( m, typeVar ) =
                    newTypeVar n

                typeBodyResult =
                    infer body m (context0 |> extendContext var typeVar) eqs0
            in
            typeBodyResult
                |> Result.andThen
                    -- TODO: q. is this correct? Shou;d I return `context0` here and not the context of `typeBodyResult`?
                    --       a. I should return `typeBodyResult` without the binding `var : typeVar`
                    (\( ( k, context1, eqs1 ), typeBody ) ->
                        let
                            context2 =
                                -- TODO: remove binding `var : typeVar` from `context1`
                                context1 |> popVarFromContext var
                        in
                        Ok ( ( k, context2, eqs1 ), Arrow typeVar typeBody )
                    )

        Application { fn, arg } ->
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
                                                infer leftBody k2 (context1 |> extendContext leftVar leftType) eqs2
                                        in
                                        typeLeftBodyResult
                                            |> Result.andThen
                                                (\( ( k3, context2, eqs3 ), typeLeftBody ) ->
                                                    let
                                                        context3 =
                                                            context2 |> popVarFromContext leftVar

                                                        typeRightBodyResult =
                                                            infer rightBody k3 (context3 |> extendContext rightVar rightType) eqs3
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

        IfThenElse { arg, leftBody, rightBody } ->
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
                                                            |> extendContext loop.indexVar LambdaNat
                                                            |> extendContext loop.stateVar baseType
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


infer0 : LambdaTerm -> Result (List TypeError) ( Equations, LambdaType )
infer0 term =
    infer term 0 emptyEnv emptyEqs
        |> Result.map
            (\( ( _, _, eqs ), type1 ) ->
                ( eqs, type1 )
            )


showInfer0 : LambdaTerm -> Maybe String
showInfer0 term =
    case infer0 term of
        Ok ( _, type1 ) ->
            Just (showType type1)

        Err _ ->
            Nothing



-- This expands the final type according to equations


showFinalInfer : LambdaTerm -> Maybe String
showFinalInfer term =
    case infer0 term of
        Ok ( eqs, type1 ) ->
            Just (showType (expandType type1 eqs))

        Err _ ->
            Nothing


showVal : LambdaVal -> String
showVal val =
    case val of
        TupleVal { fst, snd } ->
            String.concat [ "(", showVal fst, ", ", showVal snd, ")" ]

        LeftVal val1 ->
            String.concat [ "L(", showVal val1, ")" ]

        RightVal val1 ->
            String.concat [ "R(", showVal val1, ")" ]

        Closure { env, var, body } ->
            String.concat
                [ "{"
                , showEnv env
                , if AssocList.isEmpty env then
                    "lam("

                  else
                    " |- lam("
                , var
                , " -> "
                , showTerm body
                , ")}"
                ]

        TrueVal ->
            "True"

        FalseVal ->
            "False"

        NatVal natVal ->
            showNatVal natVal


natValToInt : NatVal -> Int
natValToInt natVal =
    case natVal of
        NatZeroVal ->
            0

        NatSuccVal val1 ->
            1 + natValToInt val1


intToNatTerm : Int -> LambdaTerm
intToNatTerm n =
    if n == 0 then
        NatZero

    else
        NatSucc (intToNatTerm (n - 1))


showNatVal : NatVal -> String
showNatVal natVal =
    -- case natVal of
    --     NatZeroVal ->
    --         "Z"
    --     NatSuccVal val1 ->
    --         String.concat [ "S(", showNatVal val1, ")" ]
    natValToInt natVal
        |> String.fromInt


showEnv : LambdaEnv -> String
showEnv env =
    env
        |> AssocList.toList
        |> List.concatMap
            (\( varname, vals ) ->
                case List.head vals of
                    Just val ->
                        [ String.concat [ varname, " := ", showVal val ] ]

                    Nothing ->
                        []
            )
        |> String.join ", "


showEval : LambdaEnv -> LambdaTerm -> Result (List EvalError) String
showEval env term =
    eval env term
        |> Result.map
            (\val ->
                String.concat [ showTerm term, "  ~~>  ", showVal val ]
            )
