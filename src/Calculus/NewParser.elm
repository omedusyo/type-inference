module Calculus.NewParser exposing
    ( atleastOneOpenParens
    , closingParens
    , mandatoryParens
    , nat
    , operatorKeyword
    , optionalParens
    , spaces
    , symbol
    , term
    , varIntro
    , whitespaceChars
    , zeroOrMoreOpenParens
    )

import Calculus.Base as Base
    exposing
        ( FunctorLiteral
        , FunctorTerm
        , FunctorVarName
        , Interface
        , InterfaceAssumption
        , ModuleLetBinding
        , ModuleLiteral
        , ModuleTerm
        , ModuleVarName
        , Term
        , TermVarName
        , Type
        , TypeVarName
        )
import Dict exposing (Dict)
import Either exposing (Either)
import Lib.Parser.Error as PError
import Lib.Parser.Parser as Parser
import Lib.Parser.State as PState
import Set exposing (Set)



-- ===Errors===


type ExpectedKeywordGapCharacter
    = ExpectedKeywordGapCharacter { failedAtChar : Char }


type ExpectedOperatorKeyword
    = ExpectedOperatorKeyword { consumedSuccessfully : String, failedAtChar : Maybe Char }
    | ExpectedGapAfterOperatorKeyword { operatorKeyword : OperatorKeyword, failedAtChar : Char }


type ExpectedIdentifierIntroduction
    = ExpectedIdentifierCharacters { failedAtChar : Maybe Char }
    | ExpectedIdentifierToStartWithNonDigit { failedAtChar : Char }


type ExpectedBindingTerm
    = ExpectedOpenBraces { failedAtChar : Maybe Char }
    | ExpectedDot { failedAtChar : Maybe Char }
    | ExpectedClosingBraces { failedAtChar : Maybe Char }
    | ExpectedDefEquals { failedAtChar : Maybe Char }
    | ExpectedSemicolon { failedAtChar : Maybe Char }


type ExpectedParens
    = ExpectedOpenParens { failedAtChar : Maybe Char }
    | ExpectedClosingParens { failedAtChar : Maybe Char }


type ExpectedPattern
    = ExpectedPatternKeyword { patternKeyword : OperatorKeyword, consumedSuccessfully : String, failedAtChar : Maybe Char }
    | ExpectedGapAfterPatternKeyword { patternKeyword : OperatorKeyword, failedAtChar : Char }


type ExpectedTerm
    = ExpectedOperator ExpectedOperatorKeyword
    | ExpectedIdentifier ExpectedIdentifierIntroduction
    | ExpectedParens ExpectedParens
    | ExpectedBindingTerm ExpectedBindingTerm
    | ExpectedPattern ExpectedPattern
    | ExpectedAtleastTwoArgumentsToApplication { got : Int }
    | ExpectedAtleastOneParameterToAbstraction { got : Int }
    | ExpectedNatConstant


whitespaceChars : Set Char
whitespaceChars =
    -- \u{000D} === \r
    Set.fromList [ ' ', '\n', '\u{000D}', '\t', ',' ]


isWhitespaceChar : Char -> Bool
isWhitespaceChar c =
    Set.member c whitespaceChars



-- the set of characters that's used to check for gaps after the keyword


gapChars : Set Char
gapChars =
    -- TODO: maybe I should consider characters like `@` or `\\`? Or will I allow
    -- TODO: This should actually be strings because I want to include "<-" and "->". Right now use symbols `>` and `<`. Damn it... but I want `<` and `>` for record angle parens.
    --         gapSymbols...
    --       But I also want to have dashes in variable names...
    Set.union whitespaceChars (Set.fromList [ '(', ')', '{', '}', '.', '$', '\'', '"', '%', '/', '=', ';' ])


isGapChar : Char -> Bool
isGapChar c =
    Set.member c gapChars



-- ===Types===


type alias Parser e a =
    Parser.Parser {} e a



-- ===nat===


nat : Parser () Int
nat =
    let
        -- sends a char digit to its corresponding int
        charToInt : Char -> Int
        charToInt c =
            -- 48 == Char.toCode '0'
            Char.toCode c - 48

        digitStringToInt : String -> Int
        digitStringToInt str =
            String.foldr
                (\c ( x, exponent ) ->
                    ( 10 ^ exponent * charToInt c + x, exponent + 1 )
                )
                ( 0, 0 )
                str
                |> Tuple.first
    in
    -- Fails IFF the input doesn't start with a digit
    Parser.anyCharSatisfying Char.isDigit
        |> Parser.andThen
            (\c ->
                if c == '0' then
                    Parser.second
                        (Parser.allWhileTrue (\d -> d == '0'))
                        (Parser.allWhileTrue Char.isDigit)

                else
                    Parser.allWhileTrue Char.isDigit
                        -- this is a bit inefficient
                        |> Parser.map (\digits -> digits ++ String.fromChar c)
            )
        |> Parser.map digitStringToInt
        |> Parser.mapError (\_ -> ())



-- ===Basics===


spaces : Parser e ()
spaces =
    Parser.allWhileTrue isWhitespaceChar
        |> Parser.discard


symbol : String -> Parser PState.ExpectedString ()
symbol symbol0 =
    Parser.unit
        |> Parser.o (Parser.string symbol0)
        |> Parser.o spaces



-- Doesn't consume anything, but may fail


keywordGap : Parser ExpectedKeywordGapCharacter ()
keywordGap =
    Parser.check (Parser.anyCharSatisfying isGapChar)
        |> Parser.ifError
            (\error ->
                case error of
                    PState.CharFailedTest { failedAtChar } ->
                        case failedAtChar of
                            Just c ->
                                Parser.fail (ExpectedKeywordGapCharacter { failedAtChar = c })

                            Nothing ->
                                Parser.unit
            )
        |> Parser.o spaces


keyword : String -> Parser (Either PState.ExpectedString ExpectedKeywordGapCharacter) ()
keyword string0 =
    Parser.unit
        |> Parser.o (Parser.string string0 |> Parser.mapError Either.Left)
        |> Parser.o (keywordGap |> Parser.mapError Either.Right)



-- ===Parentheses===


zeroOrMoreOpenParens : Parser e Int
zeroOrMoreOpenParens =
    Parser.loopChars 0
        (\c numOfOpenParens ->
            if isWhitespaceChar c then
                Parser.NextChar numOfOpenParens

            else if c == '(' then
                Parser.NextChar (numOfOpenParens + 1)

            else
                Parser.DoneAndRevertChar numOfOpenParens
        )
        Ok


atleastOneOpenParens : Parser ExpectedParens Int
atleastOneOpenParens =
    let
        handleFirstOpenParen : PState.ExpectedString -> ExpectedParens
        handleFirstOpenParen msg =
            case msg of
                PState.ExpectedString { expected, consumedSuccessfully, failedAtChar } ->
                    ExpectedOpenParens { failedAtChar = failedAtChar }
    in
    Parser.identity
        |> Parser.o (Parser.string "(" |> Parser.mapError handleFirstOpenParen)
        |> Parser.o spaces
        |> Parser.ooo zeroOrMoreOpenParens
        |> Parser.map (\numOfOpenParens -> numOfOpenParens + 1)


closingParens : Int -> Parser ExpectedParens ()
closingParens numOfExpectedClosingParens =
    let
        handleFirstClosingParen : PState.ExpectedString -> ExpectedParens
        handleFirstClosingParen msg =
            case msg of
                PState.ExpectedString { expected, consumedSuccessfully, failedAtChar } ->
                    ExpectedClosingParens { failedAtChar = failedAtChar }
    in
    if numOfExpectedClosingParens == 0 then
        spaces

    else
        -- We are starting with a single ")" because we want to fail on strings that start with whitespace e.g. "  )))"
        (Parser.string ")" |> Parser.mapError handleFirstClosingParen)
            |> Parser.o
                (Parser.loopChars 1
                    (\c numOfClosingParens ->
                        if numOfClosingParens >= numOfExpectedClosingParens then
                            Parser.DoneAndRevertChar numOfExpectedClosingParens

                        else if isWhitespaceChar c then
                            Parser.NextChar numOfClosingParens

                        else if c == ')' then
                            Parser.NextChar (numOfClosingParens + 1)

                        else
                            -- Note that here `numOfClosingParens < numOfExpectedClosingParens)
                            Parser.Fail (ExpectedClosingParens { failedAtChar = Just c })
                    )
                    (\numOfClosingParens ->
                        if numOfClosingParens >= numOfExpectedClosingParens then
                            Ok numOfClosingParens

                        else
                            Err (ExpectedClosingParens { failedAtChar = Nothing })
                    )
                )
            |> Parser.o spaces


optionalParens : Parser e a -> Parser (Either ExpectedParens e) a
optionalParens parser =
    zeroOrMoreOpenParens
        |> Parser.andThen
            (\numOfOpenParens ->
                parser
                    |> Parser.mapError Either.Right
                    |> Parser.o (closingParens numOfOpenParens |> Parser.mapError Either.Left)
            )


mandatoryParens : Parser e a -> Parser (Either ExpectedParens e) a
mandatoryParens parser =
    (atleastOneOpenParens |> Parser.mapError Either.Left)
        |> Parser.andThen
            (\numOfOpenParens ->
                parser
                    |> Parser.mapError Either.Right
                    |> Parser.o (closingParens numOfOpenParens |> Parser.mapError Either.Left)
            )



-- ===Sequences===


emptySequence : Parser e ()
emptySequence =
    spaces



-- ===VAR===


varIntro : Parser ExpectedIdentifierIntroduction TermVarName
varIntro =
    -- TODO: You can make the error messages better
    let
        excludedChars : Set Char
        excludedChars =
            Set.union
                (Set.fromList [ '$', '.', '(', ')', '{', '}', '\'', '"', '/', '%', '=', ';' ])
                whitespaceChars

        isExcludedChar : Char -> Bool
        isExcludedChar c =
            Set.member c excludedChars

        isPrintableChar : Char -> Bool
        isPrintableChar c =
            let
                charCode =
                    Char.toCode c
            in
            32 <= charCode && charCode <= 126

        isInnerVarChar : Char -> Bool
        isInnerVarChar c =
            not (isExcludedChar c) && isPrintableChar c

        handleCharError : PState.CharFailedTest -> ExpectedIdentifierIntroduction
        handleCharError msg =
            case msg of
                PState.CharFailedTest { failedAtChar } ->
                    ExpectedIdentifierCharacters { failedAtChar = failedAtChar }
    in
    Parser.anyCharSatisfying isInnerVarChar
        |> Parser.mapError handleCharError
        |> Parser.andThen
            (\c ->
                if Char.isDigit c then
                    Parser.fail (ExpectedIdentifierToStartWithNonDigit { failedAtChar = c })

                else
                    Parser.allWhileTrue isInnerVarChar
                        |> Parser.map (\str -> String.cons c str)
            )
        |> Parser.o spaces



-- ===Binding===


binding : Parser ExpectedTerm a -> Parser ExpectedTerm b -> Parser ExpectedTerm ( a, b )
binding patternParser bodyParser =
    let
        handleOpenBraces : PState.ExpectedString -> ExpectedTerm
        handleOpenBraces msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedBindingTerm (ExpectedOpenBraces { failedAtChar = failedAtChar })

        handleDot : PState.ExpectedString -> ExpectedTerm
        handleDot msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedBindingTerm (ExpectedDot { failedAtChar = failedAtChar })

        handleClosingBraces : PState.ExpectedString -> ExpectedTerm
        handleClosingBraces msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedBindingTerm (ExpectedClosingBraces { failedAtChar = failedAtChar })
    in
    Parser.return (\vars body -> ( vars, body ))
        |> Parser.o (symbol "{" |> Parser.mapError handleOpenBraces)
        |> Parser.ooo patternParser
        |> Parser.o (symbol "." |> Parser.mapError handleDot)
        |> Parser.ooo bodyParser
        |> Parser.o (symbol "}" |> Parser.mapError handleClosingBraces)


defEquals : Parser PState.ExpectedString ()
defEquals =
    symbol "="


semicolon : Parser PState.ExpectedString ()
semicolon =
    symbol ";"



-- ===Operators===


type OperatorKeyword
    = -- Var Intro
      VarUse
      -- Bool
    | ConstTrue
    | ConstFalse
    | MatchBool
      -- Pair
    | Pair
    | MatchPair
      -- Sum
    | Left
    | Right
    | MatchSum
      -- Function
    | Application
    | Abstraction
      -- Nat
    | ConstZero
    | ConstNat
    | Succ
    | FoldNat
      -- List
    | ConstEmpty
    | Cons
    | FoldList
      -- Let binding
    | LetBe
    | Let
      -- Freeze
    | Delay
    | Force


operatorKeyword : Parser ExpectedOperatorKeyword OperatorKeyword
operatorKeyword =
    let
        handleKeywordError : PState.ExpectedStringIn -> ExpectedOperatorKeyword
        handleKeywordError msg =
            case msg of
                PState.ExpectedStringIn { consumedSuccessfully, failedAtChar } ->
                    ExpectedOperatorKeyword { consumedSuccessfully = consumedSuccessfully, failedAtChar = failedAtChar }

        handleGapError : OperatorKeyword -> ExpectedKeywordGapCharacter -> ExpectedOperatorKeyword
        handleGapError operatorKeyword0 msg =
            case msg of
                ExpectedKeywordGapCharacter { failedAtChar } ->
                    ExpectedGapAfterOperatorKeyword { operatorKeyword = operatorKeyword0, failedAtChar = failedAtChar }
    in
    Parser.stringIn
        [ ( "$", VarUse )

        -- Bool
        , ( "true", ConstTrue )
        , ( "false", ConstFalse )
        , ( "match-bool", MatchBool )

        -- Pair
        , ( "pair", Pair )
        , ( "match-pair", MatchPair )

        -- Sum
        , ( "left", Left )
        , ( "right", Right )
        , ( "match-sum", MatchSum )

        -- Function
        , ( "@", Application )
        , ( "\\", Abstraction )

        -- Nat
        , ( "zero", ConstZero )
        , ( "n", ConstNat )
        , ( "succ", Succ )
        , ( "fold-nat", FoldNat )

        -- List
        , ( "empty", ConstEmpty )
        , ( "cons", Cons )
        , ( "fold-list", FoldList )

        -- Let binding
        , ( "let-be", LetBe )
        , ( "let", Let )

        -- Freeze
        , ( "delay", Delay )
        , ( "force", Force )
        ]
        --  TODO: Should I worry about optional parenthesization here?
        |> Parser.mapError handleKeywordError
        |> Parser.andThen
            (\operatorKeyword0 ->
                case operatorKeyword0 of
                    VarUse ->
                        -- Var use can't have keyword Gap
                        Parser.return VarUse

                    ConstNat ->
                        -- ConstNat use can't have keyword Gap
                        Parser.return ConstNat

                    _ ->
                        Parser.return operatorKeyword0
                            |> Parser.o (keywordGap |> Parser.mapError (handleGapError operatorKeyword0))
            )
        |> Parser.o spaces



-- helpers


mandatoryTermParens : Parser ExpectedTerm a -> Parser ExpectedTerm a
mandatoryTermParens parser =
    mandatoryParens parser
        |> Parser.mapError
            (\eitherMsg ->
                case eitherMsg of
                    Either.Left msg ->
                        ExpectedParens msg

                    Either.Right msg ->
                        msg
            )


optionalTermParens : Parser ExpectedTerm a -> Parser ExpectedTerm a
optionalTermParens parser =
    optionalParens parser
        |> Parser.mapError
            (\eitherMsg ->
                case eitherMsg of
                    Either.Left msg ->
                        ExpectedParens msg

                    Either.Right msg ->
                        msg
            )


semicolonTerm : Parser ExpectedTerm ()
semicolonTerm =
    semicolon
        |> Parser.mapError
            (\msg ->
                case msg of
                    PState.ExpectedString { failedAtChar } ->
                        ExpectedBindingTerm (ExpectedSemicolon { failedAtChar = failedAtChar })
            )


defEqualsTerm : Parser ExpectedTerm ()
defEqualsTerm =
    let
        handleLeftArrow msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedBindingTerm (ExpectedDefEquals { failedAtChar = failedAtChar })
    in
    defEquals |> Parser.mapError handleLeftArrow



-- ===Term===


term : Parser ExpectedTerm Term
term =
    let
        handlePatternError : OperatorKeyword -> Either PState.ExpectedString ExpectedKeywordGapCharacter -> ExpectedTerm
        handlePatternError patternKeyword msg =
            case msg of
                Either.Left stringError ->
                    case stringError of
                        PState.ExpectedString { expected, consumedSuccessfully, failedAtChar } ->
                            ExpectedPattern (ExpectedPatternKeyword { patternKeyword = patternKeyword, consumedSuccessfully = consumedSuccessfully, failedAtChar = failedAtChar })

                Either.Right gapError ->
                    case gapError of
                        ExpectedKeywordGapCharacter { failedAtChar } ->
                            ExpectedPattern (ExpectedGapAfterPatternKeyword { patternKeyword = patternKeyword, failedAtChar = failedAtChar })

        constant : Term -> Parser ExpectedTerm Term
        constant c =
            optionalTermParens emptySequence
                |> Parser.map (\() -> c)

        operator1 : (Term -> Term) -> Parser ExpectedTerm Term
        operator1 f =
            mandatoryTermParens
                (Parser.return f
                    |> Parser.ooo term
                )

        operator2 : (Term -> Term -> Term) -> Parser ExpectedTerm Term
        operator2 f =
            mandatoryTermParens
                (Parser.return f
                    |> Parser.ooo term
                    |> Parser.ooo term
                )

        varSeq0 : Parser ExpectedTerm ()
        varSeq0 =
            optionalTermParens emptySequence

        varSeq1 : Parser ExpectedTerm TermVarName
        varSeq1 =
            mandatoryTermParens
                (varIntro |> Parser.mapError ExpectedIdentifier)

        varSeq2 : Parser ExpectedTerm ( TermVarName, TermVarName )
        varSeq2 =
            mandatoryTermParens
                (Parser.pair
                    (varIntro |> Parser.mapError ExpectedIdentifier)
                    (varIntro |> Parser.mapError ExpectedIdentifier)
                )

        pattern0 : String -> OperatorKeyword -> Parser ExpectedTerm ()
        pattern0 str op =
            Parser.unit
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.o varSeq0

        pattern1 : String -> OperatorKeyword -> Parser ExpectedTerm TermVarName
        pattern1 str op =
            Parser.identity
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.ooo varSeq1

        pattern2 : String -> OperatorKeyword -> Parser ExpectedTerm ( TermVarName, TermVarName )
        pattern2 str op =
            Parser.identity
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.ooo varSeq2

        natTerm : Parser ExpectedTerm Term
        natTerm =
            nat
                |> Parser.o spaces
                |> Parser.map Base.intToNatTerm
                |> Parser.mapError (\_ -> ExpectedNatConstant)
    in
    operatorKeyword
        |> Parser.mapError ExpectedOperator
        |> Parser.andThen
            (\operatorkeyword0 ->
                case operatorkeyword0 of
                    -- Var
                    VarUse ->
                        varIntro
                            |> Parser.mapError ExpectedIdentifier
                            |> Parser.map Base.VarUse

                    -- Bool
                    ConstTrue ->
                        constant Base.ConstTrue

                    ConstFalse ->
                        constant Base.ConstFalse

                    MatchBool ->
                        optionalTermParens
                            (Parser.return
                                (\arg ( (), branch0 ) ( (), branch1 ) ->
                                    Base.MatchBool arg { trueBranch = { body = branch0 }, falseBranch = { body = branch1 } }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (pattern0 "true" ConstTrue) term)
                                |> Parser.ooo (binding (pattern0 "false" ConstFalse) term)
                            )

                    -- Pair
                    Pair ->
                        operator2 Base.Pair

                    MatchPair ->
                        optionalTermParens
                            (Parser.return
                                (\arg ( ( var0, var1 ), body ) ->
                                    Base.MatchPair arg { var0 = var0, var1 = var1, body = body }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (pattern2 "pair" Pair) term)
                            )

                    -- Sum
                    Left ->
                        operator1 Base.Left

                    Right ->
                        operator1 Base.Right

                    MatchSum ->
                        optionalTermParens
                            (Parser.return
                                (\arg ( leftVar, leftBody ) ( rightVar, rightBody ) ->
                                    Base.MatchSum arg { leftBranch = { var = leftVar, body = leftBody }, rightBranch = { var = rightVar, body = rightBody } }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (pattern1 "left" Left) term)
                                |> Parser.ooo (binding (pattern1 "right" Right) term)
                            )

                    -- Function
                    Application ->
                        -- operator2 Base.Application
                        mandatoryTermParens
                            (Parser.repeat term
                                |> Parser.andThen
                                    (\terms ->
                                        case terms of
                                            [] ->
                                                Parser.fail (ExpectedAtleastTwoArgumentsToApplication { got = 0 })

                                            [ _ ] ->
                                                Parser.fail (ExpectedAtleastTwoArgumentsToApplication { got = 1 })

                                            fn0 :: arg0 :: args ->
                                                let
                                                    applicationWithListOfArgs : Term -> List Term -> Term
                                                    applicationWithListOfArgs fn args0 =
                                                        case args0 of
                                                            [] ->
                                                                fn

                                                            arg :: args1 ->
                                                                applicationWithListOfArgs (Base.Application fn arg) args1
                                                in
                                                Parser.return (applicationWithListOfArgs (Base.Application fn0 arg0) args)
                                    )
                            )

                    Abstraction ->
                        let
                            abstractionWithListOfVars : List TermVarName -> Term -> Term
                            abstractionWithListOfVars vars0 body =
                                case vars0 of
                                    [] ->
                                        body

                                    var :: vars1 ->
                                        Base.Abstraction { var = var, body = abstractionWithListOfVars vars1 body }
                        in
                        optionalTermParens
                            (binding (Parser.repeat varIntro |> Parser.mapError ExpectedIdentifier) term
                                |> Parser.andThen
                                    (\( vars, body ) ->
                                        case vars of
                                            [] ->
                                                Parser.fail (ExpectedAtleastOneParameterToAbstraction { got = 0 })

                                            var0 :: vars1 ->
                                                Parser.return (Base.Abstraction { var = var0, body = abstractionWithListOfVars vars1 body })
                                    )
                            )

                    -- Nat
                    ConstZero ->
                        constant Base.ConstZero

                    ConstNat ->
                        natTerm

                    Succ ->
                        operator1 Base.Succ

                    FoldNat ->
                        optionalTermParens
                            (Parser.return
                                (\arg ( (), zeroBody ) ( succVar, succBody ) ->
                                    Base.FoldNat
                                        arg
                                        { zeroBranch = { body = zeroBody }
                                        , succBranch = { var = succVar, body = succBody }
                                        }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (pattern0 "zero" FoldNat) term)
                                |> Parser.ooo (binding (pattern1 "succ" Succ) term)
                            )

                    -- List
                    ConstEmpty ->
                        constant Base.ConstEmpty

                    Cons ->
                        operator2 Base.Cons

                    FoldList ->
                        optionalTermParens
                            (Parser.return
                                (\arg ( (), emptyBody ) ( ( consVar0, consVar1 ), consBody ) ->
                                    Base.FoldList
                                        arg
                                        { emptyBranch = { body = emptyBody }
                                        , consBranch =
                                            { var0 = consVar0, var1 = consVar1, body = consBody }
                                        }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (pattern0 "empty" ConstEmpty) term)
                                |> Parser.ooo (binding (pattern2 "cons" Cons) term)
                            )

                    -- Let binding
                    LetBe ->
                        -- let-be(e { x . body })
                        -- let-be e { x . body }
                        optionalTermParens
                            (Parser.return
                                (\arg ( var, body ) ->
                                    Base.LetBe arg { var = var, body = body }
                                )
                                |> Parser.ooo term
                                |> Parser.ooo (binding (varIntro |> Parser.mapError ExpectedIdentifier) term)
                            )

                    Let ->
                        -- let x < e; body
                        Parser.return
                            (\var arg body ->
                                Base.LetBe arg { var = var, body = body }
                            )
                            |> Parser.ooo (varIntro |> Parser.mapError ExpectedIdentifier)
                            |> Parser.o defEqualsTerm
                            |> Parser.ooo term
                            |> Parser.o semicolonTerm
                            |> Parser.ooo term

                    -- Freeze
                    Delay ->
                        binding
                            spaces
                            term
                            |> Parser.map (\( (), body ) -> Base.Delay { body = body })

                    Force ->
                        operator1 Base.Force
            )
