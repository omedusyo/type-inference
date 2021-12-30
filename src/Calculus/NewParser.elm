module Calculus.NewParser exposing
    ( ExpectedModuleTerm(..)
    , ExpectedTerm(..)
    , interface
    , moduleTerm
    , moduleTermErrorToString
    , runInterface
    , runModuleTerm
    , runTerm
    , term
    ,  termErrorToString
       -- , typeTerm

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
import Lib.Parser.Error as ParserError
import Lib.Parser.Parser as Parser
import Lib.Parser.Position as PPosition
import Lib.Parser.State as ParserState
import Set exposing (Set)



-- ===Whitesepace/Gaps===


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
    Set.union whitespaceChars (Set.fromList [ '(', ')', '{', '}', '.', '$', '\'', '"', '%', '/', '=', ';', '\\', '[', ']' ])


isGapChar : Char -> Bool
isGapChar c =
    Set.member c gapChars



-- ===Types===


type alias Parser e a =
    Parser.Parser {} e a



-- ===nat===
-- ===Basics===


spaces : Parser e ()
spaces =
    Parser.allWhileTrue isWhitespaceChar
        |> Parser.discard


symbol : String -> Parser ParserError.ExpectedString ()
symbol symbol0 =
    Parser.unit
        |> Parser.o (Parser.string symbol0)
        |> Parser.o spaces


defEquals : Parser ParserError.ExpectedString ()
defEquals =
    symbol "="


semicolon : Parser ParserError.ExpectedString ()
semicolon =
    symbol ";"



-- Doesn't consume anything, but may fail


keywordGap : Parser ExpectedKeywordGapCharacter ()
keywordGap =
    Parser.check (Parser.anyCharSatisfying isGapChar)
        |> Parser.ifError
            (\failedAtCharErr ->
                case failedAtCharErr.failedAtChar of
                    Just c ->
                        Parser.fail { failedAtChar = c }

                    Nothing ->
                        Parser.unit
            )
        |> Parser.o spaces


keyword : String -> Parser (Either ParserError.ExpectedString ExpectedKeywordGapCharacter) ()
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
        handleFirstOpenParen : ParserError.ExpectedString -> ExpectedParens
        handleFirstOpenParen { expected, consumedSuccessfully, failedAtChar } =
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
        handleFirstClosingParen : ParserError.ExpectedString -> ExpectedParens
        handleFirstClosingParen { expected, consumedSuccessfully, failedAtChar } =
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



-- ===Identifier===


identifier : Parser ExpectedIdentifierIntroduction String
identifier =
    -- TODO: You can make the error messages better
    let
        excludedChars : Set Char
        excludedChars =
            Set.union
                (Set.fromList [ '$', '.', '(', ')', '{', '}', '\'', '"', '/', '%', '=', ';', '[', ']' ])
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
    in
    Parser.anyCharSatisfying isInnerVarChar
        |> Parser.mapError ExpectedIdentifierCharacters
        |> Parser.andThen
            (\c ->
                if Char.isDigit c then
                    Parser.fail (ExpectedIdentifierToStartWithNonDigit { failedAtChar = c })

                else
                    Parser.allWhileTrue isInnerVarChar
                        |> Parser.map (\str -> String.cons c str)
            )
        |> Parser.o spaces



-- ===Generic Errors===


type alias FailedAtCharOrEmpty =
    { failedAtChar : ParserError.FailedAtCharOrEmpty }


type alias FailedAtChar =
    { failedAtChar : ParserError.FailedAtChar }


type ExpectedIdentifierIntroduction
    = ExpectedIdentifierCharacters ParserError.CharFailedTest
    | ExpectedIdentifierToStartWithNonDigit FailedAtChar


type alias ExpectedKeywordGapCharacter =
    FailedAtChar


type ExpectedParens
    = ExpectedOpenParens FailedAtCharOrEmpty
    | ExpectedClosingParens FailedAtCharOrEmpty


failedAtCharToStringHelper : Char -> String
failedAtCharToStringHelper c =
    String.concat
        [ "failed at char '"
        , if c == '\n' then
            "<new-line>"

          else if c == ' ' then
            "<space>"

          else if c == '\t' then
            "<tab>"

          else
            String.fromChar c
        , "`"
        ]


failedAtMaybeCharToString : { e | failedAtChar : ParserError.FailedAtCharOrEmpty } -> String
failedAtMaybeCharToString { failedAtChar } =
    case failedAtChar of
        Just c ->
            failedAtCharToStringHelper c

        Nothing ->
            "failed at <empty-input>"


failedAtCharToString : { e | failedAtChar : ParserError.FailedAtChar } -> String
failedAtCharToString { failedAtChar } =
    failedAtCharToStringHelper failedAtChar


consumedSuccessfullyToString : { e | consumedSuccessfully : String } -> String
consumedSuccessfullyToString { consumedSuccessfully } =
    String.concat [ "consumed successfully: ", consumedSuccessfully ]


expectedParensToString : ExpectedParens -> String
expectedParensToString msg =
    case msg of
        ExpectedOpenParens msg0 ->
            String.concat
                [ "Expected open parentheses ("
                , failedAtMaybeCharToString msg0
                , ")"
                ]

        ExpectedClosingParens msg0 ->
            String.concat
                [ "Expected closing parentheses ("
                , failedAtMaybeCharToString msg0
                , ")"
                ]


expectedIdentifierIntroductionToString : String -> ExpectedIdentifierIntroduction -> String
expectedIdentifierIntroductionToString identifierKind msg =
    case msg of
        ExpectedIdentifierCharacters expIden ->
            String.concat
                [ "Expected to see "
                , identifierKind
                , " identifier character but "
                , failedAtMaybeCharToString expIden
                ]

        ExpectedIdentifierToStartWithNonDigit failedAtCharError ->
            String.concat
                [ "Expected "
                , identifierKind
                , " identifier to start with non-digit ("
                , failedAtCharToString failedAtCharError
                , ")"
                ]



-- ===Term Errors===


type ExpectedOperatorKeyword a
    = ExpectedOperatorKeyword ParserError.ExpectedStringIn
    | ExpectedGapAfterOperatorKeyword { operatorKeyword : a, failedAtChar : ParserError.FailedAtChar }


type ExpectedBindingTerm
    = ExpectedOpenBraces ParserError.ExpectedString
    | ExpectedDot ParserError.ExpectedString
    | ExpectedClosingBraces ParserError.ExpectedString
    | ExpectedDefEquals ParserError.ExpectedString
    | ExpectedSemicolon ParserError.ExpectedString


type ExpectedPattern
    = ExpectedPatternKeyword { patternKeyword : TermOperatorKeyword, consumedSuccessfully : String, failedAtChar : ParserError.FailedAtCharOrEmpty }
    | ExpectedGapAfterPatternKeyword { patternKeyword : TermOperatorKeyword, failedAtChar : ParserError.FailedAtChar }


type ExpectedTerm
    = ExpectedOperator (ExpectedOperatorKeyword TermOperatorKeyword)
    | ExpectedTermIdentifier ExpectedIdentifierIntroduction
    | ExpectedParens ExpectedParens
    | ExpectedBindingTerm ExpectedBindingTerm
    | ExpectedPattern ExpectedPattern
    | ExpectedAtleastTwoArgumentsToApplication { got : Int }
    | ExpectedAtleastOneParameterToAbstraction { got : Int }
    | ExpectedNatConstant ParserError.ExpectedDecimalNaturalNumber
    | ExpectedClosingOfApplication ParserError.ExpectedString
    | ExpectedModuleTermInModuleAccess ExpectedModuleTerm
    | ExpectedModuleAccessSymbol ParserError.ExpectedString


expectedOperatorKeywordToString : ExpectedOperatorKeyword TermOperatorKeyword -> String
expectedOperatorKeywordToString msg =
    case msg of
        ExpectedOperatorKeyword opKeyword ->
            String.concat
                [ "Expected operator keyword ("
                , consumedSuccessfullyToString opKeyword
                , ", "
                , failedAtMaybeCharToString opKeyword
                , ")"
                ]

        ExpectedGapAfterOperatorKeyword gapAfter ->
            String.concat
                [ "Succesfully parsed operator keyword `"
                , operatorKeywordToString gapAfter.operatorKeyword
                , "`, but failed to see a gap following it ("
                , failedAtCharToString { failedAtChar = gapAfter.failedAtChar }
                , ")"
                ]


expectedBindingTermToString : ExpectedBindingTerm -> String
expectedBindingTermToString msg =
    case msg of
        ExpectedOpenBraces msg0 ->
            String.concat [ "Expected open braces (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedDot msg0 ->
            String.concat [ "Expected dot (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedClosingBraces msg0 ->
            String.concat [ "Expected closing braces (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedDefEquals msg0 ->
            String.concat [ "Expected def. equals symbol in let binding (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedSemicolon msg0 ->
            String.concat [ "Expected semicolon after let binding (", failedAtMaybeCharToString msg0, ")" ]


expectedPatternToString : ExpectedPattern -> String
expectedPatternToString msg =
    case msg of
        ExpectedPatternKeyword msg0 ->
            String.concat
                [ "Expected the pattern keyword `"
                , operatorKeywordToString msg0.patternKeyword
                , "` ("
                , consumedSuccessfullyToString msg0
                , ", "
                , failedAtMaybeCharToString msg0
                , ")"
                ]

        ExpectedGapAfterPatternKeyword msg0 ->
            String.concat
                [ "Expected a gap after the pattern keyword `"
                , operatorKeywordToString msg0.patternKeyword
                , "` ("
                , failedAtMaybeCharToString { failedAtChar = Just msg0.failedAtChar }
                , ")"
                ]


operatorKeywordToString : TermOperatorKeyword -> String
operatorKeywordToString op =
    case op of
        -- Var Intro
        VarUse ->
            "$"

        -- Bool
        ConstTrue ->
            "true"

        ConstFalse ->
            "false"

        MatchBool ->
            "match-bool"

        -- Pair
        Pair ->
            "pair"

        MatchPair ->
            "match-pair"

        -- Sum ->
        Left ->
            "left"

        Right ->
            "right"

        MatchSum ->
            "match-sum"

        -- Function
        Application ->
            "["

        Abstraction ->
            "\\"

        -- Nat
        ConstZero ->
            "zero"

        NatLiteral ->
            "0nat-literal"

        Succ ->
            "succ"

        FoldNat ->
            "fold-nat"

        -- List
        ConstEmpty ->
            "empty"

        Cons ->
            "cons"

        FoldList ->
            "fold-list"

        -- Let binding
        LetBe ->
            "let-be"

        Let ->
            "let"

        -- Freeze
        Delay ->
            "delay"

        Force ->
            "force"

        -- Module Access
        ModuleAccess ->
            "@"


expectedTermToString : ExpectedTerm -> String
expectedTermToString msg =
    case msg of
        ExpectedOperator expectedOperatorKeyword ->
            expectedOperatorKeywordToString expectedOperatorKeyword

        ExpectedTermIdentifier expectedIdentifierIntroduction ->
            expectedIdentifierIntroductionToString "term" expectedIdentifierIntroduction

        ExpectedParens expectedParens ->
            expectedParensToString expectedParens

        ExpectedBindingTerm expectedBindingTerm ->
            expectedBindingTermToString expectedBindingTerm

        ExpectedPattern expectedPattern ->
            expectedPatternToString expectedPattern

        ExpectedAtleastTwoArgumentsToApplication { got } ->
            String.concat [ "Expected atleast two arguments to application, instead got ", String.fromInt got ]

        ExpectedAtleastOneParameterToAbstraction { got } ->
            String.concat [ "Expected atleast one parameter to abstraction, instead got ", String.fromInt got ]

        ExpectedNatConstant msg0 ->
            String.concat [ "Expected natural number literal (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedClosingOfApplication msg0 ->
            String.concat [ "Expected closing of application (", failedAtMaybeCharToString msg0, ")" ]

        ExpectedModuleTermInModuleAccess err ->
            expectedModuleTermToString err

        ExpectedModuleAccessSymbol msg0 ->
            String.concat [ "Expected module access symbol `.` (", failedAtMaybeCharToString msg0, ")" ]


termErrorToString : ParserError.Error ExpectedTerm -> String
termErrorToString error =
    let
        position : PPosition.Position
        position =
            ParserError.getPosition error

        msg : ExpectedTerm
        msg =
            ParserError.getMsg error
    in
    String.concat
        [ expectedTermToString msg
        , " at (line="
        , String.fromInt position.line
        , ", col="
        , String.fromInt position.col
        , ")"
        ]



-- ===Sequences===


emptySequence : Parser e ()
emptySequence =
    spaces



-- ===Binding===


varIntro : Parser ExpectedIdentifierIntroduction TermVarName
varIntro =
    identifier


binding : Parser ExpectedTerm a -> Parser ExpectedTerm b -> Parser ExpectedTerm ( a, b )
binding patternParser bodyParser =
    Parser.return (\vars body -> ( vars, body ))
        |> Parser.o (symbol "{" |> Parser.mapError (ExpectedBindingTerm << ExpectedOpenBraces))
        |> Parser.ooo patternParser
        |> Parser.o (symbol "." |> Parser.mapError (ExpectedBindingTerm << ExpectedDot))
        |> Parser.ooo bodyParser
        |> Parser.o (symbol "}" |> Parser.mapError (ExpectedBindingTerm << ExpectedClosingBraces))



-- ===Operators===


type TermOperatorKeyword
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
    | NatLiteral
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
      -- Module Access
    | ModuleAccess


handleKeywordGapError : a -> ExpectedKeywordGapCharacter -> ExpectedOperatorKeyword a
handleKeywordGapError operatorKeyword0 gapError =
    ExpectedGapAfterOperatorKeyword { operatorKeyword = operatorKeyword0, failedAtChar = gapError.failedAtChar }


operatorKeyword : Parser (ExpectedOperatorKeyword TermOperatorKeyword) TermOperatorKeyword
operatorKeyword =
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
        , ( "[", Application )
        , ( "\\", Abstraction )

        -- Nat
        , ( "zero", ConstZero )
        , ( "0", NatLiteral )
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

        -- Module Access
        , ( "/", ModuleAccess )
        ]
        --  TODO: Should I worry about optional parenthesization here?
        |> Parser.mapError ExpectedOperatorKeyword
        |> Parser.andThen
            (\operatorKeyword0 ->
                case operatorKeyword0 of
                    VarUse ->
                        -- Var use can't have keyword Gap
                        Parser.return VarUse

                    NatLiteral ->
                        -- NatLiteral use can't have keyword Gap
                        Parser.return NatLiteral

                    _ ->
                        Parser.return operatorKeyword0
                            |> Parser.o (keywordGap |> Parser.mapError (handleKeywordGapError operatorKeyword0))
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
    semicolon |> Parser.mapError (ExpectedBindingTerm << ExpectedSemicolon)


defEqualsTerm : Parser ExpectedTerm ()
defEqualsTerm =
    defEquals |> Parser.mapError (ExpectedBindingTerm << ExpectedDefEquals)



-- ===Term===


term : Parser ExpectedTerm Term
term =
    let
        handlePatternError : TermOperatorKeyword -> Either ParserError.ExpectedString ExpectedKeywordGapCharacter -> ExpectedTerm
        handlePatternError patternKeyword msg =
            case msg of
                Either.Left { expected, consumedSuccessfully, failedAtChar } ->
                    ExpectedPattern (ExpectedPatternKeyword { patternKeyword = patternKeyword, consumedSuccessfully = consumedSuccessfully, failedAtChar = failedAtChar })

                Either.Right gapError ->
                    ExpectedPattern (ExpectedGapAfterPatternKeyword { patternKeyword = patternKeyword, failedAtChar = gapError.failedAtChar })

        naturalNumberLiteral : Parser ExpectedTerm Term
        naturalNumberLiteral =
            Parser.naturalNumber
                |> Parser.mapError ExpectedNatConstant
                |> Parser.o spaces
                |> Parser.map Base.intToNatTerm

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
                (varIntro |> Parser.mapError ExpectedTermIdentifier)

        varSeq2 : Parser ExpectedTerm ( TermVarName, TermVarName )
        varSeq2 =
            mandatoryTermParens
                (Parser.pair
                    (varIntro |> Parser.mapError ExpectedTermIdentifier)
                    (varIntro |> Parser.mapError ExpectedTermIdentifier)
                )

        pattern0 : String -> TermOperatorKeyword -> Parser ExpectedTerm ()
        pattern0 str op =
            Parser.unit
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.o varSeq0

        pattern1 : String -> TermOperatorKeyword -> Parser ExpectedTerm TermVarName
        pattern1 str op =
            Parser.identity
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.ooo varSeq1

        pattern2 : String -> TermOperatorKeyword -> Parser ExpectedTerm ( TermVarName, TermVarName )
        pattern2 str op =
            Parser.identity
                |> Parser.o (keyword str |> Parser.mapError (handlePatternError op))
                |> Parser.ooo varSeq2
    in
    operatorKeyword
        |> Parser.mapError ExpectedOperator
        |> Parser.andThen
            (\operatorkeyword0 ->
                case operatorkeyword0 of
                    -- Var
                    VarUse ->
                        varIntro
                            |> Parser.mapError ExpectedTermIdentifier
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
                        Parser.repeat term
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
                            |> Parser.o (symbol "]" |> Parser.mapError ExpectedClosingOfApplication)

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
                            (binding (Parser.repeat varIntro |> Parser.mapError ExpectedTermIdentifier) term
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

                    NatLiteral ->
                        naturalNumberLiteral

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
                                |> Parser.ooo (binding (varIntro |> Parser.mapError ExpectedTermIdentifier) term)
                            )

                    Let ->
                        -- let x < e; body
                        Parser.return
                            (\var arg body ->
                                Base.LetBe arg { var = var, body = body }
                            )
                            |> Parser.ooo (varIntro |> Parser.mapError ExpectedTermIdentifier)
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

                    -- Module Access
                    ModuleAccess ->
                        -- /($Nat multiply)
                        -- /([$F $Nat] foo)
                        mandatoryTermParens
                            (Parser.return Base.ModuleAccess
                                -- TODO: this will generate module-access error without telling the user that this is in the context of a module access
                                |> Parser.ooo (moduleTerm |> Parser.mapError ExpectedModuleTermInModuleAccess)
                                |> Parser.ooo (varIntro |> Parser.mapError ExpectedTermIdentifier)
                            )
            )


runTerm : String -> Result (ParserError.Error ExpectedTerm) Term
runTerm input =
    Parser.run term {} (ParserState.return input)
        |> Result.map Tuple.second



-- ===Types===
-- ==Errors==
-- type ExpectedTypeIdentifierIntroduction
--     = ExpectedIdentifierCharacters { failedAtChar : Maybe Char }
--     | ExpectedIdentifierToStartWithNonDigit { failedAtChar : Char }


type ExpectedType
    = ExpectedTypeIdentifier ParserError.ExpectedDecimalNaturalNumber
    | ExpectedTypeVarUseToStartWithQuote ParserError.ExpectedString
    | ExpectedTypeOperator (ExpectedOperatorKeyword TypeOperatorKeyword)
    | ExpectedTypeParens ExpectedParens


mandatoryTypeTermParens : Parser ExpectedType a -> Parser ExpectedType a
mandatoryTypeTermParens parser =
    mandatoryParens parser
        |> Parser.mapError
            (\eitherMsg ->
                case eitherMsg of
                    Either.Left msg ->
                        ExpectedTypeParens msg

                    Either.Right msg ->
                        msg
            )


optionalTypeTermParens : Parser ExpectedType a -> Parser ExpectedType a
optionalTypeTermParens parser =
    optionalParens parser
        |> Parser.mapError
            (\eitherMsg ->
                case eitherMsg of
                    Either.Left msg ->
                        ExpectedTypeParens msg

                    Either.Right msg ->
                        msg
            )


typeVarIntro : Parser ExpectedType TypeVarName
typeVarIntro =
    -- Why can't we just use `Parser.int`?
    -- Because it consumes spaces and dots in `123  .`. WTF?
    Parser.naturalNumber
        |> Parser.o spaces
        |> Parser.mapError ExpectedTypeIdentifier


typeVar : Parser ExpectedType Type
typeVar =
    Parser.return (\typeVarName -> Base.VarType typeVarName)
        |> Parser.o (Parser.string "'" |> Parser.mapError ExpectedTypeVarUseToStartWithQuote)
        |> Parser.ooo typeVarIntro


type TypeOperatorKeyword
    = TypeVarUse
    | Product
    | Sum
    | Arrow
    | ConstBool
    | ConstNat
    | List
      -- TODO: forall
    | Frozen


typeOperatorKeyword : Parser (ExpectedOperatorKeyword TypeOperatorKeyword) TypeOperatorKeyword
typeOperatorKeyword =
    Parser.stringIn
        [ ( "'", TypeVarUse )
        , ( "Product", Product )
        , ( "Sum", Sum )
        , ( "Arrow", Arrow )
        , ( "Bool", ConstBool )
        , ( "Nat", ConstNat )
        , ( "List", List )
        , ( "Frozen", Frozen )
        ]
        |> Parser.mapError ExpectedOperatorKeyword
        |> Parser.andThen
            (\typeOperatorKeyword0 ->
                case typeOperatorKeyword0 of
                    TypeVarUse ->
                        -- Var use can't have keyword Gap
                        Parser.return TypeVarUse

                    _ ->
                        Parser.return typeOperatorKeyword0
                            |> Parser.o (keywordGap |> Parser.mapError (handleKeywordGapError typeOperatorKeyword0))
            )
        |> Parser.o spaces


typeTerm : Parser ExpectedType Type
typeTerm =
    let
        constant : Type -> Parser ExpectedType Type
        constant c =
            optionalTypeTermParens emptySequence
                |> Parser.map (\() -> c)

        operator1 : (Type -> Type) -> Parser ExpectedType Type
        operator1 f =
            mandatoryTypeTermParens
                (Parser.return f
                    |> Parser.ooo typeTerm
                )

        operator2 : (Type -> Type -> Type) -> Parser ExpectedType Type
        operator2 f =
            mandatoryTypeTermParens
                (Parser.return f
                    |> Parser.ooo typeTerm
                    |> Parser.ooo typeTerm
                )
    in
    typeOperatorKeyword
        |> Parser.mapError ExpectedTypeOperator
        |> Parser.andThen
            (\typeOperatorKeyword0 ->
                case typeOperatorKeyword0 of
                    TypeVarUse ->
                        typeVarIntro |> Parser.map Base.VarType

                    Product ->
                        operator2 Base.Product

                    Sum ->
                        operator2 Base.Sum

                    Arrow ->
                        operator2 Base.Arrow

                    ConstBool ->
                        constant Base.ConstBool

                    ConstNat ->
                        constant Base.ConstNat

                    List ->
                        operator1 Base.List

                    -- TODO: forall
                    Frozen ->
                        operator1 Base.Frozen
            )



-- ===Module===
-- =Errors=


type ExpectedModuleTerm
    = -- Module Operator Keyword
      ExpectedModuleOperator ParserError.ExpectedStringIn
    | ExpectedGapAfterModuleOperator { operatorKeyword : ModuleOperatorKeyword, failedAtChar : ParserError.FailedAtChar }
      -- Module Var Use
    | ExpectedModuleIdentifier ExpectedIdentifierIntroduction
      -- Module Literal
    | ExpectedGapAfterModuleKeyword { failedAtChar : ParserError.FailedAtChar }
    | ExpectedModuleOpenBraces ParserError.ExpectedString
    | ExpectedModuleLetBinding ExpectedModuleLetBinding
    | ExpectedSemicolonAfterModuleLetBinding ParserError.ExpectedString
    | ExpectedModuleClosingBraces ParserError.ExpectedString


type ExpectedModuleLetBinding
    = ExpectedModuleLetBindingKeyword ParserError.ExpectedStringIn
    | ExpectedGapAfterModuleLetBindingKeyword FailedAtChar
    | ExpectedEqualsInModuleLetBinding ParserError.ExpectedString
      -- term
    | ExpectedTermIdentifierInModuleLetBinding ExpectedIdentifierIntroduction
    | ExpectedTermInModuleLetBinding ExpectedTerm
      -- module
    | ExpectedModuleIdentifierInModuleLetBinding ExpectedIdentifierIntroduction
    | ExpectedModuleTermInModuleLetBinding ExpectedModuleTerm


expectedModuleTermToString : ExpectedModuleTerm -> String
expectedModuleTermToString msg =
    case msg of
        -- Module Keyword
        ExpectedModuleOperator err ->
            String.concat
                [ "Expected module operator keyword ("
                , consumedSuccessfullyToString err
                , ", "
                , failedAtMaybeCharToString err
                , ")"
                ]

        ExpectedGapAfterModuleOperator err ->
            String.concat
                [ "Succesfully parsed module operator keyword `"
                , moduleOperatorKeywordToString err.operatorKeyword
                , "`, but failed to see a gap following it ("
                , failedAtCharToString err
                , ")"
                ]

        -- Module Var Use
        ExpectedModuleIdentifier err ->
            expectedIdentifierIntroductionToString "module" err

        -- Module Literal
        ExpectedGapAfterModuleKeyword failedAtCharError ->
            String.concat
                [ "Expected a gap after the `module` keyword "
                , String.concat [ "(", failedAtCharToString failedAtCharError, ")" ]
                ]

        ExpectedModuleOpenBraces failedAtCharError ->
            String.concat
                [ "Expected an open brace `{` after the `module` keyword "
                , String.concat [ "(", failedAtMaybeCharToString failedAtCharError, ")" ]
                ]

        ExpectedModuleLetBinding expectedModuleLetBinding ->
            expectedModuleLetBindingToString expectedModuleLetBinding

        ExpectedSemicolonAfterModuleLetBinding failedAtCharError ->
            String.concat
                [ "Expected a semicolon `;` to separate the module let-bindings "
                , String.concat [ "(", failedAtMaybeCharToString failedAtCharError, ")" ]
                ]

        ExpectedModuleClosingBraces failedAtCharError ->
            String.concat
                [ "Expected a closing brace `}` to end the module expression "
                , String.concat [ "(", failedAtMaybeCharToString failedAtCharError, ")" ]
                ]


expectedModuleLetBindingToString : ExpectedModuleLetBinding -> String
expectedModuleLetBindingToString msg =
    case msg of
        ExpectedModuleLetBindingKeyword err ->
            -- TODO consumed succesfully?
            String.concat
                [ "Expected module let binding keyword "
                , String.concat [ "(such as ", allModuleLetBindingKeywords |> List.map (\keyword0 -> String.concat [ "`", moduleLetBindingKeywordToString keyword0, "`" ]) |> String.join ", ", ")" ]
                , " "
                , String.concat [ "(", failedAtMaybeCharToString err, ")" ]
                ]

        ExpectedGapAfterModuleLetBindingKeyword failedAtCharError ->
            String.concat
                [ "Expected a gap after the let-binding keyword "
                , String.concat [ "(", failedAtCharToString failedAtCharError, ")" ]
                ]

        ExpectedEqualsInModuleLetBinding err ->
            -- TODO err.consumedSuccessfully?
            String.concat
                [ "Expected equals `=` symbol "
                , String.concat [ "(", failedAtMaybeCharToString err, ")" ]
                ]

        -- term
        ExpectedTermIdentifierInModuleLetBinding expectedIdentifierIntroduction ->
            expectedIdentifierIntroductionToString "term" expectedIdentifierIntroduction

        ExpectedTermInModuleLetBinding expectedTerm ->
            expectedTermToString expectedTerm

        -- module
        ExpectedModuleIdentifierInModuleLetBinding expectedIdentifierIntroduction ->
            expectedIdentifierIntroductionToString "module" expectedIdentifierIntroduction

        ExpectedModuleTermInModuleLetBinding expectedModuleTerm ->
            expectedModuleTermToString expectedModuleTerm


moduleTermErrorToString : ParserError.Error ExpectedModuleTerm -> String
moduleTermErrorToString error =
    let
        position : PPosition.Position
        position =
            ParserError.getPosition error

        msg : ExpectedModuleTerm
        msg =
            ParserError.getMsg error
    in
    String.concat
        [ expectedModuleTermToString msg
        , " at (line="
        , String.fromInt position.line
        , ", col="
        , String.fromInt position.col
        , ")"
        ]



-- =Terms=


type ModuleOperatorKeyword
    = ModuleVarUse
    | ModuleLiteralTerm
    | FunctorApplication


allModuleOperatorKeywords : List ModuleOperatorKeyword
allModuleOperatorKeywords =
    [ ModuleVarUse, ModuleLiteralTerm, FunctorApplication ]


moduleOperatorKeywordToString : ModuleOperatorKeyword -> String
moduleOperatorKeywordToString keyword0 =
    case keyword0 of
        ModuleVarUse ->
            "$"

        ModuleLiteralTerm ->
            "module"

        FunctorApplication ->
            "["


moduleOperatorKeyword : Parser ExpectedModuleTerm ModuleOperatorKeyword
moduleOperatorKeyword =
    let
        handleGap keyword0 gapError =
            ExpectedGapAfterModuleOperator { operatorKeyword = keyword0, failedAtChar = gapError.failedAtChar }
    in
    Parser.stringIn
        [ ( "$", ModuleVarUse )
        , ( "module", ModuleLiteralTerm )
        , ( "[", FunctorApplication )
        ]
        |> Parser.mapError ExpectedModuleOperator
        |> Parser.andThen
            (\keyword0 ->
                case keyword0 of
                    ModuleVarUse ->
                        Parser.return ModuleVarUse

                    _ ->
                        Parser.return keyword0
                            |> Parser.o (keywordGap |> Parser.mapError (handleGap keyword0))
            )
        |> Parser.o spaces


moduleVarIntro : Parser ExpectedIdentifierIntroduction ModuleVarName
moduleVarIntro =
    identifier


moduleTerm : Parser ExpectedModuleTerm ModuleTerm
moduleTerm =
    moduleOperatorKeyword
        |> Parser.andThen
            (\keyword0 ->
                case keyword0 of
                    ModuleVarUse ->
                        moduleVarIntro
                            |> Parser.mapError ExpectedModuleIdentifier
                            |> Parser.map Base.ModuleVarUse

                    ModuleLiteralTerm ->
                        moduleLiteral |> Parser.map Base.ModuleLiteralTerm

                    FunctorApplication ->
                        Debug.todo ""
            )


moduleLiteral : Parser ExpectedModuleTerm ModuleLiteral
moduleLiteral =
    Parser.return ModuleLiteral
        |> Parser.o (symbol "{" |> Parser.mapError ExpectedModuleOpenBraces)
        |> Parser.ooo
            (Parser.repeatUntil
                ((moduleLetBinding |> Parser.mapError ExpectedModuleLetBinding)
                    |> Parser.o (semicolon |> Parser.mapError ExpectedSemicolonAfterModuleLetBinding)
                )
                (symbol "}" |> Parser.mapError ExpectedModuleClosingBraces)
            )


type ModuleLetBindingKeyword
    = LetTerm
    | LetType
    | LetModule
    | LetFunctor


allModuleLetBindingKeywords : List ModuleLetBindingKeyword
allModuleLetBindingKeywords =
    [ LetTerm, LetType, LetModule, LetFunctor ]


moduleLetBindingKeywordToString : ModuleLetBindingKeyword -> String
moduleLetBindingKeywordToString keyword0 =
    case keyword0 of
        LetTerm ->
            "let-term"

        LetType ->
            "let-type"

        LetModule ->
            "let-module"

        LetFunctor ->
            "let-functor"


moduleLetBindingKeyword : Parser ExpectedModuleLetBinding ModuleLetBindingKeyword
moduleLetBindingKeyword =
    Parser.stringIn
        [ ( "let-term", LetTerm )
        , ( "let-type", LetType )
        , ( "let-module", LetModule )
        , ( "let-functor", LetFunctor )
        ]
        |> Parser.mapError ExpectedModuleLetBindingKeyword
        |> Parser.o (keywordGap |> Parser.mapError ExpectedGapAfterModuleLetBindingKeyword)
        |> Parser.o spaces


moduleLetBinding : Parser ExpectedModuleLetBinding ModuleLetBinding
moduleLetBinding =
    moduleLetBindingKeyword
        |> Parser.andThen
            (\keyword0 ->
                case keyword0 of
                    LetTerm ->
                        Parser.return Base.LetTerm
                            |> Parser.ooo (varIntro |> Parser.mapError ExpectedTermIdentifierInModuleLetBinding)
                            |> Parser.o (symbol "=" |> Parser.mapError ExpectedEqualsInModuleLetBinding)
                            |> Parser.ooo (term |> Parser.mapError ExpectedTermInModuleLetBinding)

                    LetType ->
                        Debug.todo ""

                    LetModule ->
                        Parser.return Base.LetModule
                            |> Parser.ooo (moduleVarIntro |> Parser.mapError ExpectedModuleIdentifierInModuleLetBinding)
                            |> Parser.o (symbol "=" |> Parser.mapError ExpectedEqualsInModuleLetBinding)
                            |> Parser.ooo (moduleTerm |> Parser.mapError ExpectedModuleTermInModuleLetBinding)

                    LetFunctor ->
                        Debug.todo ""
            )


runModuleTerm : String -> Result (ParserError.Error ExpectedModuleTerm) ModuleTerm
runModuleTerm input =
    Parser.run moduleTerm {} (ParserState.return input)
        |> Result.map Tuple.second



-- ===Interface===


type ExpectedInterface
    = ExpectedInterfaceKeyword ParserError.ExpectedString
    | ExpectedGapAfterInterfaceKeyword { failedAtChar : ParserError.FailedAtChar }
    | ExpectedInterfaceOpenBraces ParserError.ExpectedString
    | ExpectedInterfaceAssumption ExpectedInterfaceAssumption
    | ExpectedSemicolonAfterInterfaceAssumption ParserError.ExpectedString
    | ExpectedInterfaceClosingBraces ParserError.ExpectedString


type ExpectedInterfaceAssumption
    = ExpectedInterfaceAssumptionKeyword ParserError.ExpectedStringIn
    | ExpectedGapAfterInterfaceAssumptionKeyword FailedAtChar
    | ExpectedTypingSymbolInInterfaceAssumption ParserError.ExpectedString
      -- term
    | ExpectedTermIdentifierInInterfaceAssumption ExpectedIdentifierIntroduction
    | ExpectedTypeInInterfaceAssumption ExpectedType
      -- module
    | ExpectedModuleIdentifierInInterfaceAssumption ExpectedIdentifierIntroduction
    | ExpectedInterfaceLiteralInInterfaceAssumption ExpectedInterface


interface : Parser ExpectedInterface Interface
interface =
    let
        handleKeyword msg =
            case msg of
                Either.Left expectedStringErr ->
                    ExpectedInterfaceKeyword expectedStringErr

                Either.Right gapErr ->
                    ExpectedGapAfterInterfaceKeyword gapErr
    in
    Parser.identity
        |> Parser.o (keyword "interface" |> Parser.mapError handleKeyword)
        |> Parser.ooo interfaceLiteral


interfaceLiteral : Parser ExpectedInterface Interface
interfaceLiteral =
    Parser.return Base.Interface
        |> Parser.o (symbol "{" |> Parser.mapError ExpectedInterfaceOpenBraces)
        |> Parser.ooo
            (Parser.repeatUntil
                ((interfaceAssumption |> Parser.mapError ExpectedInterfaceAssumption)
                    |> Parser.o (semicolon |> Parser.mapError ExpectedSemicolonAfterInterfaceAssumption)
                )
                (symbol "}" |> Parser.mapError ExpectedModuleClosingBraces)
            )


type InterfaceAssumptionKeyword
    = AssumeTerm
    | AssumeType
    | AssumeModule
    | AssumeFunctor


allInterfaceAssumptionKeywords : List InterfaceAssumptionKeyword
allInterfaceAssumptionKeywords =
    [ AssumeTerm, AssumeType, AssumeModule, AssumeFunctor ]


interfaceAssumptionKeywordToString : InterfaceAssumptionKeyword -> String
interfaceAssumptionKeywordToString keyword0 =
    case keyword0 of
        AssumeTerm ->
            "assume-term"

        AssumeType ->
            "assume-type"

        AssumeModule ->
            "assume-module"

        AssumeFunctor ->
            "assume-functor"


interfaceAssumptionKeyword : Parser ExpectedInterfaceAssumption InterfaceAssumptionKeyword
interfaceAssumptionKeyword =
    Parser.stringIn
        [ ( "assume-term", AssumeTerm )
        , ( "assume-type", AssumeType )
        , ( "assume-module", AssumeModule )
        , ( "assume-functor", AssumeFunctor )
        ]
        |> Parser.mapError ExpectedInterfaceAssumptionKeyword
        |> Parser.o (keywordGap |> Parser.mapError ExpectedGapAfterInterfaceAssumptionKeyword)
        |> Parser.o spaces


interfaceAssumption : Parser ExpectedInterfaceAssumption InterfaceAssumption
interfaceAssumption =
    interfaceAssumptionKeyword
        |> Parser.andThen
            (\keyword0 ->
                case keyword0 of
                    AssumeTerm ->
                        Parser.return Base.AssumeTerm
                            |> Parser.ooo (varIntro |> Parser.mapError ExpectedTermIdentifierInInterfaceAssumption)
                            |> Parser.o (symbol ":" |> Parser.mapError ExpectedTypingSymbolInInterfaceAssumption)
                            |> Parser.ooo (typeTerm |> Parser.mapError ExpectedTypeInInterfaceAssumption)

                    AssumeType ->
                        Debug.todo ""

                    AssumeModule ->
                        Parser.return Base.AssumeModule
                            |> Parser.ooo (moduleVarIntro |> Parser.mapError ExpectedModuleIdentifierInInterfaceAssumption)
                            |> Parser.o (symbol ":" |> Parser.mapError ExpectedTypingSymbolInInterfaceAssumption)
                            |> Parser.ooo (interfaceLiteral |> Parser.mapError ExpectedInterfaceLiteralInInterfaceAssumption)

                    AssumeFunctor ->
                        Debug.todo ""
            )


runInterface : String -> Result (ParserError.Error ExpectedInterface) Interface
runInterface input =
    Parser.run interface {} (ParserState.return input)
        |> Result.map Tuple.second
