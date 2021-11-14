module Calculus.NewParser exposing
    ( atleastOneOpenParens
    , closingParens
    , mandatoryParens
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


type ExpectedPattern a
    = ExpectedPatternKeyword { consumedSuccessfully : String, failedAtChar : Maybe Char }
    | ExpectedGapAfterPatternKeyword { patternKeyword : a, failedAtChar : Char }


type Patterns
    = ExpectedBoolPattern (ExpectedPattern BoolPatternKeyword)


type ExpectedIdentifierIntroduction
    = ExpectedIdentifierCharacters { failedAtChar : Maybe Char }
    | ExpectedIdentifierToStartWithNonDigit { failedAtChar : Char }


type ExpectedBindingTerm
    = ExpectedOpenBraces { failedAtChar : Maybe Char }
    | ExpectedDot { failedAtChar : Maybe Char }
    | ExpectedClosingBraces { failedAtChar : Maybe Char }


type ExpectedParens
    = ExpectedOpenParens { failedAtChar : Maybe Char }
    | ExpectedClosingParens { failedAtChar : Maybe Char }


type ExpectedTerm
    = ExpectedOperator ExpectedOperatorKeyword
    | ExpectedIdentifier ExpectedIdentifierIntroduction
    | ExpectedParens ExpectedParens
    | ExpectedBindingTerm ExpectedBindingTerm
    | ExpectedPattern Patterns


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
    Set.union whitespaceChars (Set.fromList [ '(', ')', '{', '}', '.', '$', '\'', '"' ])


isGapChar : Char -> Bool
isGapChar c =
    Set.member c gapChars



-- ===Types===


type alias Parser e a =
    Parser.Parser {} e a



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


optionalParens : Parser ExpectedTerm a -> Parser ExpectedTerm a
optionalParens parser =
    zeroOrMoreOpenParens
        |> Parser.andThen
            (\numOfOpenParens ->
                parser
                    |> Parser.o (closingParens numOfOpenParens |> Parser.mapError ExpectedParens)
            )


mandatoryParens : Parser ExpectedTerm a -> Parser ExpectedTerm a
mandatoryParens parser =
    (atleastOneOpenParens |> Parser.mapError ExpectedParens)
        |> Parser.andThen
            (\numOfOpenParens ->
                parser
                    |> Parser.o (closingParens numOfOpenParens |> Parser.mapError ExpectedParens)
            )



-- ===Sequences===
-- TODO


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
                (Set.fromList [ '$', '.', '(', ')', '{', '}', '\'', '"' ])
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



-- binding : Parser e a -> Parser f b -> Parser (ExpectedBindingTerm e f) ( a, b )
-- binding varsParser bodyParser =
--     -- TODO: redo from scratch
--     let
--         handleOpenBraces : PState.ExpectedString -> ExpectedBindingTerm e f
--         handleOpenBraces msg =
--             case msg of
--                 PState.ExpectedString { failedAtChar } ->
--                     ExpectedOpenBraces { failedAtChar = failedAtChar }
--         handleDot : PState.ExpectedString -> ExpectedBindingTerm e f
--         handleDot msg =
--             case msg of
--                 PState.ExpectedString { failedAtChar } ->
--                     ExpectedDot { failedAtChar = failedAtChar }
--         handleClosingBraces : PState.ExpectedString -> ExpectedBindingTerm e f
--         handleClosingBraces msg =
--             case msg of
--                 PState.ExpectedString { failedAtChar } ->
--                     ExpectedClosingBraces { failedAtChar = failedAtChar }
--     in
--     Parser.return (\vars body -> ( vars, body ))
--         |> Parser.o (symbol "{" |> Parser.mapError handleOpenBraces)
--         |> Parser.ooo (varsParser |> Parser.mapError VariablesParserError)
--         |> Parser.o (symbol "." |> Parser.mapError handleDot)
--         -- TODO: here I need to somehow indicate that the parens are optional
--         |> Parser.ooo (bodyParser |> Parser.mapError BodyParserError)
--         |> Parser.o (symbol "}" |> Parser.mapError handleClosingBraces)


type OperatorKeyword
    = -- Var Intro
      VarUse
      -- Bool
    | ConstTrue
    | ConstFalse
    | IfThenElse
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
    | Succ
    | IterNat
      -- List
    | ConstEmpty
    | Cons
    | IterList



-- Bool pattern forest


type BoolPatternKeyword
    = TruePatternKeyword
    | FalsePatternKeyword


boolPatternKeyword : Parser ExpectedTerm BoolPatternKeyword
boolPatternKeyword =
    let
        handleKeywordError : PState.ExpectedStringIn -> ExpectedTerm
        handleKeywordError msg =
            case msg of
                PState.ExpectedStringIn { consumedSuccessfully, failedAtChar } ->
                    { consumedSuccessfully = consumedSuccessfully, failedAtChar = failedAtChar }
                        |> ExpectedPatternKeyword
                        |> ExpectedBoolPattern
                        |> ExpectedPattern

        handleGapError : BoolPatternKeyword -> ExpectedKeywordGapCharacter -> ExpectedTerm
        handleGapError boolKeyword0 msg =
            case msg of
                ExpectedKeywordGapCharacter { failedAtChar } ->
                    { patternKeyword = boolKeyword0, failedAtChar = failedAtChar }
                        |> ExpectedGapAfterPatternKeyword
                        |> ExpectedBoolPattern
                        |> ExpectedPattern
    in
    Parser.stringIn
        [ ( "true", TruePatternKeyword )
        , ( "false", FalsePatternKeyword )
        ]
        |> Parser.mapError handleKeywordError
        |> Parser.andThen
            (\boolKeyword0 ->
                Parser.return boolKeyword0
                    |> Parser.o (keywordGap |> Parser.mapError (handleGapError boolKeyword0))
            )
        |> Parser.o spaces



-- Operator forest


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
        , ( "if", IfThenElse )

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
        , ( "succ", Succ )
        , ( "iter-nat", IterNat )

        -- List
        , ( "empty", ConstEmpty )
        , ( "cons", Cons )
        , ( "iter-list", IterList )
        ]
        --  TODO: Should I worry about optional parenthesization here?
        |> Parser.mapError handleKeywordError
        |> Parser.andThen
            (\operatorKeyword0 ->
                case operatorKeyword0 of
                    VarUse ->
                        -- Var use can't have keyword Gap
                        Parser.return VarUse

                    _ ->
                        Parser.return operatorKeyword0
                            |> Parser.o (keywordGap |> Parser.mapError (handleGapError operatorKeyword0))
            )
        |> Parser.o spaces


term : Parser ExpectedTerm Term
term =
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

        openBrace : Parser ExpectedTerm ()
        openBrace =
            symbol "{" |> Parser.mapError handleOpenBraces

        dot : Parser ExpectedTerm ()
        dot =
            symbol "." |> Parser.mapError handleDot

        closingBrace : Parser ExpectedTerm ()
        closingBrace =
            symbol "}" |> Parser.mapError handleClosingBraces

        constant : Term -> Parser ExpectedTerm Term
        constant c =
            optionalParens emptySequence
                |> Parser.map (\() -> c)

        operator1 : (Term -> Term) -> Parser ExpectedTerm Term
        operator1 f =
            mandatoryParens
                (Parser.return f
                    |> Parser.ooo term
                )

        operator2 : (Term -> Term -> Term) -> Parser ExpectedTerm Term
        operator2 f =
            mandatoryParens
                (Parser.return f
                    |> Parser.ooo term
                    |> Parser.ooo term
                )
    in
    -- TODO: better error msg when on wrong number of arguments
    --       actually allowing optional parens for unary operators will make this impossible...
    --       Note `pair(left true right false)` is valid    q. Maybe we should require comma when not using parens?
    --         e.g. `pair(left true, right false)`  valid
    --         e.g. `pair(left(true) right(false))` valid
    --         but  `pair(left true  right false )` invalid
    --         e.g. `pair(pair true false, pair empty zero)`  valid
    --         e.g. `pair(pair true false ,,,, pair empty zero)`  valid
    --         e.g. `pair(pair(true false) pair(empty zero))` valid
    --         but  `pair(pair true false  pair empty zero )` invalid but how can I tell?
    --         but  `pair pair true false  pair empty zero  ` invalid but how can I tell?
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

                    IfThenElse ->
                        let
                            branch =
                                Parser.second
                                    openBrace
                                    (boolPatternKeyword
                                        |> Parser.andThen
                                            (\boolPattern ->
                                                Parser.return
                                                    (\body ->
                                                        ( boolPattern, body )
                                                    )
                                                    |> Parser.o dot
                                                    |> Parser.ooo term
                                                    |> Parser.o closingBrace
                                            )
                                    )
                        in
                        optionalParens
                            (Parser.return
                                (\arg ( pattern0, branch0 ) ( pattern1, branch1 ) ->
                                    case ( pattern0, pattern1 ) of
                                        ( TruePatternKeyword, FalsePatternKeyword ) ->
                                            Base.IfThenElse arg branch0 branch1

                                        ( FalsePatternKeyword, TruePatternKeyword ) ->
                                            Base.IfThenElse arg branch1 branch0

                                        _ ->
                                            -- TODO
                                            Debug.todo "THIS IS AN ERROR. Both patterns are the same"
                                )
                                |> Parser.ooo term
                                -- TODO: make this commutative
                                |> Parser.ooo branch
                                |> Parser.ooo branch
                            )

                    -- Pair
                    Pair ->
                        operator2 Base.Pair

                    MatchPair ->
                        Debug.todo ""

                    -- Sum
                    Left ->
                        operator1 Base.Left

                    Right ->
                        operator1 Base.Right

                    MatchSum ->
                        Debug.todo ""

                    -- Function
                    Application ->
                        operator2 Base.Application

                    Abstraction ->
                        Debug.todo ""

                    -- Nat
                    ConstZero ->
                        constant Base.ConstZero

                    Succ ->
                        operator1 Base.Succ

                    IterNat ->
                        Debug.todo ""

                    -- List
                    ConstEmpty ->
                        constant Base.ConstEmpty

                    Cons ->
                        Debug.todo ""

                    IterList ->
                        Debug.todo ""
            )
