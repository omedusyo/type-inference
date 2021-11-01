module Calculus.NewParser exposing
    ( binding
    , false
    , initReadOnlyState
    , keyword
    , keywordGap
    , spaces
    , symbol
    , true
    , varIntro
    , whitespaceChars
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


type ExpectedKeyword
    = ExpectedKeyword { expected : String, consumedSuccessfully : String, failedAtChar : Maybe Char }
    | ExpectedGapAfterKeyword { failedAtChar : Char }


type ExpectedIdentifierIntroduction
    = ExpectedIdentifierCharacters { failedAtChar : Maybe Char }
    | ExpectedIdentifierToStartWithNonDigit { failedAtChar : Char }


type ExpectedBindingTerm e f
    = ExpectedOpenBraces { failedAtChar : Maybe Char }
    | ExpectedDot { failedAtChar : Maybe Char }
    | ExpectedClosingBraces { failedAtChar : Maybe Char }
    | VariablesParserError e
    | BodyParserError f


type ExpectedParens
    = ExpectedOpenParens { failedAtChar : Maybe Char }
    | ExpectedClosingParens { failedAtChar : Maybe Char }



-- TODO


type ExpectedTerm
    = ExpectedOperator ExpectedOperator
    | ExpectedConstant ExpectedKeyword
    | ExpectedParens ExpectedParens


type
    ExpectedOperator
    -- TODO
    = ExpectedOperatorName ExpectedKeyword


whitespaceChars : Set Char
whitespaceChars =
    -- \u{000D} === \r
    Set.fromList [ ' ', '\n', '\u{000D}', '\t' ]


isWhitespaceChar : Char -> Bool
isWhitespaceChar c =
    Set.member c whitespaceChars



-- the set of characters that's used to check for gaps after the keyword


gapChars : Set Char
gapChars =
    Set.union whitespaceChars (Set.fromList [ '(', '{', '.', '$', '\'', '"' ])


isGapChar : Char -> Bool
isGapChar c =
    Set.member c gapChars



-- ===Types===


type alias ReadOnlyState =
    { areParanthesesMandatory : Bool }


initReadOnlyState : ReadOnlyState
initReadOnlyState =
    { areParanthesesMandatory = False }


type alias Parser e a =
    Parser.Parser ReadOnlyState e a


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



-- This consumes the string `keyword0`, then looks ahead one character to see if there's a keyword gap
-- TODO: this is kinda inefficient if you have a lot of keywords.
-- TODO: It would be better to have function `keywords : List (String, Keyword) -> Parser e Keyword`
-- that would be given a map of strings to keywords, and then created a finite state machine (this could actually be built ahead of time)
-- to efficiently decide which keyword is being parsed


keyword : String -> Parser ExpectedKeyword ()
keyword keyword0 =
    let
        handleStringError : PState.ExpectedString -> ExpectedKeyword
        handleStringError msg =
            case msg of
                PState.ExpectedString { expected, consumedSuccessfully, failedAtChar } ->
                    ExpectedKeyword { expected = expected, consumedSuccessfully = consumedSuccessfully, failedAtChar = failedAtChar }

        handleGapError : ExpectedKeywordGapCharacter -> ExpectedKeyword
        handleGapError msg =
            case msg of
                ExpectedKeywordGapCharacter { failedAtChar } ->
                    ExpectedGapAfterKeyword { failedAtChar = failedAtChar }
    in
    Parser.unit
        |> Parser.o
            (Parser.string keyword0
                |> Parser.mapError handleStringError
            )
        |> Parser.o
            (keywordGap
                |> Parser.mapError handleGapError
            )
        |> Parser.o spaces


parens : Parser e a -> Parser (Either ExpectedParens e) a
parens parser =
    let
        handleOpenParens : PState.ExpectedString -> Either ExpectedParens e
        handleOpenParens msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    Either.Left (ExpectedOpenParens { failedAtChar = failedAtChar })

        handleClosingParens : PState.ExpectedString -> Either ExpectedParens e
        handleClosingParens msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    Either.Left (ExpectedClosingParens { failedAtChar = failedAtChar })
    in
    Parser.read
        (\{ areParanthesesMandatory } ->
            if areParanthesesMandatory then
                Parser.identity
                    |> Parser.o (symbol "(" |> Parser.mapError handleOpenParens)
                    |> Parser.ooo (parser |> Parser.mapError Either.Right)
                    |> Parser.o (symbol ")" |> Parser.mapError handleClosingParens)

            else
                -- TODO: maybe do a lookahead and make parentheses truly optional here?
                parser |> Parser.mapError Either.Right
        )


pushMandatoryParens : Parser e a -> Parser e a
pushMandatoryParens parser =
    Parser.push (\r -> { r | areParanthesesMandatory = True }) parser


pushOptionalParens : Parser e a -> Parser e a
pushOptionalParens parser =
    Parser.push (\r -> { r | areParanthesesMandatory = False }) parser



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


binding : Parser e a -> Parser f b -> Parser (ExpectedBindingTerm e f) ( a, b )
binding varsParser bodyParser =
    let
        handleOpenBraces : PState.ExpectedString -> ExpectedBindingTerm e f
        handleOpenBraces msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedOpenBraces { failedAtChar = failedAtChar }

        handleDot : PState.ExpectedString -> ExpectedBindingTerm e f
        handleDot msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedDot { failedAtChar = failedAtChar }

        handleClosingBraces : PState.ExpectedString -> ExpectedBindingTerm e f
        handleClosingBraces msg =
            case msg of
                PState.ExpectedString { failedAtChar } ->
                    ExpectedClosingBraces { failedAtChar = failedAtChar }
    in
    Parser.return (\vars body -> ( vars, body ))
        |> Parser.o (symbol "{" |> Parser.mapError handleOpenBraces)
        |> Parser.ooo (varsParser |> Parser.mapError VariablesParserError)
        |> Parser.o (symbol "." |> Parser.mapError handleDot)
        -- TODO: here I need to somehow indicate that the parens are optional
        |> Parser.ooo (pushOptionalParens bodyParser |> Parser.mapError BodyParserError)
        |> Parser.o (symbol "}" |> Parser.mapError handleClosingBraces)


pair : Parser e Term
pair =
    parens
        (Parser.return Base.Pair
            |> Parser.o (keyword "pair" |> Parser.mapError (Debug.todo ""))
            |> Parser.ooo (true |> Parser.mapError (Debug.todo ""))
            |> Parser.ooo (true |> Parser.mapError (Debug.todo ""))
        )
        |> Parser.mapError (Debug.todo "")



-- TODO


constKeywords : List String
constKeywords =
    [ "true", "false", "0n0", "empty-list" ]


constantsMap : Dict String Term
constantsMap =
    Dict.fromList
        [ ( "true", Base.ConstTrue )
        , ( "false", Base.ConstFalse )
        , ( "0n0", Base.ConstZero )
        , ( "empty-list", Base.ConstEmpty )
        ]


term : Parser ExpectedTerm Term
term =
    Debug.todo ""


operatorApplication0 : String -> Term -> Parser ExpectedTerm Term
operatorApplication0 keyword0 constantTerm =
    Parser.second
        (keyword keyword0 |> Parser.mapError ExpectedConstant)
        (Parser.return constantTerm)


handleKeywordToExpectedTerm : ExpectedKeyword -> ExpectedTerm
handleKeywordToExpectedTerm msg =
    ExpectedOperator (ExpectedOperatorName msg)


handleParensToExpectedTerm : Either ExpectedParens ExpectedTerm -> ExpectedTerm
handleParensToExpectedTerm msg =
    case msg of
        Either.Left parensMsg ->
            ExpectedParens parensMsg

        Either.Right err ->
            err


operatorApplication1 : String -> (Term -> Term) -> Parser ExpectedTerm Term
operatorApplication1 keyword0 f =
    parens
        (Parser.return f
            |> Parser.o (keyword keyword0 |> Parser.mapError handleKeywordToExpectedTerm)
            |> Parser.ooo (Parser.lazy (\() -> term))
        )
        |> Parser.mapError handleParensToExpectedTerm



-- ===Bool===


true : Parser ExpectedTerm Term
true =
    operatorApplication0 "true" Base.ConstTrue


false : Parser ExpectedTerm Term
false =
    operatorApplication0 "false" Base.ConstFalse


emptyList : Parser ExpectedTerm Term
emptyList =
    operatorApplication0 "empty" Base.ConstEmpty
