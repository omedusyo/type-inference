module Calculus.NewParser exposing
    ( keyword
    , keywordGap
    , spaces
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
import Lib.Parser.Parser as Parser
import Lib.Parser.State as PState exposing (Error)
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


type alias ReadOnlyState =
    {}


type alias Parser e a =
    Parser.Parser ReadOnlyState (Error e) a


spaces : Parser e ()
spaces =
    Parser.allWhileTrue isWhitespaceChar
        |> Parser.discard


keywordGap : Parser ExpectedKeywordGapCharacter ()
keywordGap =
    Parser.check (Parser.anyCharSatisfying isGapChar)
        |> Parser.ifError
            (\error ->
                case error.msg of
                    PState.CharFailedTest { failedAtChar } ->
                        case failedAtChar of
                            Just c ->
                                Parser.fail
                                    (error |> PState.setMsg (ExpectedKeywordGapCharacter { failedAtChar = c }))

                            Nothing ->
                                Parser.return ()
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
    Parser.second
        (Parser.string keyword0
            |> Parser.mapError (PState.mapMsg handleStringError)
        )
        (keywordGap
            |> Parser.mapError (PState.mapMsg handleGapError)
        )



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
        |> Parser.mapError (PState.mapMsg handleCharError)
        |> Parser.andThen
            (\c ->
                if Char.isDigit c then
                    Parser.getPosition
                        (\position ->
                            Parser.fail { position = position, msg = ExpectedIdentifierToStartWithNonDigit { failedAtChar = c } }
                        )

                else
                    Parser.allWhileTrue isInnerVarChar
                        |> Parser.map (\str -> String.cons c str)
            )