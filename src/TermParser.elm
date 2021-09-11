module TermParser exposing (..)

import Main exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set



-- ===VAR===


varIntro : Parser TermVarName
varIntro =
    let
        excludedChars =
            Set.fromList [ '$', '(', ')', '{', '}', ' ', '\n', '\t', '\'', '"' ]

        isPrintable charCode =
            32 <= charCode && charCode <= 126
    in
    Parser.variable
        { start =
            -- TODO: first attempt
            \c ->
                let
                    charCode =
                        Char.toCode c
                in
                not (Set.member c excludedChars)
                    && isPrintable charCode
                    && not (Char.isDigit c)

        -- TODO also filter-out the non-printable characters and whitespace
        , inner =
            \c ->
                -- TODO: the only difference from start is can actually be a digit
                let
                    charCode =
                        Char.toCode c
                in
                not (Set.member c excludedChars)
                    && isPrintable charCode
        , reserved =
            -- TODO
            Set.empty
        }


varUse : Parser Term
varUse =
    Parser.succeed VarUse
        |. Parser.symbol "$"
        |= varIntro



-- ===Bool===


true : Parser Term
true =
    Parser.succeed BoolTrue
        |. Parser.keyword "True"


false : Parser Term
false =
    Parser.succeed BoolFalse
        |. Parser.keyword "False"


bool : Parser Term
bool =
    Parser.oneOf [ true, false ]



-- TODO if-then-else
-- ===Nat===


natConstant : Parser Term
natConstant =
    Parser.int
        |> Parser.map intToNatTerm



-- TODO: nat-loop
