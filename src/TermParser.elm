module TermParser exposing (..)

import Main exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


whitespaceChars : Set Char
whitespaceChars =
    Set.fromList [ ' ', '\n', '\u{000D}', '\t' ]


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> Set.member c whitespaceChars)



-- ===TERM===


term : Parser Term
term =
    Parser.oneOf
        [ -- literals/variables
          varUse
        , emptyList
        , bool
        , natConstant -- TODO: this has to be last constant? why? Does it chomp stuff?

        -- operatorTerm has to be at the ned
        , operatorTerm
        ]


operatorTerm : Parser Term
operatorTerm =
    -- TODO: how to refactor this identity?
    Parser.succeed (\x -> x)
        |. Parser.symbol "("
        |. spaces
        |= Parser.oneOf
            [ abstraction
            , ifThenElse
            , pair
            , snd
            , fst
            , left
            , right
            , natSucc
            , cons
            ]
        |. Parser.symbol ")"
        |. spaces



-- ===VAR===


varIntro : Parser TermVarName
varIntro =
    let
        excludedChars =
            Set.union
                (Set.fromList [ '$', '(', ')', '{', '}', '\'', '"' ])
                whitespaceChars

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
        |. Parser.keyword "true"


false : Parser Term
false =
    Parser.succeed BoolFalse
        |. Parser.keyword "false"


bool : Parser Term
bool =
    Parser.oneOf [ true, false ]


ifThenElse : Parser Term
ifThenElse =
    Parser.succeed IfThenElse
        |. Parser.keyword "if"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



-- ==Cartesian Product==
-- (pair e e')


pair : Parser Term
pair =
    Parser.succeed Pair
        |. Parser.keyword "pair"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces


fst : Parser Term
fst =
    Parser.succeed Fst
        |. Parser.keyword "first"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces


snd : Parser Term
snd =
    Parser.succeed Snd
        |. Parser.keyword "second"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



-- ==Function Space==
-- TODO: generalize so it can take 0,1,2,... variables
-- (fn (x) body)


abstraction : Parser Term
abstraction =
    Parser.succeed Abstraction
        |. Parser.keyword "fn"
        |. spaces
        -- Parameters
        |. Parser.symbol "("
        |. spaces
        |= varIntro
        |. spaces
        |. Parser.symbol ")"
        |. spaces
        -- body
        |= Parser.lazy (\() -> term)
        |. spaces



-- TODO: application
-- ==Coproduct==


left : Parser Term
left =
    Parser.succeed Left
        |. Parser.keyword "left"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces


right : Parser Term
right =
    Parser.succeed Right
        |. Parser.keyword "right"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



-- TODO: case
-- ===Nat===


natConstant : Parser Term
natConstant =
    Parser.int
        |> Parser.map intToNatTerm


natSucc : Parser Term
natSucc =
    Parser.succeed NatSucc
        |. Parser.keyword "succ"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



-- TODO: nat-loop
-- ===Lists===


emptyList : Parser Term
emptyList =
    Parser.succeed EmptyList
        |. Parser.keyword "empty-list"


cons : Parser Term
cons =
    Parser.succeed Cons
        |. Parser.keyword "cons"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



-- TODO: list-loop
