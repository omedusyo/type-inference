module TermParser exposing (..)

import Main exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)



-- true, false
-- 0, 1, 2, 3, ...
-- empty-list
--


whitespaceChars : Set Char
whitespaceChars =
    -- \u{000D} === \r
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
            , application
            , ifThenElse
            , pair
            , snd
            , fst
            , left
            , right
            , sumCase
            , natSucc
            , natLoop
            , cons
            , listLoop
            ]
        |. Parser.symbol ")"
        |. spaces



-- ===VAR===


varIntro : Parser TermVarName
varIntro =
    let
        excludedChars =
            Set.union
                (Set.fromList [ '$', '.', '(', ')', '{', '}', '\'', '"' ])
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
        -- test expression
        |= Parser.lazy (\() -> term)
        |. spaces
        -- left branch
        |. Parser.symbol "{"
        |. spaces
        -- TODO: make the `.` optional
        |. Parser.symbol "."
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces
        -- right branch
        |. Parser.symbol "{"
        |. spaces
        -- TODO: make the `.` optional
        |. Parser.symbol "."
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
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
--   (fn {x . body})
-- this syntax has an unfortunate amount of parens
-- TODO: generalize so it can take 0,1,2,... variables
--   (fn { x y z . body }) ~> (fn {x . (fn {y . (fn {z . body}) }) })


abstraction : Parser Term
abstraction =
    Parser.succeed Abstraction
        |. Parser.keyword "fn"
        |. spaces
        -- Parameters
        |. Parser.symbol "{"
        |. spaces
        |= varIntro
        |. spaces
        |. Parser.symbol "."
        |. spaces
        -- body
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces



-- TODO: should we have special syntax for application since it is so ubiquitous?
-- TODO: extend so
--   (apply f e1 e2) ~> (apply (apply f e1) e2)


application : Parser Term
application =
    Parser.succeed Application
        |. Parser.keyword "@"
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces



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



-- (sum-case e {x . e1} {y . e2})


sumCase : Parser Term
sumCase =
    Parser.succeed
        (\arg leftVar leftBody rightVar rightBody ->
            Case
                { arg = arg
                , leftVar = leftVar
                , leftBody = leftBody
                , rightVar = rightVar
                , rightBody = rightBody
                }
        )
        |. Parser.keyword "sum-case"
        |. spaces
        -- arg
        |= Parser.lazy (\() -> term)
        |. spaces
        -- leftVar
        |. Parser.symbol "{"
        |. spaces
        |= varIntro
        |. spaces
        |. Parser.symbol "."
        |. spaces
        -- left body
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces
        -- rightVar
        |. Parser.symbol "{"
        |. spaces
        |= varIntro
        |. spaces
        |. Parser.symbol "."
        |. spaces
        -- right body
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces



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



-- (nat-loop n initState { i s . body })


natLoop : Parser Term
natLoop =
    Parser.succeed
        (\arg initState indexVar stateVar body ->
            NatLoop
                { base = initState
                , loop =
                    { indexVar = indexVar
                    , stateVar = stateVar
                    , body = body
                    }
                , arg = arg
                }
        )
        |. Parser.keyword "nat-loop"
        |. spaces
        -- loop argument (the natural number bound)
        |= Parser.lazy (\() -> term)
        |. spaces
        -- the initial state of the loop
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "{"
        |. spaces
        -- index var
        |= varIntro
        |. spaces
        -- state var
        |= varIntro
        |. spaces
        |. Parser.symbol "."
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces



-- (nat-loop n e { i s . body })
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



-- (list-loop xs initState { x s . body })


listLoop : Parser Term
listLoop =
    Parser.succeed
        (\arg initState listElementVar stateVar body ->
            ListLoop
                { initState = initState
                , loop =
                    { listElementVar = listElementVar
                    , stateVar = stateVar
                    , body = body
                    }
                , arg = arg
                }
        )
        |. Parser.keyword "list-loop"
        |. spaces
        -- loop argument (the list)
        |= Parser.lazy (\() -> term)
        |. spaces
        -- the initial state of the loop
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "{"
        |. spaces
        -- list element var
        |= varIntro
        |. spaces
        -- state var
        |= varIntro
        |. spaces
        |. Parser.symbol "."
        |. spaces
        |= Parser.lazy (\() -> term)
        |. spaces
        |. Parser.symbol "}"
        |. spaces
