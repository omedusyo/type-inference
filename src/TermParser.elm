module TermParser exposing (..)

import LambdaBasics exposing (..)
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Set exposing (Set)



-- Constants/vars:
--   true, false
--   0n0, 0n1, 0n2, 0n3, ...
--   empty-list
--   $foo
-- Simple Operators
--   (pair e e')
--   (@ f x)
--   (left e)
--   (right e)
--   (succ e)
--   (cons e1 e2)
--   (@ e) -- force
-- Bindings Operators
--   (fn { x . body })
--   (match-pair pairExp { (pair x y) . body })
--   (if e { e1 } { e2 })
--   (sum-case e { (left x) . e1 } { (right y) . e2 })
--   (nat-loop   n initState { i s . body })
--   (list-loop xs initState { x s . body })
--   (fn { e }) -- delay
--   (let exp { x . body })


type alias TermParsingError =
    List DeadEnd


parsingErrorToString : TermParsingError -> String
parsingErrorToString =
    Parser.deadEndsToString


whitespaceChars : Set Char
whitespaceChars =
    -- \u{000D} === \r
    Set.fromList [ ' ', '\n', '\u{000D}', '\t' ]


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> Set.member c whitespaceChars)


keyword : String -> Parser ()
keyword str =
    Parser.keyword str
        |. spaces


symbol : String -> Parser ()
symbol str =
    Parser.symbol str
        |. spaces


paren : Parser a -> Parser a
paren p =
    Parser.succeed (\x -> x)
        |. symbol "("
        |= p
        |. symbol ")"


binding : Parser a -> Parser b -> Parser ( a, b )
binding varsParser bodyParser =
    Parser.succeed (\vars body -> ( vars, body ))
        |. symbol "{"
        |= varsParser
        |. symbol "."
        |= bodyParser
        |. symbol "}"


optionalBinding : Parser a -> Parser a
optionalBinding bodyParser =
    Parser.succeed (\x -> x)
        |. symbol "{"
        |. Parser.oneOf [ symbol ".", Parser.succeed () ]
        |= bodyParser
        |. symbol "}"


seq : Parser a -> Parser (List a)
seq p =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= p
                , Parser.succeed (Parser.Done (List.reverse xs))
                ]
        )



-- ===TERM===


parseTerm : String -> Result TermParsingError Term
parseTerm input =
    Parser.run term input


term : Parser Term
term =
    Parser.oneOf
        [ -- literals/variables
          varUse
        , emptyList
        , bool
        , natConstant -- TODO: this has to be last constant? why? Does it chomp stuff?

        -- operatorTerm has to be at the end
        , operatorTerm
        ]


operatorTerm : Parser Term
operatorTerm =
    -- TODO: how to refactor this identity?
    Parser.succeed (\x -> x)
        |. symbol "("
        |= Parser.oneOf
            [ abstraction
            , application
            , ifThenElse
            , pair
            , matchProduct
            , left
            , right
            , sumCase
            , natSucc
            , natLoop
            , cons
            , listLoop
            , letExpression
            ]
        |. symbol ")"



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
        |. spaces



-- Parses a nonemptysequence of vars
-- "foo  bar quux" ~> ["foo", "bar", "quux"]
-- "foo  bar quux   " ~> failure cause of the trailing whitespace
-- "   foo  bar quux" ~> failure cause you cant start with spaces


varsIntro : Parser (List TermVarName)
varsIntro =
    seq varIntro


terms : Parser (List Term)
terms =
    seq term


varUse : Parser Term
varUse =
    Parser.succeed VarUse
        -- WARNING: It's very important that this uses `Parser.symbol` and not `symbol` because here we don't actually want to consume trailing whitespace after `$`
        |. Parser.symbol "$"
        |= varIntro



-- ===Bool===


trueKeyword : String
trueKeyword =
    "true"


true : Parser Term
true =
    Parser.succeed BoolTrue
        |. keyword trueKeyword


truePattern : Parser ()
truePattern =
    keyword trueKeyword


falseKeyword : String
falseKeyword =
    "false"


false : Parser Term
false =
    Parser.succeed BoolFalse
        |. keyword falseKeyword


falsePattern : Parser ()
falsePattern =
    keyword falseKeyword


bool : Parser Term
bool =
    Parser.oneOf [ true, false ]



-- (if e { e1 } { e2 })
-- TODO: also allow pattern matching syntax `(if e { true . e1 } { false . e2 })`


ifThenElse : Parser Term
ifThenElse =
    Parser.succeed IfThenElse
        |. keyword "if"
        -- test expression
        |= Parser.lazy (\() -> term)
        -- left branch
        |= optionalBinding
            (Parser.lazy (\() -> term))
        -- right branch
        |= optionalBinding
            (Parser.lazy (\() -> term))



-- ==Cartesian Product==
-- (pair e e')


pairKeyword : String
pairKeyword =
    "pair"


pair : Parser Term
pair =
    Parser.succeed Pair
        |. keyword pairKeyword
        |= Parser.lazy (\() -> term)
        |= Parser.lazy (\() -> term)


pairPattern : Parser ( TermVarName, TermVarName )
pairPattern =
    Parser.succeed (\x y -> ( x, y ))
        |. keyword pairKeyword
        |= varIntro
        |= varIntro



--   (match-pair pairExp { (pair x y) . body })


matchProduct : Parser Term
matchProduct =
    Parser.succeed
        (\arg ( ( var0, var1 ), body ) ->
            MatchProduct
                { arg = arg
                , var0 = var0
                , var1 = var1
                , body = body
                }
        )
        |. keyword "match-pair"
        -- arg
        |= Parser.lazy (\() -> term)
        -- body
        |= binding
            (paren pairPattern)
            (Parser.lazy (\() -> term))



-- ==Function Space/Frozen type==
--   (fn {x . body})
--   (fn { x y z . body }) ~> (fn {x . (fn {y . (fn {z . body}) }) })
-- TODO
--   (fn { body }) ~> delay expression
--  but now you have to write
--   (fn {. body })
--  make the dot optional somehow


abstraction : Parser Term
abstraction =
    let
        abstractionWithListOfVars : List TermVarName -> Term -> Term
        abstractionWithListOfVars vars0 body =
            case vars0 of
                [] ->
                    Delay body

                [ var ] ->
                    Abstraction var body

                var :: vars1 ->
                    Abstraction var (abstractionWithListOfVars vars1 body)
    in
    Parser.succeed (\( vars0, body ) -> abstractionWithListOfVars vars0 body)
        |. keyword "fn"
        -- Parameters
        |= binding
            -- Note that this parser multiple vars and not just one
            varsIntro
            (Parser.lazy (\() -> term))



-- (@ f x)
-- (@ f e1 e2) ~> (@ (@ f e1) e2)
--
-- How can we interpret
--   (@ f) ?
-- Suppose f is a frozen computation e.g. the result of something like `(fn {. body })`
-- Then the application could be interpreted as the "thawing" of the frozen computation.


application : Parser Term
application =
    let
        applicationWithListOfArgs : Term -> List Term -> Term
        applicationWithListOfArgs fn args0 =
            case args0 of
                [] ->
                    Force fn

                [ arg ] ->
                    Application fn arg

                arg :: args1 ->
                    applicationWithListOfArgs (Application fn arg) args1
    in
    Parser.succeed applicationWithListOfArgs
        |. keyword "@"
        |= Parser.lazy (\() -> term)
        |= Parser.lazy (\() -> terms)



-- ==Coproduct==


leftKeyword : String
leftKeyword =
    "left"


left : Parser Term
left =
    Parser.succeed Left
        |. keyword leftKeyword
        |= Parser.lazy (\() -> term)


leftPattern : Parser TermVarName
leftPattern =
    Parser.succeed (\x -> x)
        |. keyword leftKeyword
        |= varIntro


rightKeyword : String
rightKeyword =
    "right"


right : Parser Term
right =
    Parser.succeed Right
        |. keyword rightKeyword
        |= Parser.lazy (\() -> term)


rightPattern : Parser TermVarName
rightPattern =
    Parser.succeed (\x -> x)
        |. keyword rightKeyword
        |= varIntro



-- (sum-case e {(left x) . e1} {(right y) . e2})


sumCase : Parser Term
sumCase =
    -- TODO: we allow
    --          (sum-case e {(left x) . e1} {(right y) . e2})
    --       but we don't allow
    --          (sum-case e {(right y) . e2} {(left x) . e1} )
    --       Order shouldn't matter.
    Parser.succeed
        (\arg ( leftVar, leftBody ) ( rightVar, rightBody ) ->
            Case
                { arg = arg
                , leftVar = leftVar
                , leftBody = leftBody
                , rightVar = rightVar
                , rightBody = rightBody
                }
        )
        |. keyword "match-sum"
        -- arg
        |= Parser.lazy (\() -> term)
        -- left
        |= binding
            (paren leftPattern)
            (Parser.lazy (\() -> term))
        -- right
        |= binding
            (paren rightPattern)
            (Parser.lazy (\() -> term))



-- ===Nat===
-- TODO: nat patterns


natConstant : Parser Term
natConstant =
    Parser.succeed intToNatTerm
        -- WARNING: It's very important that this uses `Parser.symbol` and not `symbol` because here we don't actually want to consume trailing whitespace after `0n`
        |. Parser.symbol "0n"
        |= Parser.int
        |. spaces


natSucc : Parser Term
natSucc =
    Parser.succeed NatSucc
        |. keyword "succ"
        |= Parser.lazy (\() -> term)



-- (nat-loop n initState { i s . body })


natLoop : Parser Term
natLoop =
    Parser.succeed
        (\arg initState ( ( indexVar, stateVar ), body ) ->
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
        |. keyword "nat-loop"
        -- loop argument (the natural number bound)
        |= Parser.lazy (\() -> term)
        -- the initial state of the loop
        |= Parser.lazy (\() -> term)
        |= binding
            (Parser.succeed (\indexVar stateVar -> ( indexVar, stateVar )) |= varIntro |= varIntro)
            (Parser.lazy (\() -> term))



-- ===Lists===


emptyList : Parser Term
emptyList =
    Parser.succeed EmptyList
        |. keyword "empty-list"


cons : Parser Term
cons =
    Parser.succeed Cons
        |. keyword "cons"
        |= Parser.lazy (\() -> term)
        |= Parser.lazy (\() -> term)



-- (list-loop xs initState { x s . body })


listLoop : Parser Term
listLoop =
    Parser.succeed
        (\arg initState ( ( listElementVar, stateVar ), body ) ->
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
        |. keyword "list-loop"
        -- loop argument (the list)
        |= Parser.lazy (\() -> term)
        -- the initial state of the loop
        |= Parser.lazy (\() -> term)
        |= binding
            (Parser.succeed (\listElementVar stateVar -> ( listElementVar, stateVar )) |= varIntro |= varIntro)
            (Parser.lazy (\() -> term))



-- ===LET===
-- (let exp { x . body })


letExpression : Parser Term
letExpression =
    Parser.succeed (\arg ( var, body ) -> Let var arg body)
        |. keyword "let"
        |= Parser.lazy (\() -> term)
        |= binding
            varIntro
            (Parser.lazy (\() -> term))



-- ===Module===


moduleTerm : Parser ModuleTerm
moduleTerm =
    Parser.succeed (\bindings -> { bindings = bindings })
        |. symbol "("
        |. keyword "module"
        |= seq moduleLetBinding
        |. symbol ")"


moduleVarIntro : Parser ModuleVarName
moduleVarIntro =
    varIntro


moduleLetBinding : Parser ModuleLetBinding
moduleLetBinding =
    Parser.succeed (\x -> x)
        |. symbol "("
        |= Parser.oneOf
            [ Parser.succeed (\var term0 -> LetTerm var term0)
                |. keyword "let-term"
                |= varIntro
                |= term
            , Parser.succeed (\var module0 -> LetModule var module0)
                |. keyword "let-module"
                |= moduleVarIntro
                |= Parser.lazy (\() -> moduleTerm)

            -- TODO: types
            ]
        |. symbol ")"
