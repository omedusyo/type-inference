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
        |. symbol "("
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


true : Parser Term
true =
    Parser.succeed BoolTrue
        |. keyword "true"


false : Parser Term
false =
    Parser.succeed BoolFalse
        |. keyword "false"


bool : Parser Term
bool =
    Parser.oneOf [ true, false ]



-- (if { x } { y })
-- TODO: also allow `(if { true . x } { false . y })`


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


pair : Parser Term
pair =
    Parser.succeed Pair
        |. keyword "pair"
        |= Parser.lazy (\() -> term)
        |= Parser.lazy (\() -> term)


fst : Parser Term
fst =
    Parser.succeed Fst
        |. keyword "first"
        |= Parser.lazy (\() -> term)


snd : Parser Term
snd =
    Parser.succeed Snd
        |. keyword "second"
        |= Parser.lazy (\() -> term)



-- ==Function Space==
--   (fn {x . body})
--   (fn { x y z . body }) ~> (fn {x . (fn {y . (fn {z . body}) }) })


abstraction : Parser Term
abstraction =
    let
        abstractionWithListOfVars : List TermVarName -> Term -> Term
        abstractionWithListOfVars vars0 body =
            case vars0 of
                [] ->
                    -- TODO: what's the result of 0-ary abstraction?
                    --       You'll have to introduce a type for Frozen computations (thunks?)
                    Debug.todo "Use of Frozen computation"

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
                    -- TODO: some sort of "thawing" of `fn`
                    Debug.todo "Use of Thawing of a Computation"

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


left : Parser Term
left =
    Parser.succeed Left
        |. keyword "left"
        |= Parser.lazy (\() -> term)


leftPattern : Parser TermVarName
leftPattern =
    Parser.succeed (\x -> x)
        |. keyword "left"
        |= varIntro


right : Parser Term
right =
    Parser.succeed Right
        |. keyword "right"
        |= Parser.lazy (\() -> term)


rightPattern : Parser TermVarName
rightPattern =
    Parser.succeed (\x -> x)
        |. keyword "right"
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


natConstant : Parser Term
natConstant =
    Parser.int
        |> Parser.map intToNatTerm


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



-- (nat-loop n e { i s . body })
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
