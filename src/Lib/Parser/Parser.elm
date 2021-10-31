module Lib.Parser.Parser exposing
    ( Parser
    , allWhileSucceeds
    , allWhileTrue
    , andMap
    , andThen
    , andThen2
    , anyChar
    , anyCharSatisfying
    , check
    , discard
    , fail
    , first
    , getInput
    , getPosition
    , identity
    , ifError
    , ifSuccessIfError
    , join
    , make
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , mid
    , o
    , oneOf
    , ooo
    , or
    , pair
    , pairRightToLeft
    , push
    , read
    , return
    , run
    , second
    , sequence
    , string
    , throw
    , unit
    )

import Either exposing (Either)
import Lib.Parser.Error as Error exposing (Error)
import Lib.Parser.Position as Position exposing (Position)
import Lib.Parser.State as State exposing (ExpectedEndOfInput, ExpectedString, ExpectingNonEmptyInput, State)


type alias Parser r e a =
    r -> State -> Result (Error e) ( State, a )



-- ===base===


run : Parser r e a -> r -> State -> Result (Error e) ( State, a )
run parser =
    parser


make : (r -> State -> Result (Error e) ( State, a )) -> Parser r e a
make f =
    f


getInput : (String -> Parser r e a) -> Parser r e a
getInput f =
    make <|
        \r s -> run (f (State.getInput s)) r s


getPosition : (Position -> Parser r e a) -> Parser r e a
getPosition f =
    make <|
        \r s -> run (f (State.getPosition s)) r s



-- ===structure===
-- unit


return : a -> Parser r e a
return a =
    make <| \r s -> Ok ( s, a )



-- functor


map : (a -> b) -> Parser r e a -> Parser r e b
map f parser =
    make <|
        \r s0 ->
            run parser r s0
                |> Result.map (\( s1, a ) -> ( s1, f a ))



-- applicative


unit : Parser r e ()
unit =
    return ()


pair : Parser r e a -> Parser r e b -> Parser r e ( a, b )
pair parser0 parser1 =
    make <|
        \r s0 ->
            run parser0 r s0
                |> Result.andThen
                    (\( s1, a ) ->
                        run parser1 r s1
                            |> Result.map (\( s2, b ) -> ( s2, ( a, b ) ))
                    )


pairRightToLeft : Parser r e a -> Parser r e b -> Parser r e ( a, b )
pairRightToLeft parser0 parser1 =
    pair parser1 parser0 |> map (\( b, a ) -> ( a, b ))


first : Parser r e a -> Parser r e b -> Parser r e a
first parser0 parser1 =
    pair parser0 parser1
        |> map (\( a, _ ) -> a)


second : Parser r e a -> Parser r e b -> Parser r e b
second parser0 parser1 =
    pair parser0 parser1
        |> map (\( _, b ) -> b)


mid : Parser r e a -> Parser r e b -> Parser r e c -> Parser r e b
mid parser0 parser1 parser2 =
    second parser0 (first parser1 parser2)



-- return (\a b  -> ...)
--   |> o   p_ignored
--   |> ooo a_returned
--   |> o   p_ignored
--   |> o   p_ignored
--   |> ooo b_returned
--   |> o   p_ignored


o : Parser r e b -> Parser r e a -> Parser r e a
o parser1 parser0 =
    first parser0 parser1


ooo : Parser r e a -> Parser r e (a -> b) -> Parser r e b
ooo parser_a parser_f =
    pair parser_f parser_a
        |> map (\( f, a ) -> f a)


andMap : Parser r e a -> Parser r e (a -> b) -> Parser r e b
andMap =
    ooo


identity : Parser r e (a -> a)
identity =
    return (\x -> x)


map2 : (a0 -> a1 -> b) -> Parser r e a0 -> Parser r e a1 -> Parser r e b
map2 f parser0 parser1 =
    pair parser0 parser1
        |> map (\( a0, a1 ) -> f a0 a1)


map3 : (a0 -> a1 -> a2 -> b) -> Parser r e a0 -> Parser r e a1 -> Parser r e a2 -> Parser r e b
map3 f parser0 parser1 parser2 =
    pair parser0 (pair parser1 parser2)
        |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2)


map4 : (a0 -> a1 -> a2 -> a3 -> b) -> Parser r e a0 -> Parser r e a1 -> Parser r e a2 -> Parser r e a3 -> Parser r e b
map4 f parser0 parser1 parser2 parser3 =
    pair parser0 (pair parser1 (pair parser2 parser3))
        |> map (\( a0, ( a1, ( a2, a3 ) ) ) -> f a0 a1 a2 a3)


map5 : (a0 -> a1 -> a2 -> a3 -> a4 -> b) -> Parser r e a0 -> Parser r e a1 -> Parser r e a2 -> Parser r e a3 -> Parser r e a4 -> Parser r e b
map5 f parser0 parser1 parser2 parser3 parser4 =
    pair parser0 (pair parser1 (pair parser2 (pair parser3 parser4)))
        |> map (\( a0, ( a1, ( a2, ( a3, a4 ) ) ) ) -> f a0 a1 a2 a3 a4)



-- monad


andThen : (a -> Parser r e b) -> Parser r e a -> Parser r e b
andThen f parser =
    make <|
        \r s0 ->
            run parser r s0
                |> Result.andThen
                    (\( s1, a ) ->
                        run (f a) r s1
                    )


join : Parser r e (Parser r e a) -> Parser r e a
join parser_parser =
    parser_parser |> andThen (\x -> x)



-- TODO: andThen3,4,5


andThen2 : (a0 -> a1 -> Parser r e b) -> Parser r e a0 -> Parser r e a1 -> Parser r e b
andThen2 f parser0 parser1 =
    pair parser0 parser1
        |> andThen (\( a, b ) -> f a b)



-- ===error===


fail : e -> Parser r e a
fail error =
    make <| \_ s -> Err (Error.make (State.getPosition s) error)


mapError : (e -> f) -> Parser r e a -> Parser r f a
mapError f parser0 =
    make <|
        \r s ->
            run parser0 r s
                |> Result.mapError (Error.mapMsg f)


throw : e -> Parser r e a -> Parser r e b
throw error parser =
    second parser (fail error)



-- ===choice structure===


{-| This backtracks on failure.
p
|> ifSuccessIfError
(\\a -> ...)
(\\error -> ...)
-}
ifSuccessIfError : (a -> Parser r f b) -> (e -> Parser r f b) -> Parser r e a -> Parser r f b
ifSuccessIfError handleSuccess handleError parser =
    make <|
        \r s0 ->
            case run parser r s0 of
                Ok ( s1, a ) ->
                    run (handleSuccess a) r s1

                Err error ->
                    -- notice the backtrack to s0
                    run (handleError (Error.getMsg error)) r s0


ifErrorIfSuccess : (e -> Parser r f b) -> (a -> Parser r f b) -> Parser r e a -> Parser r f b
ifErrorIfSuccess handleError handleSuccess =
    ifSuccessIfError handleSuccess handleError


ifError : (e -> Parser r f a) -> Parser r e a -> Parser r f a
ifError handleError parser =
    parser
        |> ifSuccessIfError
            return
            handleError


or : Parser r e a -> Parser r e b -> Parser r ( e, e ) (Either a b)
or parser_a parser_b =
    parser_a
        |> ifSuccessIfError
            (\a -> return (Either.Left a))
            (\error_a ->
                parser_b
                    |> ifSuccessIfError
                        (\b -> return (Either.Right b))
                        (\error_b -> fail ( error_a, error_b ))
            )



-- This does lookahead with a parser


check : Parser r e a -> Parser r e ()
check parser =
    make <|
        \r s0 ->
            case run parser r s0 of
                Ok ( _, _ ) ->
                    -- This backtracks
                    Ok ( s0, () )

                Err error ->
                    Err error



-- match2
--   (string "pair", \() -> pair)
--   (string "cons", \() -> cons)
--   (\pairError consError -> ...)


match2 : ( Parser r e0 a0, a0 -> Parser r f b ) -> ( Parser r e1 a1, a1 -> Parser r f b ) -> (e0 -> e1 -> Parser r f b) -> Parser r f b
match2 ( pattern0, body0 ) ( pattern1, body1 ) combineErrors =
    pattern0
        |> ifSuccessIfError
            body0
            (\error0 ->
                pattern1
                    |> ifSuccessIfError
                        body1
                        (\error1 -> combineErrors error0 error1)
            )


match3 : ( Parser r e0 a0, a0 -> Parser r f b ) -> ( Parser r e1 a1, a1 -> Parser r f b ) -> ( Parser r e2 a2, a2 -> Parser r f b ) -> (e0 -> e1 -> e2 -> Parser r f b) -> Parser r f b
match3 ( pattern0, body0 ) ( pattern1, body1 ) ( pattern2, body2 ) combineErrors =
    pattern0
        |> ifSuccessIfError
            body0
            (\error0 ->
                pattern1
                    |> ifSuccessIfError
                        body1
                        (\error1 ->
                            pattern2
                                |> ifSuccessIfError
                                    body2
                                    (\error2 ->
                                        combineErrors error0 error1 error2
                                    )
                        )
            )



-- This is an analogue of the case-expression for parsing
-- Its type is really
--  forall n : Nat.
--    List n ( Parser e a, a -> Parser f b) ->(List n e -> Parser f b) -> Parser f b


match : List ( Parser r e a, a -> Parser r f b ) -> (List e -> Parser r f b) -> Parser r f b
match initBranches combineErrors =
    let
        loop : List ( Parser r e a, a -> Parser r f b ) -> List e -> Parser r f b
        loop branches0 reversed_errors =
            case branches0 of
                [] ->
                    combineErrors (List.reverse reversed_errors)

                ( pattern, body ) :: branches1 ->
                    pattern
                        |> ifSuccessIfError
                            body
                            (\error -> loop branches1 (error :: reversed_errors))
    in
    loop initBranches []


oneOf : List (Parser r e a) -> (List e -> Parser r e a) -> Parser r e a
oneOf parsers combineErrors =
    match (parsers |> List.map (\parser -> ( parser, \a -> return a ))) combineErrors



-- ===specifics===


end : Parser r ExpectedEndOfInput ()
end =
    make <|
        \_ s ->
            State.end s |> Result.map (\() -> ( s, () ))


lazy : (() -> Parser r e a) -> Parser r e a
lazy parser =
    parser ()


discard : Parser r e a -> Parser r e ()
discard parser =
    parser |> map (\_ -> ())


anyChar : (Char -> Parser r e a) -> Parser r (Either ExpectingNonEmptyInput e) a
anyChar f =
    make <|
        \r s0 ->
            case State.consumeAnyChar s0 of
                Ok ( s1, c ) ->
                    run (f c) r s1
                        |> Result.mapError (\error -> error |> Error.mapMsg Either.Right)

                Err error ->
                    Err (error |> Error.mapMsg Either.Left)



-- TODO: Should I have the type `0 | 1 | 2 | ... | 9`?


digit =
    Debug.todo ""



-- TODO: Should I use `any`? Maybe I should use `anyOne`? What about while consumers? `allSatisfying` `asMuchAsPossible`... nice modalities


anyCharSatisfying : (Char -> Bool) -> Parser r State.CharFailedTest Char
anyCharSatisfying test =
    make <|
        \_ s0 ->
            State.consumeAnyCharSatisfying test s0


string : String -> Parser r State.ExpectedString ()
string strToBeMatched =
    make <|
        \_ s0 ->
            State.consumeString strToBeMatched s0
                |> Result.map (\s1 -> ( s1, () ))



-- ===looping===


sequence : List (Parser r e a) -> Parser r e (List a)
sequence parsers0 =
    case parsers0 of
        [] ->
            return []

        parser :: parsers1 ->
            parser
                |> andThen
                    (\a -> map (\xs -> a :: xs) (sequence parsers1))


allWhileTrue : (Char -> Bool) -> Parser r e String
allWhileTrue test =
    make <|
        \_ s ->
            Ok (State.consumeWhileTrue test s)


allWhileSucceeds : Parser r e a -> Parser r f (List a)
allWhileSucceeds parser =
    make <|
        \r init_s ->
            let
                loop : ( State, List a ) -> Result (Error f) ( State, List a )
                loop ( s0, reversed_xs ) =
                    case run parser r s0 of
                        Ok ( s1, x ) ->
                            loop ( s1, x :: reversed_xs )

                        Err _ ->
                            Ok ( s0, List.reverse reversed_xs )
            in
            loop ( init_s, [] )



-- ===read-only state===


push : (r1 -> r2) -> Parser r2 e a -> Parser r1 e a
push f parser =
    make <|
        \r s ->
            run parser (f r) s


read : (r -> Parser r e a) -> Parser r e a
read f =
    make <|
        \r s ->
            run (f r) r s
