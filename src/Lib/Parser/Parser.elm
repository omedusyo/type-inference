module Lib.Parser.Parser exposing
    ( Parser
    , allWhileSucceeds
    , allWhileTrue
    , andMap
    , andThen
    , andThen2
    , anyChar
    , anyCharSatisfying
    , fail
    , first
    , getInput
    , join
    , make
    , map
    , map2
    , map3
    , map4
    , map5
    , mid
    , or
    , pair
    , pairRightToLeft
    , return
    , run
    , second
    , sequence
    , string
    , throw
    )

import Either exposing (Either)
import Lib.Parser.State as State exposing (Error, ExpectedEndOfInput, ExpectedString, ExpectingNonEmptyInput, Position, State)


type alias Parser e a =
    State -> Result e ( State, a )



-- ===base===


run : Parser e a -> State -> Result e ( State, a )
run parser =
    parser


make : (State -> Result e ( State, a )) -> Parser e a
make f =
    f


getInput : (String -> Parser e a) -> Parser e a
getInput f =
    make <|
        \s -> run (f (State.getInput s)) s


getPosition : (State.Position -> Parser e a) -> Parser e a
getPosition f =
    make <|
        \s -> run (f (State.getPosition s)) s



-- ===structure===
-- unit


return : a -> Parser e a
return a =
    make <| \s -> Ok ( s, a )



-- functor


map : (a -> b) -> Parser e a -> Parser e b
map f parser =
    make <|
        \s0 ->
            run parser s0
                |> Result.map (\( s1, a ) -> ( s1, f a ))



-- applicative


pair : Parser e a -> Parser e b -> Parser e ( a, b )
pair parser0 parser1 =
    make <|
        \s0 ->
            run parser0 s0
                |> Result.andThen
                    (\( s1, a ) ->
                        run parser1 s1
                            |> Result.map (\( s2, b ) -> ( s2, ( a, b ) ))
                    )


pairRightToLeft : Parser e a -> Parser e b -> Parser e ( a, b )
pairRightToLeft parser0 parser1 =
    pair parser1 parser0 |> map (\( b, a ) -> ( a, b ))


first : Parser e a -> Parser e b -> Parser e a
first parser0 parser1 =
    pair parser0 parser1
        |> map (\( a, _ ) -> a)


second : Parser e a -> Parser e b -> Parser e b
second parser0 parser1 =
    pair parser0 parser1
        |> map (\( _, b ) -> b)


mid : Parser e a -> Parser e b -> Parser e c -> Parser e b
mid parser0 parser1 parser2 =
    second parser0 (first parser1 parser2)


map2 : (a0 -> a1 -> b) -> Parser e a0 -> Parser e a1 -> Parser e b
map2 f parser0 parser1 =
    pair parser0 parser1
        |> map (\( a0, a1 ) -> f a0 a1)


map3 : (a0 -> a1 -> a2 -> b) -> Parser e a0 -> Parser e a1 -> Parser e a2 -> Parser e b
map3 f parser0 parser1 parser2 =
    pair parser0 (pair parser1 parser2)
        |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2)


map4 : (a0 -> a1 -> a2 -> a3 -> b) -> Parser e a0 -> Parser e a1 -> Parser e a2 -> Parser e a3 -> Parser e b
map4 f parser0 parser1 parser2 parser3 =
    pair parser0 (pair parser1 (pair parser2 parser3))
        |> map (\( a0, ( a1, ( a2, a3 ) ) ) -> f a0 a1 a2 a3)


map5 : (a0 -> a1 -> a2 -> a3 -> a4 -> b) -> Parser e a0 -> Parser e a1 -> Parser e a2 -> Parser e a3 -> Parser e a4 -> Parser e b
map5 f parser0 parser1 parser2 parser3 parser4 =
    pair parser0 (pair parser1 (pair parser2 (pair parser3 parser4)))
        |> map (\( a0, ( a1, ( a2, ( a3, a4 ) ) ) ) -> f a0 a1 a2 a3 a4)



-- TODO: what order should this be in?


andMap : Parser e (a -> b) -> Parser e a -> Parser e b
andMap parser_fn parser_a =
    pair parser_fn parser_a
        |> map (\( f, a ) -> f a)



-- monad


andThen : (a -> Parser e b) -> Parser e a -> Parser e b
andThen f parser =
    make <|
        \s0 ->
            run parser s0
                |> Result.andThen
                    (\( s1, a ) ->
                        run (f a) s1
                    )


join : Parser e (Parser e a) -> Parser e a
join parser_parser =
    parser_parser |> andThen identity



-- TODO: andThen3,4,5


andThen2 : (a0 -> a1 -> Parser e b) -> Parser e a0 -> Parser e a1 -> Parser e b
andThen2 f parser0 parser1 =
    pair parser0 parser1
        |> andThen (\( a, b ) -> f a b)



-- ===error===


fail : e -> Parser e a
fail error =
    make <| \_ -> Err error


mapError : (e1 -> e2) -> Parser e1 a -> Parser e2 a
mapError f parser0 =
    make <|
        \s ->
            run parser0 s
                |> Result.mapError f


throw : e -> Parser e a -> Parser e b
throw error parser =
    parser
        |> andThen (\_ -> fail error)



-- ===choice structure===


{-| This backtracks on failure.
p
|> ifSuccessIfError
(\\a -> ...)
(\\error -> ...)
-}
ifSuccessIfError : (a -> Parser f b) -> (e -> Parser f b) -> Parser e a -> Parser f b
ifSuccessIfError handleSuccess handleError parser =
    make <|
        \s0 ->
            case run parser s0 of
                Ok ( s1, a ) ->
                    run (handleSuccess a) s1

                Err error ->
                    -- notice the backtrack to s0
                    run (handleError error) s0


ifErrorIfSuccess : (e -> Parser f b) -> (a -> Parser f b) -> Parser e a -> Parser f b
ifErrorIfSuccess handleError handleSuccess =
    ifSuccessIfError handleSuccess handleError


or : Parser e a -> Parser e b -> Parser ( e, e ) (Either a b)
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


try : Parser e a -> Parser f ()
try parser =
    parser
        |> ifSuccessIfError
            (\_ -> return ())
            (\_ -> return ())


oneOf : List (Parser e a) -> Parser e a
oneOf parsers =
    Debug.todo ""



-- match2
--   (string "pair", \() -> pair)
--   (string "cons", \() -> cons)
--   (\pairError consError -> ...)


match2 : ( Parser e0 a0, a0 -> Parser f b ) -> ( Parser e1 a1, a1 -> Parser f b ) -> (e0 -> e1 -> Parser f b) -> Parser f b
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


match3 : ( Parser e0 a0, a0 -> Parser f b ) -> ( Parser e1 a1, a1 -> Parser f b ) -> ( Parser e2 a2, a2 -> Parser f b ) -> (e0 -> e1 -> e2 -> Parser f b) -> Parser f b
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


match : List ( Parser e a, a -> Parser f b ) -> (List e -> Parser f b) -> Parser f b
match initBranches combineErrors =
    let
        loop : List ( Parser e a, a -> Parser f b ) -> List e -> Parser f b
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



-- ===looping===


sequence : List (Parser e a) -> Parser e (List a)
sequence parsers0 =
    case parsers0 of
        [] ->
            return []

        parser :: parsers1 ->
            parser
                |> andThen
                    (\a -> map (\xs -> a :: xs) (sequence parsers1))



-- ===specifics===


end : Parser (Error ExpectedEndOfInput) ()
end =
    make <|
        \s ->
            State.end s |> Result.map (\() -> ( s, () ))


lazy : (() -> Parser e a) -> Parser e a
lazy parser =
    parser ()


anyChar : (Char -> Parser e a) -> Parser (Either (Error ExpectingNonEmptyInput) e) a
anyChar f =
    make <|
        \s0 ->
            case State.consumeAnyChar s0 of
                Ok ( s1, c ) ->
                    run (f c) s1
                        |> Result.mapError (\error -> Either.Right error)

                Err stateError ->
                    Err (Either.Left stateError)



-- TODO: Should I have the type `0 | 1 | 2 | ... | 9`?


digit =
    Debug.todo ""



-- TODO: Should I use `any`? Maybe I should use `anyOne`? What about while consumers? `allSatisfying` `asMuchAsPossible`... nice modalities


anyCharSatisfying : (Char -> Bool) -> Parser (State.Error State.CharFailedTest) Char
anyCharSatisfying test =
    make <|
        \s0 ->
            State.consumeAnyCharSatisfying test s0


string : String -> Parser (State.Error State.ExpectedString) ()
string strToBeMatched =
    make <|
        \s0 ->
            State.consumeString strToBeMatched s0
                |> Result.map (\s1 -> ( s1, () ))


allWhileTrue : (Char -> Bool) -> Parser e String
allWhileTrue test =
    make <|
        \s ->
            Ok (State.consumeWhileTrue test s)


allWhileSucceeds : Parser e a -> Parser e (List a)
allWhileSucceeds parser =
    make <|
        \init_s ->
            let
                loop : ( State, List a ) -> Result e ( State, List a )
                loop ( s0, reversed_xs ) =
                    case run parser s0 of
                        Ok ( s1, x ) ->
                            loop ( s1, x :: reversed_xs )

                        Err error ->
                            Ok ( s0, List.reverse reversed_xs )
            in
            loop ( init_s, [] )
