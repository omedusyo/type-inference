module Lib.Parser.Parser exposing
    ( Parser
    , allSatisfying
    , allUntil
    , andMap
    , andThen
    , andThen2
    , anyChar
    , anyCharSatisfying
    , fail
    , first
    , get0
    , join
    , make
    , map
    , map2
    , map3
    , map4
    , map5
    , mid
    , pair
    , pairRightToLeft
    , return
    , run
    , second
    , sequence
    , string
    , throw
    , try
    )

import Either exposing (Either)
import Lib.Parser.State as State exposing (State)


type alias Parser e a =
    State -> Result e ( State, a )



-- ===base===


run : Parser e a -> State -> Result e ( State, a )
run parser =
    parser


make : (State -> Result e ( State, a )) -> Parser e a
make f =
    f


get0 : (State -> Parser e a) -> Parser e a
get0 f =
    make <|
        \s -> run (f s) s



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
-- TODO: I don't like the use of `combineError` here
--       it would be nice if the return type would be something like `Parser (e1 | e2 | (e1, e2))`
--       so that the client doesn't need to deal with the error handling immediately


try : Parser e a -> Parser e b -> (e -> e -> e) -> Parser e (Either a b)
try parser0 parser1 combineErrors =
    make <|
        \s ->
            case run parser0 s of
                Ok ( s_a, a ) ->
                    Ok ( s_a, Either.Left a )

                Err error_a ->
                    case run parser1 s of
                        Ok ( s_b, b ) ->
                            Ok ( s_b, Either.Right b )

                        Err error_b ->
                            Err (combineErrors error_a error_b)



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


anyChar : (Char -> Parser e a) -> Parser (Either (State.Error State.EmptyInput) e) a
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


anyDigit =
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


allSatisfying : (Char -> Bool) -> Parser e String
allSatisfying test =
    make <|
        \s ->
            Ok (State.consumeWhile test s)


allUntil : Parser e a -> Parser e (List a)
allUntil parser =
    make <|
        \s0 ->
            Debug.todo ""
