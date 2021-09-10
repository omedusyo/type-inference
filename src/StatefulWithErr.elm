module StatefulWithErr exposing
    ( StatefulWithErr
    , andMap
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , andThen5
    , blank
    , error
    , first
    , get
    , get0
    , ifThenElse
    , join
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , pair
    , pairRightToLeft
    , return
    , run
    , second
    , sequence
    , set
    , throw
    , try
    , tryMapError
    , until
    , update
    , update0
    , while
    )

-- `e` is the error type
-- `s` is the state type
--
--   Stateful e s : Type -> Type
-- has the monad structure


type alias StatefulWithErr e s a =
    s -> Result e ( s, a )



-- example use
--  stateful_a0
--  |> get (\state0 -> stateful_a1)
--
-- first executes stateful_a0, discards the result
-- then executes stateful_a1 while making the current state availiable to it


get : (s -> StatefulWithErr e s b) -> StatefulWithErr e s a -> StatefulWithErr e s b
get stateful_a1 stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( state1, _ ) ->
                stateful_a1 state1 state1

            Err err ->
                Err err


get0 : (s -> StatefulWithErr e s b) -> StatefulWithErr e s b
get0 f =
    \state0 ->
        f state0 state0



-- example use
-- stateful_a0
-- |> update (\s -> s')
--
-- first executes `stateful_a0`
-- then changes the state


update : (s -> s) -> StatefulWithErr e s a -> StatefulWithErr e s a
update nextState stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( state1, a ) ->
                Ok ( nextState state1, a )

            Err err ->
                Err err


update0 : (s -> s) -> StatefulWithErr e s ()
update0 f =
    \state0 ->
        Ok ( f state0, () )


set : s -> StatefulWithErr e s a -> StatefulWithErr e s a
set state stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( _, a ) ->
                Ok ( state, a )

            Err err ->
                Err err



-- example use
--   state0
--   |> run
--     (...stateful_a0...)


run : StatefulWithErr e s a -> s -> Result e ( s, a )
run stateful_a0 =
    stateful_a0



-- ===Unit===


return : a -> StatefulWithErr e s a
return a =
    \state0 -> Ok ( state0, a )



-- ===Functor===
-- example use
--   stateful_a0
--   |> update (\s -> s')
--
-- first executes `stateful_a0`
-- then changes the value


map : (a -> b) -> StatefulWithErr e s a -> StatefulWithErr e s b
map f stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( state1, a ) ->
                Ok ( state1, f a )

            Err err ->
                Err err



-- ===Applicative===
--   pair stateful_a0 stateful_a1
-- first executes stateful_a which results into the value a
-- then executes stateful_b which results into the value b
-- the result is (a, b)
--
-- WARNING
-- I don't recommend writing
--   stateful_a0
--   |> pair stateful_a1
-- because the flow of the text suggests that the effects of `stateful_a0` will be evaluated first, but this is NOT the case
-- just write
--   pair
--     stateful_a0
--     stateful_a1


pair : StatefulWithErr e s a -> StatefulWithErr e s b -> StatefulWithErr e s ( a, b )
pair stateful_a stateful_b =
    \state0 ->
        case stateful_a state0 of
            Ok ( state1, a ) ->
                case stateful_b state1 of
                    Ok ( state2, b ) ->
                        Ok ( state2, ( a, b ) )

                    Err err ->
                        Err err

            Err err ->
                Err err


blank : StatefulWithErr e s ()
blank =
    return ()



-- exmaple use
--   semicolon
--     stateful_a_to_be_ignored
--     stateful_b


second : StatefulWithErr e s a -> StatefulWithErr e s b -> StatefulWithErr e s b
second stateful_a stateful_b =
    pair stateful_a stateful_b |> map (\( _, b ) -> b)


first : StatefulWithErr e s a -> StatefulWithErr e s b -> StatefulWithErr e s a
first stateful_a stateful_b =
    pair stateful_a stateful_b |> map (\( a, _ ) -> a)



--   pairRightToLeft stateful_a0 stateful_a1
-- first executes `stateful_b` which results into the value b
-- then executes `stateful_a` which results into the value a
-- the result is (a, b)
--
-- Note
--   pairRightToLeft stateful_a stateful_b == pair stateful_b stateful_a |> map (\(b, a) -> (a, b))
-- the only difference is the order of side-effects.


pairRightToLeft : StatefulWithErr e s a -> StatefulWithErr e s b -> StatefulWithErr e s ( a, b )
pairRightToLeft stateful_a stateful_b =
    \state0 ->
        case stateful_b state0 of
            Ok ( state1, b ) ->
                case stateful_a state1 of
                    Ok ( state2, a ) ->
                        Ok ( state2, ( a, b ) )

                    Err err ->
                        Err err

            Err err ->
                Err err



-- example
--  return (\a b c -> f a b c )
--  |> andMap stateful_a
--  |> andMap stateful_b
--  |> andMap stateful_c
--
-- first  executes `stateful_a` which results in `a0`
-- second executes `stateful_b` which results in `b0`
-- third  executes `stateful_c` which results in `c0`
-- the result is `f a0 b0 c0`
--
-- WARNING:
-- Note that
--   andMap stateful_a stateful_f
--  first executes `stateful_f`, then `stateful_a`
--  that is
--    andMap stateful_a stateful_f == pair stateful_f stateful_a |> map (\(f, a) -> f a)
--    andMap stateful_a stateful_f != pairRightToLeft stateful_a stateful_f |> map (\(a, f) -> f a)


andMap : StatefulWithErr e s a -> StatefulWithErr e s (a -> b) -> StatefulWithErr e s b
andMap stateful_a stateful_f =
    \state0 ->
        case stateful_f state0 of
            Ok ( state1, f ) ->
                case stateful_a state1 of
                    Ok ( state2, a ) ->
                        Ok ( state2, f a )

                    Err err ->
                        Err err

            Err err ->
                Err err


map2 : (a0 -> a1 -> b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s b
map2 f stateful_a0 stateful_a1 =
    pair stateful_a0 stateful_a1
        |> map (\( a0, a1 ) -> f a0 a1)


map3 : (a0 -> a1 -> a2 -> b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s a2 -> StatefulWithErr e s b
map3 f stateful_a0 stateful_a1 stateful_a2 =
    pair stateful_a0 (pair stateful_a1 stateful_a2)
        |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2)


map4 :
    (a0 -> a1 -> a2 -> a3 -> b)
    -> StatefulWithErr e s a0
    -> StatefulWithErr e s a1
    -> StatefulWithErr e s a2
    -> StatefulWithErr e s a3
    -> StatefulWithErr e s b
map4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 =
    pair stateful_a0 (pair stateful_a1 (pair stateful_a2 stateful_a3))
        |> map (\( a0, ( a1, ( a2, a3 ) ) ) -> f a0 a1 a2 a3)


map5 :
    (a0 -> a1 -> a2 -> a3 -> a4 -> b)
    -> StatefulWithErr e s a0
    -> StatefulWithErr e s a1
    -> StatefulWithErr e s a2
    -> StatefulWithErr e s a3
    -> StatefulWithErr e s a4
    -> StatefulWithErr e s b
map5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4 =
    pair stateful_a0 (pair stateful_a1 (pair stateful_a2 (pair stateful_a3 stateful_a4)))
        |> map (\( a0, ( a1, ( a2, ( a3, a4 ) ) ) ) -> f a0 a1 a2 a3 a4)



-- example
--   sequence
--     [ stateful_a0
--     , stateful_a1
--     , ...
--     ]


sequence : List (StatefulWithErr e s a) -> StatefulWithErr e s (List a)
sequence stateful_as0 =
    case stateful_as0 of
        [] ->
            return []

        stateful_a :: stateful_as1 ->
            pair stateful_a (sequence stateful_as1)
                |> map (\( a, as1 ) -> a :: as1)



-- ===Monad===


andThen : (a -> StatefulWithErr e s b) -> StatefulWithErr e s a -> StatefulWithErr e s b
andThen f stateful_a =
    \state0 ->
        case stateful_a state0 of
            Ok ( state1, a ) ->
                f a state1

            Err err ->
                Err err


join : StatefulWithErr e s (StatefulWithErr e s a) -> StatefulWithErr e s a
join stateful_stateful_a =
    andThen (\x -> x) stateful_stateful_a


andThen2 : (a0 -> a1 -> StatefulWithErr e s b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s b
andThen2 f stateful_a0 stateful_a1 =
    join (map2 f stateful_a0 stateful_a1)


andThen3 : (a0 -> a1 -> a2 -> StatefulWithErr e s b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s a2 -> StatefulWithErr e s b
andThen3 f stateful_a0 stateful_a1 stateful_a2 =
    join (map3 f stateful_a0 stateful_a1 stateful_a2)


andThen4 : (a0 -> a1 -> a2 -> a3 -> StatefulWithErr e s b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s a2 -> StatefulWithErr e s a3 -> StatefulWithErr e s b
andThen4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 =
    join (map4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3)


andThen5 : (a0 -> a1 -> a2 -> a3 -> a4 -> StatefulWithErr e s b) -> StatefulWithErr e s a0 -> StatefulWithErr e s a1 -> StatefulWithErr e s a2 -> StatefulWithErr e s a3 -> StatefulWithErr e s a4 -> StatefulWithErr e s b
andThen5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4 =
    join (map5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4)



-- ===Errors===


error : e -> StatefulWithErr e s a
error err =
    \_ -> Err err



-- stateful_a0
-- |> throw e
--
-- first executes `stateful_a0
-- and if that succeeds,
-- the result fails with `e`


throw : e -> StatefulWithErr e s a -> StatefulWithErr e s a
throw err0 stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Err err1 ->
                Err err1

            Ok _ ->
                Err err0


mapError : (e0 -> e1) -> StatefulWithErr e0 s a -> StatefulWithErr e1 s a
mapError f stateful_a0 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( state1, a ) ->
                Ok ( state1, a )

            Err err ->
                Err (f err)



-- ===BRANCHING constructs===
--   stateful_a
--   |> ifThenElse
--         stateful_bool
--         stateful_b0
--         stateful_b1
-- first execute stateful_a, then ignore the result
-- then execute stateful_bool
-- if that's True, execute stateful_b
-- otherwise execute stateful_c


ifThenElse : StatefulWithErr e s Bool -> StatefulWithErr e s b -> StatefulWithErr e s b -> StatefulWithErr e s a -> StatefulWithErr e s b
ifThenElse stateful_bool stateful_b0 stateful_b1 stateful_a =
    \state0 ->
        case stateful_a state0 of
            Ok ( state1, _ ) ->
                case stateful_bool state1 of
                    Ok ( state2, b ) ->
                        if b then
                            stateful_b0 state2

                        else
                            stateful_b1 state2

                    Err err ->
                        Err err

            Err err ->
                Err err



-- ===WHILE-LOOP like constructs===
--   stateful_before_loop
--   |> while (\state -> ... condition ...)
--        stateful_loop_body
-- executes `stateful_before_loop`, which results in `a0`
-- then checks if the state satisfies the test (if it doesn't, the result is a0)
-- if it does, it executes `stateful_loop_body`, which results in `a1`)
-- then checks if the state satisfies the test (if it doesn't, the result is `a1`)
-- if it does, it executes `stateful_loop_body`, which results in `a2`)
-- then checks if the state satisfies the test (if it doesn't, the result is `a2`)
--   ...


while : (s -> Bool) -> StatefulWithErr e s a -> StatefulWithErr e s a -> StatefulWithErr e s a
while test stateful_loopBody stateful_beforeLoop =
    \state0 ->
        case stateful_beforeLoop state0 of
            Ok ( state1, a0 ) ->
                if test state1 then
                    (stateful_loopBody
                        |> while test stateful_loopBody
                    )
                        state1

                else
                    Ok ( state1, a0 )

            Err err ->
                Err err



-- example use
--   stateful_loopBody
--   |> until test
-- first executes the body of the loop which results into a0
-- then checks if the state satisfies the test (if it does, the result is a0)
-- otherwise executes the loop body again which results into a1
-- then checks if the state satisfies the test (if it does, the result is a1)
--    ...


until : (s -> Bool) -> StatefulWithErr e s a -> StatefulWithErr e s a
until test stateful_loopBody =
    \state0 ->
        case stateful_loopBody state0 of
            Ok ( state1, a0 ) ->
                if test state1 then
                    Ok ( state1, a0 )

                else
                    (stateful_loopBody
                        |> until test
                    )
                        state1

            Err err ->
                Err err



-- ===TIME-TRAVELING constructs ===


try : StatefulWithErr e s a -> StatefulWithErr e s a -> StatefulWithErr e s a
try stateful_a0 stateful_a1 =
    \state0 ->
        case stateful_a0 state0 of
            Ok ( state1, a0 ) ->
                Ok ( state1, a0 )

            Err err ->
                stateful_a1 state0


tryMapError : (e0 -> e1 -> e) -> StatefulWithErr e0 s a -> StatefulWithErr e1 s a -> StatefulWithErr e s a
tryMapError f stateful_a0 stateful_a1 =
    \state ->
        case stateful_a0 state of
            Ok ( state0, a0 ) ->
                Ok ( state0, a0 )

            Err err0 ->
                case stateful_a1 state of
                    Ok ( state1, a1 ) ->
                        Ok ( state1, a1 )

                    Err err1 ->
                        Err (f err0 err1)
