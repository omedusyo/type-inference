module Lib.State.StatefulReaderWithErr exposing
    ( StatefulReaderWithErr
    , andMap
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , andThen5
    , create
    , error
    , first
    , get
    , get0
    , join
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , mid
    , pair
    , pairRightToLeft
    , return
    , run
    , second
    , sequence
    , set
    , throw
    , update
    , update0
    , withReadOnly
    )

-- Use this when you have both read-only and mutable state
--   `r` is for read-only state
--   `s` is for the mutable state
--   `e` is the error type
--
-- For explanation of order of evaluation see the `StatefulWithErr.elm`


type alias StatefulReaderWithErr e r s a =
    r -> s -> Result e ( s, a )


create : (r -> s -> Result e ( s, a )) -> StatefulReaderWithErr e r s a
create f =
    f


get : (r -> s -> StatefulReaderWithErr e r s b) -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b
get f stateful_a =
    \r state0 ->
        case stateful_a r state0 of
            Ok ( state1, _ ) ->
                f r state1 r state1

            Err err ->
                Err err


get0 : (r -> s -> StatefulReaderWithErr e r s b) -> StatefulReaderWithErr e r s b
get0 f =
    \r state0 ->
        f r state0 r state0


withReadOnly : (r -> s -> r) -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s b
withReadOnly f stateful_a =
    \r0 state0 ->
        stateful_a (f r0 state0) state0


update : (r -> s -> s) -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s a
update nextState stateful_a0 =
    \r state0 ->
        case stateful_a0 r state0 of
            Ok ( state1, a ) ->
                Ok ( nextState r state1, a )

            Err err ->
                Err err


update0 : (r -> s -> s) -> StatefulReaderWithErr e r s ()
update0 f =
    \r state0 ->
        Ok ( f r state0, () )


set : s -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s a
set state stateful_a0 =
    \r state0 ->
        case stateful_a0 r state0 of
            Ok ( _, a ) ->
                Ok ( state, a )

            Err err ->
                Err err


run : StatefulReaderWithErr e r s a -> (r -> s -> Result e ( s, a ))
run stateful_a0 =
    stateful_a0



-- ===Unit===


return : a -> StatefulReaderWithErr e r s a
return a =
    \r state0 -> Ok ( state0, a )



-- ===Functor===


map : (a -> b) -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b
map f stateful_a0 =
    \r state0 ->
        case stateful_a0 r state0 of
            Ok ( state1, a ) ->
                Ok ( state1, f a )

            Err err ->
                Err err



-- ===Applicative===


pair : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s ( a, b )
pair stateful_a stateful_b =
    \r state0 ->
        case stateful_a r state0 of
            Ok ( state1, a ) ->
                case stateful_b r state1 of
                    Ok ( state2, b ) ->
                        Ok ( state2, ( a, b ) )

                    Err err ->
                        Err err

            Err err ->
                Err err


second : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s b
second stateful_a_ignored stateful_b =
    pair stateful_a_ignored stateful_b |> map (\( _, b ) -> b)


first : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s a
first stateful_a stateful_b_ignored =
    pair stateful_a stateful_b_ignored |> map (\( a, _ ) -> a)


mid : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s c -> StatefulReaderWithErr e r s b
mid stateful_a_ignored stateful_b stateful_c_ignored =
    second stateful_a_ignored (first stateful_b stateful_c_ignored)


pairRightToLeft : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b -> StatefulReaderWithErr e r s ( a, b )
pairRightToLeft stateful_a stateful_b =
    \r state0 ->
        case stateful_b r state0 of
            Ok ( state1, b ) ->
                case stateful_a r state1 of
                    Ok ( state2, a ) ->
                        Ok ( state2, ( a, b ) )

                    Err err ->
                        Err err

            Err err ->
                Err err


andMap : StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s (a -> b) -> StatefulReaderWithErr e r s b
andMap stateful_a stateful_f =
    \r state0 ->
        case stateful_f r state0 of
            Ok ( state1, f ) ->
                case stateful_a r state1 of
                    Ok ( state2, a ) ->
                        Ok ( state2, f a )

                    Err err ->
                        Err err

            Err err ->
                Err err


map2 : (a0 -> a1 -> b) -> StatefulReaderWithErr e r s a0 -> StatefulReaderWithErr e r s a1 -> StatefulReaderWithErr e r s b
map2 f stateful_a0 stateful_a1 =
    pair stateful_a0 stateful_a1
        |> map (\( a0, a1 ) -> f a0 a1)


map3 : (a0 -> a1 -> a2 -> b) -> StatefulReaderWithErr e r s a0 -> StatefulReaderWithErr e r s a1 -> StatefulReaderWithErr e r s a2 -> StatefulReaderWithErr e r s b
map3 f stateful_a0 stateful_a1 stateful_a2 =
    pair stateful_a0 (pair stateful_a1 stateful_a2)
        |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2)


map4 :
    (a0 -> a1 -> a2 -> a3 -> b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s a2
    -> StatefulReaderWithErr e r s a3
    -> StatefulReaderWithErr e r s b
map4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 =
    pair stateful_a0 (pair stateful_a1 (pair stateful_a2 stateful_a3))
        |> map (\( a0, ( a1, ( a2, a3 ) ) ) -> f a0 a1 a2 a3)


map5 :
    (a0 -> a1 -> a2 -> a3 -> a4 -> b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s a2
    -> StatefulReaderWithErr e r s a3
    -> StatefulReaderWithErr e r s a4
    -> StatefulReaderWithErr e r s b
map5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4 =
    pair stateful_a0 (pair stateful_a1 (pair stateful_a2 (pair stateful_a3 stateful_a4)))
        |> map (\( a0, ( a1, ( a2, ( a3, a4 ) ) ) ) -> f a0 a1 a2 a3 a4)


sequence : List (StatefulReaderWithErr e r s a) -> StatefulReaderWithErr e r s (List a)
sequence stateful_as0 =
    case stateful_as0 of
        [] ->
            return []

        stateful_a :: stateful_as1 ->
            pair stateful_a (sequence stateful_as1)
                |> map (\( a, as1 ) -> a :: as1)



-- ===Monad===


andThen : (a -> StatefulReaderWithErr e r s b) -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s b
andThen f stateful_a =
    \r state0 ->
        case stateful_a r state0 of
            Ok ( state1, a ) ->
                f a r state1

            Err err ->
                Err err


join : StatefulReaderWithErr e r s (StatefulReaderWithErr e r s a) -> StatefulReaderWithErr e r s a
join stateful_stateful_a =
    andThen (\x -> x) stateful_stateful_a


andThen2 :
    (a0 -> a1 -> StatefulReaderWithErr e r s b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s b
andThen2 f stateful_a0 stateful_a1 =
    join (map2 f stateful_a0 stateful_a1)


andThen3 :
    (a0 -> a1 -> a2 -> StatefulReaderWithErr e r s b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s a2
    -> StatefulReaderWithErr e r s b
andThen3 f stateful_a0 stateful_a1 stateful_a2 =
    join (map3 f stateful_a0 stateful_a1 stateful_a2)


andThen4 :
    (a0 -> a1 -> a2 -> a3 -> StatefulReaderWithErr e r s b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s a2
    -> StatefulReaderWithErr e r s a3
    -> StatefulReaderWithErr e r s b
andThen4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 =
    join (map4 f stateful_a0 stateful_a1 stateful_a2 stateful_a3)


andThen5 :
    (a0 -> a1 -> a2 -> a3 -> a4 -> StatefulReaderWithErr e r s b)
    -> StatefulReaderWithErr e r s a0
    -> StatefulReaderWithErr e r s a1
    -> StatefulReaderWithErr e r s a2
    -> StatefulReaderWithErr e r s a3
    -> StatefulReaderWithErr e r s a4
    -> StatefulReaderWithErr e r s b
andThen5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4 =
    join (map5 f stateful_a0 stateful_a1 stateful_a2 stateful_a3 stateful_a4)



-- ===Errors===


error : e -> StatefulReaderWithErr e r s a
error err =
    \_ _ -> Err err


throw : e -> StatefulReaderWithErr e r s a -> StatefulReaderWithErr e r s a
throw err0 stateful_a0 =
    \r state0 ->
        case stateful_a0 r state0 of
            Err err1 ->
                Err err1

            Ok _ ->
                Err err0


mapError : (e0 -> e1) -> StatefulReaderWithErr e0 r s a -> StatefulReaderWithErr e1 r s a
mapError f stateful_a0 =
    \r state0 ->
        case stateful_a0 r state0 of
            Ok ( state1, a ) ->
                Ok ( state1, a )

            Err err ->
                Err (f err)
