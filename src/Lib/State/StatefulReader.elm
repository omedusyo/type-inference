module Lib.State.StatefulReader exposing
    ( StatefulReader
    , andThen
    , andThen2
    , andThen3
    , get
    , get0
    , join
    , make
    , map
    , map2
    , map3
    , ooo
    , read
    , return
    , run
    , sequence
    , set
    , tuple2
    , tuple3
    , update
    , update0
    )


type alias StatefulReader r s a =
    r -> s -> ( s, a )


make : (r -> s -> ( s, a )) -> StatefulReader r s a
make f =
    f


run : StatefulReader r s a -> (r -> s -> ( s, a ))
run stateful_a =
    stateful_a


get : (r -> s -> StatefulReader r s b) -> StatefulReader r s a -> StatefulReader r s b
get f stateful_a =
    make <|
        \r s0 ->
            let
                ( s1, a ) =
                    run stateful_a r s0
            in
            run (f r s1) r s1


get0 : (r -> s -> StatefulReader r s b) -> StatefulReader r s b
get0 f =
    make <|
        \r s ->
            run (f r s) r s


read : (r -> StatefulReader r s a) -> StatefulReader r s a
read f =
    make <|
        \r s ->
            run (f r) r s


push : (r1 -> r2) -> StatefulReader r2 s a -> StatefulReader r1 s a
push f stateful_a =
    make <|
        \r s ->
            run stateful_a (f r) s


set : s -> StatefulReader r s a -> StatefulReader r s a
set state stateful_a0 =
    make <|
        \r s0 ->
            let
                ( s1, a ) =
                    run stateful_a0 r s0
            in
            ( state, a )


update : (r -> s -> s) -> StatefulReader r s a -> StatefulReader r s a
update nextState stateful_a0 =
    make <|
        \r s0 ->
            let
                ( s1, a ) =
                    run stateful_a0 r s0
            in
            ( nextState r s1, a )


update0 : (r -> s -> s) -> StatefulReader r s ()
update0 f =
    \r s0 ->
        ( f r s0, () )



-- ===Unit===


return : a -> StatefulReader r s a
return a =
    make <|
        \r s -> ( s, a )


identity : StatefulReader r s (a -> a)
identity =
    return (\x -> x)



-- ===Functor===


map : (a -> b) -> StatefulReader r s a -> StatefulReader r s b
map f stateful_a =
    make <|
        \r s0 ->
            let
                ( s1, a ) =
                    run stateful_a r s0
            in
            ( s1, f a )



-- ===Applicative===


tuple2 : StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s ( a0, a1 )
tuple2 stateful_a0 stateful_a1 =
    make <|
        \r s0 ->
            let
                ( s1, a0 ) =
                    run stateful_a0 r s0

                ( s2, a1 ) =
                    run stateful_a1 r s1
            in
            ( s2, ( a0, a1 ) )


tuple3 : StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s a2 -> StatefulReader r s ( a0, ( a1, a2 ) )
tuple3 stateful_a0 stateful_a1 stateful_a2 =
    make <|
        \r s0 ->
            let
                ( s1, a0 ) =
                    run stateful_a0 r s0

                ( s2, a1 ) =
                    run stateful_a1 r s1

                ( s3, a2 ) =
                    run stateful_a2 r s2
            in
            ( s2, ( a0, ( a1, a2 ) ) )


map2 : (a0 -> a1 -> b) -> StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s b
map2 f stateful_a0 stateful_a1 =
    tuple2 stateful_a0 stateful_a1 |> map (\( a0, a1 ) -> f a0 a1)


map3 : (a0 -> a1 -> a2 -> b) -> StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s a2 -> StatefulReader r s b
map3 f stateful_a0 stateful_a1 stateful_a2 =
    tuple3 stateful_a0 stateful_a1 stateful_a2 |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2)



--   stateful_a |> StatefulReader.o stateful_b
-- First eval `stateful_a ~> a`, then `stateful_b ~> b`, then return `a`


o : StatefulReader r s b -> StatefulReader r s a -> StatefulReader r s a
o stateful_b stateful_a =
    tuple2 stateful_a stateful_b |> map Tuple.first



-- This is `andMap`
--   stateful_f |> StatefulReader.ooo stateful_a
-- First eval `stateful_f ~> f`, then `stateful_a ~> a`, then return `f a`


ooo : StatefulReader r s a -> StatefulReader r s (a -> b) -> StatefulReader r s b
ooo stateful_a stateful_f =
    tuple2 stateful_f stateful_a
        |> map (\( f, a ) -> f a)


sequence : List (StatefulReader r s a) -> StatefulReader r s (List a)
sequence stateful_as0 =
    case stateful_as0 of
        [] ->
            return []

        stateful_a :: stateful_as1 ->
            tuple2 stateful_a (sequence stateful_as1)
                |> map (\( a, as1 ) -> a :: as1)



-- ===Monad===


andThen : (a -> StatefulReader r s b) -> StatefulReader r s a -> StatefulReader r s b
andThen f stateful_a =
    make <|
        \r s0 ->
            let
                ( s1, a ) =
                    run stateful_a r s0
            in
            f a r s1


join : StatefulReader r s (StatefulReader r s a) -> StatefulReader r s a
join stateful_stateful_a =
    andThen (\x -> x) stateful_stateful_a


andThen2 : (a0 -> a1 -> StatefulReader r s b) -> StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s b
andThen2 f stateful_a0 stateful_a1 =
    tuple2 stateful_a0 stateful_a1 |> map (\( a0, a1 ) -> f a0 a1) |> join


andThen3 : (a0 -> a1 -> a2 -> StatefulReader r s b) -> StatefulReader r s a0 -> StatefulReader r s a1 -> StatefulReader r s a2 -> StatefulReader r s b
andThen3 f stateful_a0 stateful_a1 stateful_a2 =
    tuple3 stateful_a0 stateful_a1 stateful_a2 |> map (\( a0, ( a1, a2 ) ) -> f a0 a1 a2) |> join
