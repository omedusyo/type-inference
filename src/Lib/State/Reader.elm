module Lib.State.Reader exposing
    ( Reader
    , andMap
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , andThen5
    , create
    , join
    , map
    , map2
    , map3
    , map4
    , map5
    , mapState
    , pair
    , read
    , return
    , run
    , sequence
    , withState
    )


type alias Reader r a =
    r -> a


create : (r -> a) -> Reader r a
create f =
    f


read : (r -> Reader r a) -> Reader r a
read f =
    \readOnlyState ->
        f readOnlyState readOnlyState


withState : r -> Reader r a -> Reader r a
withState newState reader_a =
    \_ ->
        reader_a newState


mapState : (r1 -> r0) -> Reader r0 a -> Reader r1 a
mapState f reader_a =
    \state1 -> reader_a (f state1)


run : r -> Reader r a -> a
run state reader_a =
    reader_a state



-- ===Unit===


return : a -> Reader r a
return a =
    \_ -> a



-- ===Functor===


map : (a -> b) -> Reader r a -> Reader r b
map f reader_a =
    \state -> f (reader_a state)



-- ===Applicative===


pair : Reader r a -> Reader r b -> Reader r ( a, b )
pair reader_a reader_b =
    \state ->
        ( reader_a state, reader_b state )


andMap : Reader r a -> Reader r (a -> b) -> Reader r b
andMap reader_a reader_f =
    \state ->
        reader_f state (reader_a state)


map2 :
    (a0 -> a1 -> b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r b
map2 f reader_a0 reader_a1 =
    \state ->
        f (reader_a0 state) (reader_a1 state)


map3 :
    (a0 -> a1 -> a2 -> b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r b
map3 f reader_a0 reader_a1 reader_a2 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state)


map4 :
    (a0 -> a1 -> a2 -> a3 -> b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r a3
    -> Reader r b
map4 f reader_a0 reader_a1 reader_a2 reader_a3 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state) (reader_a3 state)


map5 :
    (a0 -> a1 -> a2 -> a3 -> a4 -> b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r a3
    -> Reader r a4
    -> Reader r b
map5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state) (reader_a3 state) (reader_a4 state)


sequence : List (Reader r a) -> Reader r (List a)
sequence reader_as0 =
    \state ->
        case reader_as0 of
            [] ->
                []

            reader_a :: reader_as1 ->
                Debug.todo ""



-- ===Monad===


andThen : (a -> Reader r b) -> Reader r a -> Reader r b
andThen f reader_a =
    \state ->
        f (reader_a state) state


join : Reader r (Reader r a) -> Reader r a
join reader_reader_a =
    -- note that this happens to be the same as `read`
    \state ->
        reader_reader_a state state


andThen2 :
    (a0 -> a1 -> Reader r b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r b
andThen2 f reader_a0 reader_a1 =
    join (map2 f reader_a0 reader_a1)


andThen3 :
    (a0 -> a1 -> a2 -> Reader r b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r b
andThen3 f reader_a0 reader_a1 reader_a2 =
    join (map3 f reader_a0 reader_a1 reader_a2)


andThen4 :
    (a0 -> a1 -> a2 -> a3 -> Reader r b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r a3
    -> Reader r b
andThen4 f reader_a0 reader_a1 reader_a2 reader_a3 =
    join (map4 f reader_a0 reader_a1 reader_a2 reader_a3)


andThen5 :
    (a0 -> a1 -> a2 -> a3 -> a4 -> Reader r b)
    -> Reader r a0
    -> Reader r a1
    -> Reader r a2
    -> Reader r a3
    -> Reader r a4
    -> Reader r b
andThen5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4 =
    join (map5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4)
