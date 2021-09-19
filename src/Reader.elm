module Reader exposing
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


type alias Reader s a =
    s -> a


create : (s -> a) -> Reader s a
create f =
    f


read : (s -> Reader s a) -> Reader s a
read f =
    \state ->
        f state state


withState : s -> Reader s a -> Reader s a
withState newState reader_a =
    \_ ->
        reader_a newState


mapState : (s1 -> s0) -> Reader s0 a -> Reader s1 a
mapState f reader_a =
    \state1 -> reader_a (f state1)


run : s -> Reader s a -> a
run state reader_a =
    reader_a state



-- ===Unit===


return : a -> Reader s a
return a =
    \_ -> a



-- ===Functor===


map : (a -> b) -> Reader s a -> Reader s b
map f reader_a =
    \state -> f (reader_a state)



-- ===Applicative===


pair : Reader s a -> Reader s b -> Reader s ( a, b )
pair reader_a reader_b =
    \state ->
        ( reader_a state, reader_b state )


andMap : Reader s a -> Reader s (a -> b) -> Reader s b
andMap reader_a reader_f =
    \state ->
        reader_f state (reader_a state)


map2 : (a0 -> a1 -> b) -> Reader s a0 -> Reader s a1 -> Reader s b
map2 f reader_a0 reader_a1 =
    \state ->
        f (reader_a0 state) (reader_a1 state)


map3 : (a0 -> a1 -> a2 -> b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s b
map3 f reader_a0 reader_a1 reader_a2 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state)


map4 : (a0 -> a1 -> a2 -> a3 -> b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s a3 -> Reader s b
map4 f reader_a0 reader_a1 reader_a2 reader_a3 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state) (reader_a3 state)


map5 : (a0 -> a1 -> a2 -> a3 -> a4 -> b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s a3 -> Reader s a4 -> Reader s b
map5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4 =
    \state ->
        f (reader_a0 state) (reader_a1 state) (reader_a2 state) (reader_a3 state) (reader_a4 state)


sequence : List (Reader s a) -> Reader s (List a)
sequence reader_as0 =
    \state ->
        case reader_as0 of
            [] ->
                []

            reader_a :: reader_as1 ->
                Debug.todo ""



-- ===Monad===


andThen : (a -> Reader s b) -> Reader s a -> Reader s b
andThen f reader_a =
    \state ->
        f (reader_a state) state


join : Reader s (Reader s a) -> Reader s a
join reader_reader_a =
    \state ->
        reader_reader_a state state


andThen2 : (a0 -> a1 -> Reader s b) -> Reader s a0 -> Reader s a1 -> Reader s b
andThen2 f reader_a0 reader_a1 =
    join (map2 f reader_a0 reader_a1)


andThen3 : (a0 -> a1 -> a2 -> Reader s b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s b
andThen3 f reader_a0 reader_a1 reader_a2 =
    join (map3 f reader_a0 reader_a1 reader_a2)


andThen4 : (a0 -> a1 -> a2 -> a3 -> Reader s b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s a3 -> Reader s b
andThen4 f reader_a0 reader_a1 reader_a2 reader_a3 =
    join (map4 f reader_a0 reader_a1 reader_a2 reader_a3)


andThen5 : (a0 -> a1 -> a2 -> a3 -> a4 -> Reader s b) -> Reader s a0 -> Reader s a1 -> Reader s a2 -> Reader s a3 -> Reader s a4 -> Reader s b
andThen5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4 =
    join (map5 f reader_a0 reader_a1 reader_a2 reader_a3 reader_a4)
