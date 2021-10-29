module Lib.State.Main exposing (..)

-- problem is that I can't really say that w is a monad...


type alias Monoid m =
    { mul : m -> m -> m, neutral : m }


mul_ : Monoid m -> m -> m -> m
mul_ structure =
    structure.mul


neutral_ : Monoid m -> m
neutral_ structure =
    structure.neutral


intAdditive : Monoid Int
intAdditive =
    { mul = \x y -> x + y
    , neutral = 0
    }


intMultiplicative : Monoid Int
intMultiplicative =
    { mul = \x y -> x * y
    , neutral = 1
    }


type alias Writer w a =
    -- Can this even be turned into a "monad" (modulo associativity/neutrality which I can't express in Elm)?
    Monoid w -> ( w, a )


return : a -> Writer w a
return x =
    \{ mul, neutral } ->
        ( neutral, x )


type alias Iso a b =
    { to : a -> b, from : b -> a }
