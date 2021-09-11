module Example exposing (..)

import Main exposing (..)



-- examples


nt =
    intToNatTerm


x =
    VarUse "X"


y =
    VarUse "Y"


n0 =
    NatZero


n1 =
    NatSucc n0


n2 =
    NatSucc n1


n3 =
    NatSucc n2


n4 =
    NatSucc n3


pair0 =
    -- (x, y)
    Pair x y


pair1 =
    -- (x, x)
    Pair x x


id =
    -- \x -> x
    Abstraction "x" (VarUse "x")


dup =
    -- \x -> (x, x)
    Abstraction "x" (Pair (VarUse "x") (VarUse "x"))


twist =
    --  \tuple -> (tuple.snd, tuple.fst)
    Abstraction
        "tuple"
        (Pair (Snd (VarUse "tuple")) (Fst (VarUse "tuple")))


apply =
    --  \f x -> f x
    Abstraction "f"
        (Abstraction "x"
            (Application (VarUse "f") (VarUse "x"))
        )


pairing =
    -- \x y -> (x, y)
    Abstraction "x"
        (Abstraction "y"
            (Pair (VarUse "x") (VarUse "y"))
        )


curry =
    -- \f x y -> f (x, y)
    Abstraction "f"
        (Abstraction "x"
            (Abstraction "y"
                (Application
                    (VarUse "f")
                    (Pair (VarUse "x") (VarUse "y"))
                )
            )
        )


uncurry =
    -- \f p -> (f (fst p)) (snd p)
    Abstraction "f"
        (Abstraction "p"
            (Application
                (Application
                    (VarUse "f")
                    (Fst (VarUse "p"))
                )
                (Snd (VarUse "p"))
            )
        )


apply0 =
    Application apply dup


apply1 =
    Application apply0 n0


evalAt0 =
    Abstraction "f" (Application (VarUse "f") n0)


succ0 =
    Abstraction "n" (NatSucc (VarUse "n"))


case0 =
    -- \z -> case z of Left x -> x + 1, Right y -> y
    Abstraction "z"
        (Case
            { arg = VarUse "z"
            , leftVar = "x"
            , leftBody = NatSucc (VarUse "x")
            , rightVar = "y"
            , rightBody = VarUse "y"
            }
        )


add =
    -- Addition
    -- \x y -> x + y
    Abstraction "x"
        (Abstraction "y"
            (NatLoop
                { base = VarUse "x"
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = NatSucc (VarUse "s")
                    }
                , arg = VarUse "y"
                }
            )
        )


mul =
    -- Multiplication
    -- \x y -> x * y
    Abstraction "x"
        (Abstraction "y"
            (NatLoop
                { base = n0
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = Application (Application add (VarUse "x")) (VarUse "s")
                    }
                , arg = VarUse "y"
                }
            )
        )


exp =
    -- Multiplication
    -- \x y -> x ^ y
    Abstraction "x"
        (Abstraction "y"
            (NatLoop
                { base = n1
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = Application (Application mul (VarUse "x")) (VarUse "s")
                    }
                , arg = VarUse "y"
                }
            )
        )


sumTerm =
    -- Sum
    -- \N -> (N - 1) + ... + 2 + 3 + 1 + 0
    Abstraction "N"
        (NatLoop
            { base = n0
            , loop =
                { indexVar = "j"
                , stateVar = "sum"
                , body = Application (Application add (VarUse "j")) (VarUse "sum")
                }
            , arg = VarUse "N"
            }
        )


selfApply =
    -- \f -> f f
    Abstraction "f" (Application (VarUse "f") (VarUse "f"))
