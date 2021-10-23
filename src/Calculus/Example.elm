module Calculus.Example exposing (..)

import Calculus.Base as Base exposing (Interface, ModuleLiteral)
import Calculus.Type.Inference as TypeInference



-- examples


x =
    Base.VarUse "X"


xType =
    -- '0
    TypeInference.infer0 x


y =
    Base.VarUse "Y"


yType =
    -- '0
    TypeInference.infer0 y


n0 =
    Base.ConstZero


n0Type =
    -- Nat
    TypeInference.infer0 n0


n1 =
    Base.Succ n0


n2 =
    Base.Succ n1


n3 =
    Base.Succ n2


n4 =
    Base.Succ n3


pair0 =
    -- (x, y)
    Base.Pair x y


pair0Type =
    -- ('0, '1)
    TypeInference.infer0 pair0


pair1 =
    -- (x, x)
    Base.Pair x x


pair1Type =
    -- ('0, '0)
    TypeInference.infer0 pair1


matchProduct0 =
    Base.MatchProduct { arg = Base.Pair n0 n1, var0 = "x", var1 = "y", body = Base.VarUse "x" }


matchProduct0Type =
    TypeInference.infer0 matchProduct0


id =
    -- \x -> x
    Base.Abstraction "x" (Base.VarUse "x")


idType =
    -- '0 -> '0
    TypeInference.infer0 id


dup =
    -- \x -> (x, x)
    Base.Abstraction "x" (Base.Pair (Base.VarUse "x") (Base.VarUse "x"))


dupType =
    -- '0 -> ('0, '0)
    TypeInference.infer0 dup


twist =
    -- \p -> (match-pair p { (pair x y) . (pair y x) })
    Base.Abstraction
        "p"
        (Base.MatchProduct
            { arg = Base.VarUse "p"
            , var0 = "x"
            , var1 = "y"
            , body = Base.Pair (Base.VarUse "y") (Base.VarUse "x")
            }
        )


twistType =
    -- ('0, '1) -> ('1, '0)
    TypeInference.infer0 twist


apply =
    --  \f x -> f x
    Base.Abstraction "f"
        (Base.Abstraction "x"
            (Base.Application (Base.VarUse "f") (Base.VarUse "x"))
        )


applyType =
    -- ('0 -> '1) -> '0 -> '1
    TypeInference.infer0 apply


pairing =
    -- \x y -> (x, y)
    Base.Abstraction "x"
        (Base.Abstraction "y"
            (Base.Pair (Base.VarUse "x") (Base.VarUse "y"))
        )


pairingType =
    -- '0 -> '1 -> ('0, '1)
    TypeInference.infer0 pairing


curry =
    -- \f x y -> f (x, y)
    Base.Abstraction "f"
        (Base.Abstraction "x"
            (Base.Abstraction "y"
                (Base.Application
                    (Base.VarUse "f")
                    (Base.Pair (Base.VarUse "x") (Base.VarUse "y"))
                )
            )
        )


curryType =
    -- (('0, '1) -> '2) -> '0 -> '1 -> '2
    TypeInference.infer0 curry


uncurry =
    -- (fn { f p . (match-pair p { x y . (@ f x y) }) })
    Base.Abstraction "f"
        (Base.Abstraction "p"
            (Base.MatchProduct
                { arg = Base.VarUse "p"
                , var0 = "x"
                , var1 = "y"
                , body = Base.Application (Base.Application (Base.VarUse "f") (Base.VarUse "x")) (Base.VarUse "y")
                }
            )
        )


uncurryType =
    -- ('0 -> '1 -> '2) -> ('0, '1) -> '2
    TypeInference.infer0 uncurry


apply0 =
    Base.Application (Base.Application apply dup) n0


apply0Type =
    -- (Nat, Nat)
    TypeInference.infer0 apply0


evalAt0 =
    -- \f -> f 0
    Base.Abstraction "f" (Base.Application (Base.VarUse "f") n0)


evalAt0Type =
    -- (Nat -> '0) -> '0
    TypeInference.infer0 evalAt0


succ0 =
    -- \n -> S n
    Base.Abstraction "n" (Base.Succ (Base.VarUse "n"))


succ0Type =
    -- Nat -> Nat
    TypeInference.infer0 succ0


case0 =
    -- \z -> case z of Left x -> x + 1, Right y -> y
    Base.Abstraction "z"
        (Base.Case
            { arg = Base.VarUse "z"
            , leftVar = "x"
            , leftBody = Base.Succ (Base.VarUse "x")
            , rightVar = "y"
            , rightBody = Base.VarUse "y"
            }
        )


case0Type =
    -- [Nat + Nat] -> Nat
    TypeInference.infer0 case0


add =
    -- Addition
    -- \x y -> x + y
    Base.Abstraction "x"
        (Base.Abstraction "y"
            (Base.NatLoop
                { base = Base.VarUse "x"
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = Base.Succ (Base.VarUse "s")
                    }
                , arg = Base.VarUse "y"
                }
            )
        )


addType =
    -- Nat -> Nat
    TypeInference.infer0 add


mul =
    -- Multiplication
    -- \x y -> x * y
    Base.Abstraction "x"
        (Base.Abstraction "y"
            (Base.NatLoop
                { base = n0
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = Base.Application (Base.Application add (Base.VarUse "x")) (Base.VarUse "s")
                    }
                , arg = Base.VarUse "y"
                }
            )
        )


mulType =
    -- Nat -> Nat
    TypeInference.infer0 mul


exp =
    -- Multiplication
    -- \x y -> x ^ y
    Base.Abstraction "x"
        (Base.Abstraction "y"
            (Base.NatLoop
                { base = n1
                , loop =
                    { indexVar = "i"
                    , stateVar = "s"
                    , body = Base.Application (Base.Application mul (Base.VarUse "x")) (Base.VarUse "s")
                    }
                , arg = Base.VarUse "y"
                }
            )
        )


expType =
    -- Nat -> Nat
    TypeInference.infer0 exp


sumTerm =
    -- Sum
    -- \N -> (N - 1) + ... + 2 + 3 + 1 + 0
    Base.Abstraction "N"
        (Base.NatLoop
            { base = n0
            , loop =
                { indexVar = "j"
                , stateVar = "sum"
                , body = Base.Application (Base.Application add (Base.VarUse "j")) (Base.VarUse "sum")
                }
            , arg = Base.VarUse "N"
            }
        )


sumTermType =
    -- Nat -> Nat
    TypeInference.infer0 sumTerm


selfApply =
    -- \f -> f f
    Base.Abstraction "f" (Base.Application (Base.VarUse "f") (Base.VarUse "f"))


selfApplyType =
    -- infinite type
    TypeInference.infer0 selfApply



-- LISTS


range4 =
    Base.Cons n0 (Base.Cons n1 (Base.Cons n2 (Base.Cons n3 Base.ConstEmpty)))


range4Type =
    TypeInference.infer0 range4


constList =
    Base.Abstraction "x"
        (Base.Abstraction "n"
            (Base.NatLoop
                { base = Base.ConstEmpty
                , loop =
                    { indexVar = "i"
                    , stateVar = "xs"
                    , body =
                        Base.Cons (Base.VarUse "x") (Base.VarUse "xs")
                    }
                , arg = Base.VarUse "n"
                }
            )
        )


constListType =
    TypeInference.infer0 constList


constTrue =
    Base.Application (Base.Application constList Base.ConstTrue) n4


sumList =
    Base.Abstraction "xs"
        (Base.ListLoop
            { initState = n0
            , loop =
                { listElementVar = "x"
                , stateVar = "s"
                , body =
                    Base.Application (Base.Application add (Base.VarUse "x")) (Base.VarUse "s")
                }
            , arg = Base.VarUse "xs"
            }
        )


sumListType =
    TypeInference.infer0 sumList


sumRange4 =
    Base.Application sumList range4


sumRange4Type =
    TypeInference.infer0 sumRange4


listMap =
    Base.Abstraction "f"
        (Base.Abstraction "xs"
            (Base.ListLoop
                { initState = Base.ConstEmpty
                , loop =
                    { listElementVar = "x"
                    , stateVar = "ys"
                    , body =
                        Base.Cons (Base.Application (Base.VarUse "f") (Base.VarUse "x")) (Base.VarUse "ys")
                    }
                , arg = Base.VarUse "xs"
                }
            )
        )


listMapType =
    TypeInference.infer0 listMap


listConcat =
    Base.Abstraction "xs"
        (Base.Abstraction "ys"
            (Base.ListLoop
                { initState = Base.VarUse "ys"
                , loop =
                    { listElementVar = "x"
                    , stateVar = "zs"
                    , body =
                        Base.Cons (Base.VarUse "x") (Base.VarUse "zs")
                    }
                , arg = Base.VarUse "xs"
                }
            )
        )


listConcatType =
    TypeInference.infer0 listConcat


listAndThen =
    Base.Abstraction "f"
        (Base.Abstraction "xs"
            (Base.ListLoop
                { initState = Base.ConstEmpty
                , loop =
                    { listElementVar = "x"
                    , stateVar = "ys"
                    , body =
                        Base.Application
                            (Base.Application listConcat
                                (Base.Application (Base.VarUse "f") (Base.VarUse "x"))
                            )
                            (Base.VarUse "ys")
                    }
                , arg = Base.VarUse "xs"
                }
            )
        )


listReturn =
    Base.Abstraction "x" (Base.Cons (Base.VarUse "x") Base.ConstEmpty)


listReturnType =
    TypeInference.infer0 listReturn



-- Modules/Interfaces


module0 : ModuleLiteral
module0 =
    { bindings =
        [ Base.LetTerm "n" Base.ConstZero
        , Base.LetTerm "f" (Base.Abstraction "x" (Base.Pair (Base.VarUse "x") (Base.VarUse "n")))
        ]
    }


interface0 : Interface
interface0 =
    { assumptions =
        [ Base.AssumeTerm "n" Base.ConstNat
        , Base.AssumeTerm "f" (Base.ForAll 0 (Base.Arrow (Base.VarType 0) (Base.Product (Base.VarType 0) Base.ConstNat)))
        ]
    }
