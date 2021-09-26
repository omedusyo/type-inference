module Example exposing (..)

import Inference exposing (..)
import LambdaBasics exposing (..)



-- examples


x =
    VarUse "X"


xType =
    -- '0
    infer0 x


y =
    VarUse "Y"


yType =
    -- '0
    infer0 y


n0 =
    NatZero


n0Type =
    -- Nat
    infer0 n0


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


pair0Type =
    -- ('0, '1)
    infer0 pair0


pair1 =
    -- (x, x)
    Pair x x


pair1Type =
    -- ('0, '0)
    infer0 pair1


matchProduct0 =
    MatchProduct { arg = Pair n0 n1, var0 = "x", var1 = "y", body = VarUse "x" }


matchProduct0Type =
    infer0 matchProduct0


id =
    -- \x -> x
    Abstraction "x" (VarUse "x")


idType =
    -- '0 -> '0
    infer0 id


dup =
    -- \x -> (x, x)
    Abstraction "x" (Pair (VarUse "x") (VarUse "x"))


dupType =
    -- '0 -> ('0, '0)
    infer0 dup


twist =
    -- \p -> (match-pair p { (pair x y) . (pair y x) })
    Abstraction
        "p"
        (MatchProduct
            { arg = VarUse "p"
            , var0 = "x"
            , var1 = "y"
            , body = Pair (VarUse "y") (VarUse "x")
            }
        )


twistType =
    -- ('0, '1) -> ('1, '0)
    infer0 twist


apply =
    --  \f x -> f x
    Abstraction "f"
        (Abstraction "x"
            (Application (VarUse "f") (VarUse "x"))
        )


applyType =
    -- ('0 -> '1) -> '0 -> '1
    infer0 apply


pairing =
    -- \x y -> (x, y)
    Abstraction "x"
        (Abstraction "y"
            (Pair (VarUse "x") (VarUse "y"))
        )


pairingType =
    -- '0 -> '1 -> ('0, '1)
    infer0 pairing


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


curryType =
    -- (('0, '1) -> '2) -> '0 -> '1 -> '2
    infer0 curry


uncurry =
    -- (fn { f p . (match-pair p { x y . (@ f x y) }) })
    Abstraction "f"
        (Abstraction "p"
            (MatchProduct
                { arg = VarUse "p"
                , var0 = "x"
                , var1 = "y"
                , body = Application (Application (VarUse "f") (VarUse "x")) (VarUse "y")
                }
            )
        )


uncurryType =
    -- ('0 -> '1 -> '2) -> ('0, '1) -> '2
    infer0 uncurry


apply0 =
    Application (Application apply dup) n0


apply0Type =
    -- (Nat, Nat)
    infer0 apply0


evalAt0 =
    -- \f -> f 0
    Abstraction "f" (Application (VarUse "f") n0)


evalAt0Type =
    -- (Nat -> '0) -> '0
    infer0 evalAt0


succ0 =
    -- \n -> S n
    Abstraction "n" (NatSucc (VarUse "n"))


succ0Type =
    -- Nat -> Nat
    infer0 succ0


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


case0Type =
    -- [Nat + Nat] -> Nat
    infer0 case0


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


addType =
    -- Nat -> Nat
    infer0 add


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


mulType =
    -- Nat -> Nat
    infer0 mul


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


expType =
    -- Nat -> Nat
    infer0 exp


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


sumTermType =
    -- Nat -> Nat
    infer0 sumTerm


selfApply =
    -- \f -> f f
    Abstraction "f" (Application (VarUse "f") (VarUse "f"))


selfApplyType =
    -- infinite type
    infer0 selfApply



-- LISTS


range4 =
    Cons n0 (Cons n1 (Cons n2 (Cons n3 EmptyList)))


range4Type =
    infer0 range4


constList =
    Abstraction "x"
        (Abstraction "n"
            (NatLoop
                { base = EmptyList
                , loop =
                    { indexVar = "i"
                    , stateVar = "xs"
                    , body =
                        Cons (VarUse "x") (VarUse "xs")
                    }
                , arg = VarUse "n"
                }
            )
        )


constListType =
    infer0 constList


constTrue =
    Application (Application constList BoolTrue) n4


sumList =
    Abstraction "xs"
        (ListLoop
            { initState = n0
            , loop =
                { listElementVar = "x"
                , stateVar = "s"
                , body =
                    Application (Application add (VarUse "x")) (VarUse "s")
                }
            , arg = VarUse "xs"
            }
        )


sumListType =
    infer0 sumList


sumRange4 =
    Application sumList range4


sumRange4Type =
    infer0 sumRange4


listMap =
    Abstraction "f"
        (Abstraction "xs"
            (ListLoop
                { initState = EmptyList
                , loop =
                    { listElementVar = "x"
                    , stateVar = "ys"
                    , body =
                        Cons (Application (VarUse "f") (VarUse "x")) (VarUse "ys")
                    }
                , arg = VarUse "xs"
                }
            )
        )


listMapType =
    infer0 listMap


listConcat =
    Abstraction "xs"
        (Abstraction "ys"
            (ListLoop
                { initState = VarUse "ys"
                , loop =
                    { listElementVar = "x"
                    , stateVar = "zs"
                    , body =
                        Cons (VarUse "x") (VarUse "zs")
                    }
                , arg = VarUse "xs"
                }
            )
        )


listConcatType =
    infer0 listConcat


listAndThen =
    Abstraction "f"
        (Abstraction "xs"
            (ListLoop
                { initState = EmptyList
                , loop =
                    { listElementVar = "x"
                    , stateVar = "ys"
                    , body =
                        Application
                            (Application listConcat
                                (Application (VarUse "f") (VarUse "x"))
                            )
                            (VarUse "ys")
                    }
                , arg = VarUse "xs"
                }
            )
        )


listReturn =
    Abstraction "x" (Cons (VarUse "x") EmptyList)


listReturnType =
    infer0 listReturn



-- Modules/Interfaces


monoidInterface : Category
monoidInterface =
    Interface 0 (Arrow (Product (VarType 0) (VarType 0)) (VarType 0))


monoidOnNat : CategoryTerm
monoidOnNat =
    Module LambdaNat
        (Abstraction "x"
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
        )



-- Modules/Interfaces


module0 : ModuleTerm
module0 =
    { bindings =
        [ LetTerm "foo" (Abstraction "x" (VarUse "x"))
        , LetTerm "bar" NatZero
        ]
    }


interface0 : Interface
interface0 =
    { assumptions =
        [ AssumeTerm "foo" (Arrow LambdaNat LambdaNat)
        , AssumeTerm "bar" LambdaNat
        ]
    }
