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


env0 =
    emptyEnv
        |> extend "X" (NatVal n0val)
        |> extend "X" (NatVal n1val)
        |> extend "Y" (NatVal n2val)


n0val =
    NatZeroVal


n1val =
    NatSuccVal n0val


n2val =
    NatSuccVal n1val


n3val =
    NatSuccVal n2val


n4val =
    NatSuccVal n3val


pair0 =
    -- (x, y)
    Tuple { fst = x, snd = y }


pair1 =
    -- (x, x)
    Tuple { fst = x, snd = x }


id =
    -- \x -> x
    Abstraction { var = "x", body = VarUse "x" }


dup =
    -- \x -> (x, x)
    Abstraction { var = "x", body = Tuple { fst = VarUse "x", snd = VarUse "x" } }


twist =
    --  \tuple -> (tuple.snd, tuple.fst)
    Abstraction
        { var = "tuple"
        , body =
            Tuple
                { fst = Snd (VarUse "tuple")
                , snd = Fst (VarUse "tuple")
                }
        }


apply =
    --  \f x -> f x
    Abstraction
        { var = "f"
        , body =
            Abstraction
                { var = "x"
                , body =
                    Application { fn = VarUse "f", arg = VarUse "x" }
                }
        }


pairing =
    -- \x y -> (x, y)
    Abstraction
        { var = "x"
        , body =
            Abstraction
                { var = "y"
                , body =
                    Tuple { fst = VarUse "x", snd = VarUse "y" }
                }
        }


curry =
    -- \f x y -> f (x, y)
    Abstraction
        { var = "f"
        , body =
            Abstraction
                { var = "x"
                , body =
                    Abstraction
                        { var = "y"
                        , body =
                            Application
                                { fn = VarUse "f"
                                , arg =
                                    Tuple { fst = VarUse "x", snd = VarUse "y" }
                                }
                        }
                }
        }


uncurry =
    -- \f p -> (f (fst p)) (snd p)
    Abstraction
        { var = "f"
        , body =
            Abstraction
                { var = "p"
                , body =
                    Application
                        { fn =
                            Application
                                { fn = VarUse "f"
                                , arg = Fst (VarUse "p")
                                }
                        , arg = Snd (VarUse "p")
                        }
                }
        }


apply0 =
    Application { fn = apply, arg = dup }


apply1 =
    Application { fn = apply0, arg = n0 }


evalAt0 =
    Abstraction { var = "f", body = Application { fn = VarUse "f", arg = n0 } }


succ0 =
    Abstraction { var = "n", body = NatSucc (VarUse "n") }


case0 =
    -- \z -> case z of Left x -> x + 1, Right y -> y
    Abstraction
        { var = "z"
        , body =
            Case
                { arg = VarUse "z"
                , leftVar = "x"
                , leftBody = NatSucc (VarUse "x")
                , rightVar = "y"
                , rightBody = VarUse "y"
                }
        }


add =
    -- Addition
    -- \x y -> x + y
    Abstraction
        { var = "x"
        , body =
            Abstraction
                { var = "y"
                , body =
                    NatLoop
                        { base = VarUse "x"
                        , loop =
                            { indexVar = "i"
                            , stateVar = "s"
                            , body = NatSucc (VarUse "s")
                            }
                        , arg = VarUse "y"
                        }
                }
        }


mul =
    -- Multiplication
    -- \x y -> x * y
    Abstraction
        { var = "x"
        , body =
            Abstraction
                { var = "y"
                , body =
                    NatLoop
                        { base = n0
                        , loop =
                            { indexVar = "i"
                            , stateVar = "s"
                            , body = Application { fn = Application { fn = add, arg = VarUse "x" }, arg = VarUse "s" }
                            }
                        , arg = VarUse "y"
                        }
                }
        }


exp =
    -- Multiplication
    -- \x y -> x ^ y
    Abstraction
        { var = "x"
        , body =
            Abstraction
                { var = "y"
                , body =
                    NatLoop
                        { base = n1
                        , loop =
                            { indexVar = "i"
                            , stateVar = "s"
                            , body = Application { fn = Application { fn = mul, arg = VarUse "x" }, arg = VarUse "s" }
                            }
                        , arg = VarUse "y"
                        }
                }
        }


sumTerm =
    -- Sum
    -- \N -> (N - 1) + ... + 2 + 3 + 1 + 0
    Abstraction
        { var = "N"
        , body =
            NatLoop
                { base = n0
                , loop =
                    { indexVar = "j"
                    , stateVar = "sum"
                    , body = Application { fn = Application { fn = add, arg = VarUse "j" }, arg = VarUse "sum" }
                    }
                , arg = VarUse "N"
                }
        }



-- inference
-- \p -> p.0
-- p : '0
-- p.0 : '1 where p : ('1, '2)
-- \p -> p.0 :
