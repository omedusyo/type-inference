module Cycles exposing (..)

import Main exposing (..)
import Set exposing (Set)


expandTypeWithCycleDetection : Type -> Set TypeVarName -> Equations -> Result TypeVarName Type
expandTypeWithCycleDetection type0 seenVars eqs0 =
    case type0 of
        VarType n ->
            if Set.member n seenVars then
                Err n

            else
                case lookupEquations n eqs0 of
                    Just type1 ->
                        expandTypeWithCycleDetection type1 (Set.insert n seenVars) eqs0

                    Nothing ->
                        Ok (VarType n)

        Product type1 type2 ->
            Result.map2 Product
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        Sum type1 type2 ->
            Result.map2 Sum
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        Arrow type1 type2 ->
            Result.map2 Arrow
                (expandTypeWithCycleDetection type1 seenVars eqs0)
                (expandTypeWithCycleDetection type2 seenVars eqs0)

        LambdaBool ->
            Ok LambdaBool

        LambdaNat ->
            Ok LambdaNat
