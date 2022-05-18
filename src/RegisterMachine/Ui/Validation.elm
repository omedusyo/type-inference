module RegisterMachine.Ui.Validation exposing (validatedInstruction)

import Lib.Break as Break
import Lib.ZipList as ZipList exposing (ZipList)
import List.Extra as List
import Maybe.Extra as Maybe
import RegisterMachine.Ui.Base as Base
    exposing
        ( EntityKind(..)
        , ExpectedArity(..)
        , Instruction(..)
        , InstructionKind(..)
        , InstructionValidity(..)
        , Node(..)
        , NodeExpectations
        , NodeValidity(..)
        )


setNodeValidity : NodeValidity -> Node -> Node
setNodeValidity nodeValidity (Node nodeKind _ nodeExpectation text) =
    Node nodeKind nodeValidity nodeExpectation text


wrongArity : ExpectedArity -> Int -> InstructionValidity
wrongArity expected received =
    WrongArity { expected = expected, received = received }


validatedNode : Node -> Node
validatedNode ((Node _ _ allExpectations text) as node) =
    let
        loop : EntityKind -> NodeExpectations -> Node
        loop entityKind0 expectations0 =
            if hasEntityKind text entityKind0 then
                node |> setNodeValidity (ValidNode entityKind0)

            else
                case expectations0 of
                    [] ->
                        node |> setNodeValidity ErrorNode

                    entityKind1 :: expectations1 ->
                        loop entityKind1 expectations1
    in
    if isEmpty text then
        node |> setNodeValidity UnfinishedNode

    else
        case allExpectations of
            [] ->
                Debug.todo "This should never get triggered assuming someone doesn't create empty node-expectation"

            entityKind :: expectations0 ->
                loop entityKind expectations0


isEmpty : String -> Bool
isEmpty =
    String.isEmpty



-- TODO: I need to validate that text doesn't contain $ or :
--       or stuff like it can't start with ' ' or '-' (since you want to include negative integers)
-- I will need to do some parsing, for now we'll just make it simple


hasEntityKind : String -> EntityKind -> Bool
hasEntityKind text entityKind =
    case entityKind of
        RegisterName ->
            -- TODO
            True

        RegisterUse ->
            case String.uncons text of
                Nothing ->
                    False

                Just ( c, text1 ) ->
                    -- TODO
                    c == '$'

        Label ->
            -- TODO
            True

        LabelUse ->
            case String.uncons text of
                Nothing ->
                    False

                Just ( c, text1 ) ->
                    -- TODO
                    c == ':'

        Integer ->
            -- TODO
            Maybe.isJust (String.toInt text)

        Nil ->
            text == "nil"

        OperationName ->
            -- TODO
            True


nodesToInstructionValidity : List Node -> InstructionValidity
nodesToInstructionValidity nodes =
    let
        nonValidNodes : List Node
        nonValidNodes =
            nodes
                |> List.filter
                    (\(Node _ nodeValidity _ _) ->
                        case nodeValidity of
                            ValidNode _ ->
                                False

                            _ ->
                                True
                    )
    in
    if List.isEmpty nonValidNodes then
        EveryNodeIsValid

    else
        let
            containsErrorNode : Bool
            containsErrorNode =
                nonValidNodes
                    |> List.any
                        (\(Node _ nodeValidity _ _) ->
                            case nodeValidity of
                                ErrorNode ->
                                    True

                                _ ->
                                    False
                        )
        in
        if containsErrorNode then
            ContainsErrorNodes

        else
            ContainsUnfinishedNodes



-- The input is basically an instruction without the validation information.


validatedInstruction : InstructionKind -> ZipList Node -> Instruction
validatedInstruction instructionKind nodes =
    let
        ( validatedNodes, instructionValidity ) =
            validateInstruction instructionKind nodes

        _ =
            -- TODO: remove
            Debug.log "" ( instructionValidity, validatedNodes |> ZipList.toList )
    in
    Instruction instructionKind validatedNodes instructionValidity


validateInstruction : InstructionKind -> ZipList Node -> ( ZipList Node, InstructionValidity )
validateInstruction instructionKind nodes =
    let
        numOfNodes : Int
        numOfNodes =
            ZipList.length nodes

        validatedNodes : ZipList Node
        validatedNodes =
            nodes |> ZipList.map validatedNode

        instructionValidity : InstructionValidity
        instructionValidity =
            nodesToInstructionValidity (ZipList.toList validatedNodes)

        checkArity : ExpectedArity -> InstructionValidity
        checkArity expectedArity =
            case expectedArity of
                Atleast expectedNumOfNodes ->
                    if numOfNodes >= expectedNumOfNodes then
                        instructionValidity

                    else
                        wrongArity (Atleast expectedNumOfNodes) numOfNodes

                Exactly expectedNumOfNodes ->
                    if numOfNodes == expectedNumOfNodes then
                        instructionValidity

                    else
                        wrongArity (Exactly expectedNumOfNodes) numOfNodes
    in
    ( validatedNodes
    , checkArity
        (case instructionKind of
            LabelKind ->
                Exactly 1

            OperationApplicationKind ->
                Atleast 3

            AssignmentKind ->
                Exactly 2

            JumpKind ->
                Exactly 1

            JumpIfKind ->
                Exactly 2

            PushKind ->
                Exactly 1

            HaltKind ->
                Exactly 0
        )
    )
