module RegisterMachine.Ui.Validation exposing (validatedInstruction)

import Lib.ZipList as ZipList exposing (ZipList)
import Maybe.Extra as Maybe
import RegisterMachine.Ui.Base as Base
    exposing
        ( EntityKind(..)
        , Expectations
        , ExpectedArity(..)
        , Instruction(..)
        , InstructionKind(..)
        , InstructionValidity(..)
        , Node(..)
        , NodePosition
        , NodeValidation(..)
        )


wrongArity : ExpectedArity -> Int -> InstructionValidity
wrongArity expected received =
    WrongArity { expected = expected, received = received }


validateNode : NodePosition -> String -> Expectations -> NodeValidation
validateNode position text expectations0 =
    if isEmpty text then
        NodeIsUnfinished position

    else
        case expectations0 of
            ( entityKind, entities0 ) ->
                if hasEntityKind text entityKind then
                    NodeIsValid

                else
                    case entities0 of
                        [] ->
                            NodeIsInvalid position expectations0

                        entityKind1 :: entities1 ->
                            validateNode position text ( entityKind1, entities1 )


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


instructionValidity : List NodeValidation -> InstructionValidity
instructionValidity nodesValidation =
    if List.isEmpty nodesValidation then
        Finished

    else
        InvalidOrUnfinishedNodes nodesValidation



-- The input is basically an instruction without the validation information.


validatedInstruction : InstructionKind -> ZipList Node -> Instruction
validatedInstruction instructionKind nodes =
    Instruction instructionKind nodes (validateInstruction instructionKind nodes)


validateInstruction : InstructionKind -> ZipList Node -> InstructionValidity
validateInstruction instructionKind nodes =
    let
        wat =
            Debug.log "performing validation" ""
    in
    case instructionKind of
        LabelKind ->
            case ZipList.current nodes of
                Node _ label ->
                    instructionValidity
                        [ validateNode 0 label ( Label, [] ) ]

        OperationApplicationKind ->
            let
                argExpectation : Expectations
                argExpectation =
                    ( RegisterUse, [ LabelUse, Integer, Nil ] )
            in
            case ZipList.toList nodes of
                (Node _ source) :: (Node _ opName) :: argNodes ->
                    if List.length argNodes > 0 then
                        instructionValidity
                            ([ validateNode 0 source ( RegisterName, [] )
                             , validateNode 1 opName ( OperationName, [] )
                             ]
                                ++ (argNodes |> List.indexedMap (\i (Node _ arg) -> validateNode (i + 2) arg argExpectation))
                            )

                    else
                        wrongArity (Atleast 3) (ZipList.length nodes)

                _ ->
                    wrongArity (Atleast 3) (ZipList.length nodes)

        AssignmentKind ->
            case ZipList.toList nodes of
                (Node _ source) :: (Node _ arg) :: [] ->
                    instructionValidity
                        [ validateNode 0 source ( RegisterName, [] )
                        , validateNode 1 arg ( RegisterUse, [ LabelUse, Integer, Nil ] )
                        ]

                _ ->
                    wrongArity (Exactly 2) (ZipList.length nodes)

        JumpKind ->
            case ZipList.current nodes of
                Node _ arg ->
                    instructionValidity
                        [ validateNode 0 arg ( RegisterUse, [ LabelUse ] ) ]

        JumpIfKind ->
            case ZipList.toList nodes of
                (Node _ test) :: (Node _ arg) :: [] ->
                    instructionValidity
                        [ validateNode 0 arg ( RegisterUse, [] )
                        , validateNode 1 arg ( RegisterUse, [ LabelUse ] )
                        ]

                _ ->
                    wrongArity (Exactly 2) (ZipList.length nodes)

        PushKind ->
            case ZipList.current nodes of
                Node _ arg ->
                    instructionValidity
                        [ validateNode 0 arg ( RegisterUse, [ LabelUse, Integer, Nil ] ) ]

        HaltKind ->
            -- TODO: You should remove HaltKind from the InstructionKind
            Debug.todo ""
