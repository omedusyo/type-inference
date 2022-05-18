module RegisterMachine.Ui.Base exposing (..)

import Lib.ZipList as ZipList exposing (ZipList)



-- ===Directions===


type HorizontalDirection
    = Left
    | Right


type VerticalDirection
    = Up
    | Down



-- ===Instruction Types===


type
    InstructionKind
    -- contains only instruction kinds for instructions that have atleast one node
    = LabelKind
    | OperationApplicationKind
    | AssignmentKind
    | JumpKind
    | JumpIfKind
    | PushKind
    | HaltKind


type Instruction
    = Instruction InstructionKind (ZipList Node) InstructionValidity
    | Halt
      -- The only way to have a future instruction is when the user wishes to insert a completely new instruction.
      -- Then we temporarily create the FutureInstruction until the user decides with which concerete instructio to replace it with.
      -- But if the user presses Esc during his decision, the current FutureInstruction should be deleted.
      -- After the deletion in which direction should we move to? Up or Down? That's why we have the VerticalDirection argument.
    | FutureInstruction VerticalDirection



-- ===Node Types===


type NodeKind
    = Static
    | Dynamic


type Node
    = Node NodeKind String



-- ===Instructions===


initialInstruction : InstructionKind -> Instruction
initialInstruction instructionKind =
    case instructionKind of
        LabelKind ->
            -- label _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])
                initialInstructionValidity

        OperationApplicationKind ->
            -- _ <- _(_)
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode, emptyDynamicNode ])
                initialInstructionValidity

        AssignmentKind ->
            -- _ <- _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode ])
                initialInstructionValidity

        JumpKind ->
            -- jump _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])
                initialInstructionValidity

        JumpIfKind ->
            -- if _ jump _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode ])
                initialInstructionValidity

        HaltKind ->
            Halt

        PushKind ->
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])
                initialInstructionValidity



-- ===Nodes===


emptyStaticNode : Node
emptyStaticNode =
    Node Static ""


emptyDynamicNode : Node
emptyDynamicNode =
    Node Dynamic ""



-- ===Instruction Validation===
-- The following type definitions are here only because Elm doesn't allow cyclic module dependencies.
-- The problem is that I want to include InstructoinValidity as a component of Instruction


type InstructionValidity
    = Finished
    | InvalidOrUnfinishedNodes (List NodeValidation)
    | WrongArity { expected : ExpectedArity, received : Int }


initialInstructionValidity : InstructionValidity
initialInstructionValidity =
    InvalidOrUnfinishedNodes []


type ExpectedArity
    = Atleast Int
    | Exactly Int


type NodeValidation
    = NodeIsInvalid NodePosition Expectations
    | NodeIsUnfinished NodePosition
    | NodeIsValid


type alias NodePosition =
    Int


type alias Expectations =
    -- This is just a non-empty list of entity kinds
    ( EntityKind, List EntityKind )


expectationsToList : Expectations -> List EntityKind
expectationsToList ( entityKind, entityKinds ) =
    entityKind :: entityKinds


type EntityKind
    = RegisterName
    | RegisterUse
    | Label
    | LabelUse
    | Integer
    | Nil
    | OperationName
