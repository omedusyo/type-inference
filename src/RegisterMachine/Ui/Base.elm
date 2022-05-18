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
    = Node NodeKind NodeValidity NodeExpectations String



-- ===Instructions===


initialInstruction : InstructionKind -> Instruction
initialInstruction instructionKind =
    case instructionKind of
        LabelKind ->
            -- label _
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static labelExpectation) [])
                initialInstructionValidity

        OperationApplicationKind ->
            -- _ <- _(_)
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static registerExpectation) [ emptyNode Static argExpectation, emptyNode Dynamic argExpectation ])
                initialInstructionValidity

        AssignmentKind ->
            -- _ <- _
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static registerExpectation) [ emptyNode Static argExpectation ])
                initialInstructionValidity

        JumpKind ->
            -- jump _
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static jumpArgExpectation) [])
                initialInstructionValidity

        JumpIfKind ->
            -- if _ jump _
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static registerUseExpectation) [ emptyNode Static jumpArgExpectation ])
                initialInstructionValidity

        HaltKind ->
            Halt

        PushKind ->
            Instruction instructionKind
                (ZipList.fromList (emptyNode Static argExpectation) [])
                initialInstructionValidity



-- ===Nodes===


emptyNode : NodeKind -> NodeExpectations -> Node
emptyNode nodeKind nodeExpectation =
    Node nodeKind UnfinishedNode nodeExpectation ""



-- ===Instruction Validation===
-- The following type definitions are here only because Elm doesn't allow cyclic module dependencies.
-- The problem is that I want to include InstructoinValidity as a component of Instruction


type NodeValidity
    = UnfinishedNode
    | ErrorNode
    | ValidNode EntityKind


type alias NodeExpectations =
    List EntityKind



-- TODO: Maybe I should put these into a separate definition?


labelExpectation : NodeExpectations
labelExpectation =
    [ Label ]


argExpectation : NodeExpectations
argExpectation =
    [ RegisterUse, LabelUse, Integer, Nil ]


registerExpectation : NodeExpectations
registerExpectation =
    [ RegisterName ]


registerUseExpectation : NodeExpectations
registerUseExpectation =
    [ RegisterUse ]


operationNameExpectation : NodeExpectations
operationNameExpectation =
    [ OperationName ]


jumpArgExpectation : NodeExpectations
jumpArgExpectation =
    [ RegisterUse, LabelUse ]


type InstructionValidity
    = EveryNodeIsValid
    | ContainsErrorNodes
    | ContainsUnfinishedNodes
    | WrongArity { expected : ExpectedArity, received : Int }


initialInstructionValidity : InstructionValidity
initialInstructionValidity =
    ContainsUnfinishedNodes


type ExpectedArity
    = Atleast Int
    | Exactly Int


type EntityKind
    = RegisterName
    | RegisterUse
    | Label
    | LabelUse
    | Integer
    | Nil
    | OperationName
