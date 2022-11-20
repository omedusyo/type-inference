module RegisterMachine.Ui.Editor.MachineInstructionsToEditorTranslator exposing (..)

import Lib.ZipList as ZipList
import RegisterMachine.Base as RegisterMachine
import RegisterMachine.Machine as RegisterMachine
import RegisterMachine.Ui.Editor.Base as Editor


translateInstructionBlock : RegisterMachine.InstructionBlock -> List Editor.Instruction
translateInstructionBlock instructions =
    instructions
        |> List.map
            (\labelOrInstruction ->
                case labelOrInstruction of
                    RegisterMachine.Perform instruction ->
                        translateInstruction instruction

                    RegisterMachine.Label label ->
                        translateLabelIntroduction label
            )


translateInstruction : RegisterMachine.Instruction -> Editor.Instruction
translateInstruction instruction =
    let
        -- ===Instructions===
        assign : Editor.Node -> Editor.Node -> Editor.Instruction
        assign sourceNode targetNode =
            Editor.Instruction
                Editor.AssignmentKind
                (ZipList.fromList sourceNode [ targetNode ])
                Editor.EveryNodeIsValid

        jump : Editor.Node -> Editor.Instruction
        jump node =
            Editor.Instruction
                Editor.JumpKind
                (ZipList.singleton node)
                Editor.EveryNodeIsValid

        jumpIf : Editor.Node -> Editor.Node -> Editor.Instruction
        jumpIf testNode node =
            Editor.Instruction
                Editor.JumpIfKind
                (ZipList.fromList testNode [ node ])
                Editor.EveryNodeIsValid

        push : Editor.Node -> Editor.Instruction
        push node =
            Editor.Instruction
                Editor.PushKind
                (ZipList.singleton node)
                Editor.EveryNodeIsValid

        -- ===Nodes===
        -- assignment
        registerNameNode : Editor.NodeKind -> RegisterMachine.Register -> Editor.Node
        registerNameNode nodeKind register =
            Editor.Node nodeKind (Editor.ValidNode Editor.RegisterName) Editor.registerExpectation register

        argRegisterUseNode : Editor.NodeKind -> RegisterMachine.Register -> Editor.Node
        argRegisterUseNode nodeKind register =
            Editor.Node nodeKind (Editor.ValidNode Editor.RegisterUse) Editor.argExpectation register

        argConstantNode : Editor.NodeKind -> RegisterMachine.Constant -> Editor.Node
        argConstantNode nodeKind constant =
            case constant of
                RegisterMachine.Nil ->
                    Editor.Node nodeKind (Editor.ValidNode Editor.Nil) Editor.argExpectation "nil"

                RegisterMachine.Num n ->
                    Editor.Node nodeKind (Editor.ValidNode Editor.Integer) Editor.argExpectation (String.fromInt n)

        argLabelNode : Editor.NodeKind -> RegisterMachine.Label -> Editor.Node
        argLabelNode nodeKind label =
            Editor.Node nodeKind (Editor.ValidNode Editor.Label) Editor.argExpectation label

        -- jumping
        testRegisterUseNode : RegisterMachine.Register -> Editor.Node
        testRegisterUseNode register =
            -- This is used only for if $reg jump ...
            Editor.Node Editor.Static (Editor.ValidNode Editor.RegisterUse) Editor.registerUseExpectation register

        jumpArgLabelNode : RegisterMachine.Label -> Editor.Node
        jumpArgLabelNode label =
            Editor.Node Editor.Static (Editor.ValidNode Editor.Label) Editor.jumpArgExpectation label

        jumpArgRegisterNode : RegisterMachine.Register -> Editor.Node
        jumpArgRegisterNode register =
            Editor.Node Editor.Static (Editor.ValidNode Editor.RegisterUse) Editor.jumpArgExpectation register
    in
    case instruction of
        RegisterMachine.AssignRegister input ->
            assign (registerNameNode Editor.Static input.targetRegister) (argRegisterUseNode Editor.Static input.sourceRegister)

        RegisterMachine.AssignLabel input ->
            assign (registerNameNode Editor.Static input.targetRegister) (argLabelNode Editor.Static input.label)

        RegisterMachine.AssignOperation input ->
            Debug.todo ""

        RegisterMachine.AssignConstant input ->
            assign (registerNameNode Editor.Static input.targetRegister) (argConstantNode Editor.Static input.constant)

        -- jumping
        RegisterMachine.JumpToLabel input ->
            jump (jumpArgLabelNode input.label)

        RegisterMachine.JumpToInstructionPointerAtRegister input ->
            jump (jumpArgRegisterNode input.instructionPointerRegister)

        RegisterMachine.JumpToLabelIf input ->
            jumpIf (testRegisterUseNode input.testRegister) (jumpArgLabelNode input.label)

        RegisterMachine.JumpToInstructionPointerAtRegisterIf input ->
            jumpIf (testRegisterUseNode input.testRegister) (jumpArgRegisterNode input.instructionPointerRegister)

        -- haltings
        RegisterMachine.Halt _ ->
            Editor.Halt

        -- stack
        RegisterMachine.PushRegister input ->
            push (argRegisterUseNode Editor.Static input.sourceRegister)

        RegisterMachine.PushConstant input ->
            push (argConstantNode Editor.Static input.constant)

        RegisterMachine.PushLabel input ->
            push (argLabelNode Editor.Static input.label)

        RegisterMachine.Pop input ->
            Debug.todo ""

        -- memory
        RegisterMachine.ConstructPair input ->
            Debug.todo ""

        RegisterMachine.First input ->
            Debug.todo ""

        RegisterMachine.Second input ->
            Debug.todo ""

        RegisterMachine.SetFirst input ->
            Debug.todo ""

        RegisterMachine.SetSecond input ->
            Debug.todo ""

        -- dual memory
        RegisterMachine.DualFirst input ->
            Debug.todo ""

        RegisterMachine.DualSecond input ->
            Debug.todo ""

        RegisterMachine.DualSetFirst input ->
            Debug.todo ""

        RegisterMachine.DualSetSecond input ->
            Debug.todo ""

        -- garbage collection
        RegisterMachine.MoveToDual input ->
            Debug.todo ""

        RegisterMachine.MarkAsMoved input ->
            Debug.todo ""

        RegisterMachine.SwapMemory input ->
            Debug.todo ""

        -- dev-mode instructions
        RegisterMachine.Unfinished input ->
            Debug.todo ""


translateLabelIntroduction : RegisterMachine.Label -> Editor.Instruction
translateLabelIntroduction label =
    Editor.Instruction
        Editor.LabelKind
        (ZipList.singleton (Editor.Node Editor.Static (Editor.ValidNode Editor.Label) Editor.labelExpectation label))
        Editor.EveryNodeIsValid
