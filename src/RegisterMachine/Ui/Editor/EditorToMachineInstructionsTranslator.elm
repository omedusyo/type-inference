module RegisterMachine.Ui.Editor.EditorToMachineInstructionsTranslator exposing (translateEditorInstruction, translateEditorInstructions)

import Lib.ZipList as ZipList exposing (ZipList)
import Maybe.Extra as Maybe
import RegisterMachine.Base as RegisterMachine
import RegisterMachine.Machine as RegisterMachine
import RegisterMachine.Ui.Editor.Base as Editor



-- ===helpers===


unfinished : RegisterMachine.LabelOrInstruction
unfinished =
    RegisterMachine.Perform (RegisterMachine.Unfinished {})


halt : RegisterMachine.LabelOrInstruction
halt =
    RegisterMachine.Perform (RegisterMachine.Halt {})



-- ===Translators===


translateEditorInstructions : ZipList Editor.Instruction -> RegisterMachine.InstructionBlock
translateEditorInstructions instructions =
    instructions
        |> ZipList.toList
        |> List.map translateEditorInstruction


translateEditorInstruction : Editor.Instruction -> RegisterMachine.LabelOrInstruction
translateEditorInstruction instruction =
    case instruction of
        Editor.Instruction instructionKind nodes instructionValidity ->
            case instructionValidity of
                Editor.EveryNodeIsValid ->
                    case instructionKind of
                        Editor.LabelKind ->
                            case ZipList.toList nodes of
                                [ labelNode ] ->
                                    RegisterMachine.Label (Editor.nodeInput labelNode)

                                _ ->
                                    unfinished

                        Editor.OperationApplicationKind ->
                            case ZipList.toList nodes of
                                targetRegisterNode :: opNameNode :: argNodes ->
                                    argNodes
                                        |> Maybe.traverse (\argNode -> parseOperationArgument (Editor.nodeInput argNode))
                                        |> Maybe.map
                                            (\args ->
                                                let
                                                    targetRegister : RegisterMachine.Register
                                                    targetRegister =
                                                        Editor.nodeInput targetRegisterNode

                                                    opName : RegisterMachine.OperationName
                                                    opName =
                                                        Editor.nodeInput opNameNode

                                                    arguments : List RegisterMachine.OperationArgument
                                                    arguments =
                                                        args |> List.map convertOperationArgument
                                                in
                                                RegisterMachine.Perform
                                                    (RegisterMachine.AssignOperation
                                                        { targetRegister = targetRegister, operationApplication = { name = opName, arguments = arguments } }
                                                    )
                                            )
                                        |> Maybe.withDefault unfinished

                                _ ->
                                    unfinished

                        Editor.AssignmentKind ->
                            case ZipList.toList nodes of
                                [ targetRegisterNode, argNode ] ->
                                    let
                                        targetRegister : RegisterMachine.Register
                                        targetRegister =
                                            Editor.nodeInput targetRegisterNode
                                    in
                                    case parseArgument (Editor.nodeInput argNode) of
                                        Just arg ->
                                            case arg of
                                                ANumber n ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.AssignConstant
                                                            { targetRegister = targetRegister, constant = RegisterMachine.Num n }
                                                        )

                                                ANil ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.AssignConstant
                                                            { targetRegister = targetRegister, constant = RegisterMachine.Nil }
                                                        )

                                                ALabel label ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.AssignLabel
                                                            { targetRegister = targetRegister, label = label }
                                                        )

                                                ARegisterUse sourceRegister ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.AssignRegister
                                                            { targetRegister = targetRegister, sourceRegister = sourceRegister }
                                                        )

                                        Nothing ->
                                            unfinished

                                _ ->
                                    unfinished

                        Editor.JumpKind ->
                            case ZipList.toList nodes of
                                [ argNode ] ->
                                    case parseJumpArgument (Editor.nodeInput argNode) of
                                        Just arg ->
                                            case arg of
                                                JLabel label ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.JumpToLabel
                                                            { label = label }
                                                        )

                                                JRegisterUse register ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.JumpToInstructionPointerAtRegister
                                                            { instructionPointerRegister = register }
                                                        )

                                        Nothing ->
                                            unfinished

                                _ ->
                                    unfinished

                        Editor.JumpIfKind ->
                            case ZipList.toList nodes of
                                [ testRegisterNode, argNode ] ->
                                    Maybe.map2
                                        (\(TRegisterUse testRegister) arg ->
                                            case arg of
                                                JLabel label ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.JumpToLabelIf
                                                            { testRegister = testRegister, label = label }
                                                        )

                                                JRegisterUse register ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.JumpToInstructionPointerAtRegisterIf
                                                            { testRegister = testRegister, instructionPointerRegister = register }
                                                        )
                                        )
                                        (parseTestArgument (Editor.nodeInput testRegisterNode))
                                        (parseJumpArgument (Editor.nodeInput argNode))
                                        |> Maybe.withDefault unfinished

                                _ ->
                                    unfinished

                        Editor.PushKind ->
                            case ZipList.toList nodes of
                                [ argNode ] ->
                                    case parseArgument (Editor.nodeInput argNode) of
                                        Just arg ->
                                            case arg of
                                                ANumber n ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.PushConstant
                                                            { constant = RegisterMachine.Num n }
                                                        )

                                                ANil ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.PushConstant
                                                            { constant = RegisterMachine.Nil }
                                                        )

                                                ALabel label ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.PushLabel
                                                            { label = label }
                                                        )

                                                ARegisterUse sourceRegister ->
                                                    RegisterMachine.Perform
                                                        (RegisterMachine.PushRegister
                                                            { sourceRegister = sourceRegister }
                                                        )

                                        Nothing ->
                                            unfinished

                                _ ->
                                    unfinished

                        Editor.HaltKind ->
                            halt

                _ ->
                    unfinished

        Editor.Halt ->
            halt

        Editor.FutureInstruction _ ->
            -- TODO: Refine the Editor.Instruction type so that I don't have to deal with it here
            Debug.todo "This should never happen"



-- ===String Parsing===


parseArgument : String -> Maybe Argument
parseArgument input0 =
    case input0 of
        "nil" ->
            Just ANil

        _ ->
            String.uncons input0
                |> Maybe.andThen
                    (\( c, input1 ) ->
                        case c of
                            ':' ->
                                Just (ALabel input1)

                            '$' ->
                                Just (ARegisterUse input1)

                            _ ->
                                String.toInt input0
                                    |> Maybe.map ANumber
                    )


parseJumpArgument : String -> Maybe JumpArgument
parseJumpArgument input0 =
    String.uncons input0
        |> Maybe.andThen
            (\( c, input1 ) ->
                case c of
                    ':' ->
                        Just (JLabel input1)

                    '$' ->
                        Just (JRegisterUse input1)

                    _ ->
                        Nothing
            )


parseTestArgument : String -> Maybe TestArgument
parseTestArgument input0 =
    String.uncons input0
        |> Maybe.andThen
            (\( c, input1 ) ->
                case c of
                    '$' ->
                        Just (TRegisterUse input1)

                    _ ->
                        Nothing
            )


parseOperationArgument : String -> Maybe OperationArgument
parseOperationArgument input0 =
    case input0 of
        "nil" ->
            Just ONil

        _ ->
            String.uncons input0
                |> Maybe.andThen
                    (\( c, input1 ) ->
                        case c of
                            '$' ->
                                Just (ORegisterUse input1)

                            _ ->
                                String.toInt input0
                                    |> Maybe.map ONumber
                    )


type Argument
    = ANumber Int
    | ANil
    | ALabel RegisterMachine.Label
    | ARegisterUse RegisterMachine.Register


type JumpArgument
    = JLabel RegisterMachine.Label
    | JRegisterUse RegisterMachine.Register


type OperationArgument
    = ONumber Int
    | ONil
    | ORegisterUse RegisterMachine.Register


type TestArgument
    = TRegisterUse RegisterMachine.Register


convertOperationArgument : OperationArgument -> RegisterMachine.OperationArgument
convertOperationArgument arg =
    case arg of
        ONumber n ->
            RegisterMachine.Constant (RegisterMachine.Num n)

        ONil ->
            RegisterMachine.Constant RegisterMachine.Nil

        ORegisterUse register ->
            RegisterMachine.Register register
