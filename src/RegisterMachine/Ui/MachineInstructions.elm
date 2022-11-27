module RegisterMachine.Ui.MachineInstructions exposing (..)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import RegisterMachine.Base as RegisterMachine exposing (Constant(..), InstructionPointer, Value(..))
import RegisterMachine.Machine as RegisterMachine exposing (CompilationError(..), ComputationStep(..), RuntimeError(..))


type LabelOrInstruction
    = Label RegisterMachine.Label
    | Perform InstructionPointer RegisterMachine.Instruction


viewInstructions : InstructionPointer -> RegisterMachine.InstructionBlock -> Element msg
viewInstructions instructionPointer instructionBlock =
    let
        convertInstructionBlock : RegisterMachine.InstructionBlock -> List LabelOrInstruction
        convertInstructionBlock instructions =
            let
                update0 : RegisterMachine.LabelOrInstruction -> ( InstructionPointer, List LabelOrInstruction ) -> ( InstructionPointer, List LabelOrInstruction )
                update0 labelOrInstruction ( instructionPointer0, newInstructions ) =
                    case labelOrInstruction of
                        RegisterMachine.Label label ->
                            ( instructionPointer0, Label label :: newInstructions )

                        RegisterMachine.Perform instruction ->
                            ( instructionPointer0 + 1, Perform instructionPointer0 instruction :: newInstructions )
            in
            List.foldl update0 ( 0, [] ) instructions
                |> Tuple.second
                |> List.reverse

        viewInstructionName name =
            E.el [ Font.heavy ] (E.text name)

        viewRegisterName name =
            E.el [ Font.color (E.rgb255 0 56 186) ] (E.text name)

        viewRegisterUse name =
            viewRegisterName ("$" ++ name)

        viewLabel label =
            E.el [ Font.color (E.rgb255 239 151 0) ] (E.text label)

        viewLabelUse label =
            viewLabel (":" ++ label)

        viewOperationUse name args =
            E.row [] [ E.el [] (E.text name), E.text "(", E.row [] (List.intersperse (E.text ", ") args), E.text ")" ]

        viewLabelIntroduction label =
            E.row [ E.spacing 8 ] [ E.el [ Font.heavy ] (E.text "label "), E.row [] [ viewLabel label ] ]

        paddingLeft px =
            E.paddingEach { left = px, top = 0, right = 0, bottom = 0 }

        viewOperationArgument : RegisterMachine.OperationArgument -> Element msg
        viewOperationArgument argument =
            case argument of
                RegisterMachine.Register register ->
                    viewRegisterUse register

                RegisterMachine.Constant val ->
                    viewConstant val

        viewOperationApplication : RegisterMachine.OperationName -> List RegisterMachine.OperationArgument -> Element msg
        viewOperationApplication opName arguments =
            viewOperationUse
                opName
                (arguments |> List.map viewOperationArgument)

        viewInstruction : { isCurrentInstruction : Bool } -> RegisterMachine.Instruction -> Element msg
        viewInstruction { isCurrentInstruction } instruction =
            let
                spacingPx =
                    8
            in
            E.row
                [ E.spacing spacingPx ]
                [ E.el [ E.width (E.px 15) ]
                    (if isCurrentInstruction then
                        -- Right Arrow
                        -- https://unicode-table.com/en/sets/arrow-symbols/#right-arrows
                        E.text "➢"

                     else
                        E.text ""
                    )
                , E.row [ E.spacing spacingPx ]
                    (case instruction of
                        RegisterMachine.AssignRegister { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewRegisterUse sourceRegister ]

                        RegisterMachine.AssignLabel { targetRegister, label } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewLabelUse label ]

                        RegisterMachine.AssignOperation { targetRegister, operationApplication } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication operationApplication.name operationApplication.arguments ]

                        RegisterMachine.AssignConstant { targetRegister, constant } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewConstant constant ]

                        RegisterMachine.JumpToLabel { label } ->
                            [ viewInstructionName "jump", viewLabelUse label ]

                        RegisterMachine.JumpToInstructionPointerAtRegister { instructionPointerRegister } ->
                            [ viewInstructionName "jump", viewRegisterUse instructionPointerRegister ]

                        RegisterMachine.JumpToLabelIf { testRegister, label } ->
                            [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewLabelUse label ]

                        RegisterMachine.JumpToInstructionPointerAtRegisterIf { testRegister, instructionPointerRegister } ->
                            [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewRegisterUse instructionPointerRegister ]

                        RegisterMachine.Halt _ ->
                            [ viewInstructionName "halt" ]

                        RegisterMachine.PushRegister { sourceRegister } ->
                            [ viewInstructionName "push", viewRegisterUse sourceRegister ]

                        RegisterMachine.PushConstant { constant } ->
                            [ viewInstructionName "push", viewConstant constant ]

                        RegisterMachine.PushLabel { label } ->
                            [ viewInstructionName "push", viewLabelUse label ]

                        RegisterMachine.Pop { targetRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewInstructionName "pop-stack" ]

                        RegisterMachine.ConstructPair { targetRegister, operationArgument0, operationArgument1 } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "pair" [ operationArgument0, operationArgument1 ] ]

                        RegisterMachine.First { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "first" [ RegisterMachine.Register sourceRegister ] ]

                        RegisterMachine.Second { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "second" [ RegisterMachine.Register sourceRegister ] ]

                        RegisterMachine.SetFirst { targetRegister, operationArgument } ->
                            [ viewInstructionName "set-first", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                        RegisterMachine.SetSecond { targetRegister, operationArgument } ->
                            [ viewInstructionName "set-second", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                        RegisterMachine.DualFirst { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "dual-first" [ RegisterMachine.Register sourceRegister ] ]

                        RegisterMachine.DualSecond { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "dual-second" [ RegisterMachine.Register sourceRegister ] ]

                        RegisterMachine.DualSetFirst { targetRegister, operationArgument } ->
                            [ viewInstructionName "dual-set-first", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                        RegisterMachine.DualSetSecond { targetRegister, operationArgument } ->
                            [ viewInstructionName "dual-set-second", viewRegisterName targetRegister, viewOperationArgument operationArgument ]

                        RegisterMachine.MoveToDual { targetRegister, sourceRegister } ->
                            [ viewRegisterName targetRegister, viewInstructionName "<-", viewOperationApplication "move-to-dual" [ RegisterMachine.Register sourceRegister ] ]

                        RegisterMachine.MarkAsMoved { toBeCollectedFromRegister, referenceToDualMemoryRegister } ->
                            [ viewInstructionName "mark", viewRegisterUse toBeCollectedFromRegister, viewInstructionName "as-moved-to", viewRegisterUse referenceToDualMemoryRegister ]

                        RegisterMachine.SwapMemory _ ->
                            [ viewInstructionName "swap-memory" ]

                        RegisterMachine.Unfinished _ ->
                            [ viewInstructionName "unfinished" ]
                    )
                ]
    in
    E.column [ E.width E.fill ]
        (instructionBlock
            |> convertInstructionBlock
            |> List.map
                (\labelOrInstruction ->
                    case labelOrInstruction of
                        Label label ->
                            viewLabelIntroduction label

                        Perform position instruction ->
                            viewInstruction { isCurrentInstruction = instructionPointer == position } instruction
                )
        )


viewConstant : Constant -> Element msg
viewConstant const =
    E.row []
        [ case const of
            Num x ->
                E.text (String.fromInt x)

            Nil ->
                E.text "nil"
        ]
