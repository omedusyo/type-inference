module RegisterMachine.Show exposing (..)

import RegisterMachine.Base as RegisterMachine exposing (..)
import RegisterMachine.Machine as RegisterMachine exposing (LabelOrInstruction(..))


showConstant : Constant -> String
showConstant val =
    case val of
        Num x ->
            String.fromInt x

        Nil ->
            "nil"


showLabel : Label -> String
showLabel label =
    ":" ++ label


showRegisterUse : Register -> String
showRegisterUse register =
    "$" ++ register


showAssignment : String -> String -> String
showAssignment source target =
    source ++ " <- " ++ target


showArgument : OperationArgument -> String
showArgument argument =
    case argument of
        Register register ->
            showRegisterUse register

        Constant val ->
            showConstant val


showOperation : Register -> String -> List OperationArgument -> String
showOperation target opName arguments =
    showAssignment
        target
        (String.concat
            [ opName
            , "("
            , arguments
                |> List.map showArgument
                |> String.join ", "
            , ")"
            ]
        )


showInstruction : Instruction -> String
showInstruction instruction =
    case instruction of
        AssignRegister target source ->
            showAssignment target (showRegisterUse source)

        AssignLabel target label ->
            showAssignment target (showLabel label)

        AssignOperation target (Operation opName arguments) ->
            showOperation target opName arguments

        AssignConstant target val ->
            showAssignment target (showConstant val)

        JumpToLabel label ->
            String.concat [ "jump ", showLabel label ]

        JumpToInstructionPointerAtRegister target ->
            String.concat [ "jump ", showRegisterUse target ]

        JumpToLabelIf testRegister label ->
            String.concat [ "if ", showRegisterUse testRegister, " jump ", showLabel label ]

        JumpToInstructionPointerAtRegisterIf testRegister target ->
            String.concat [ "if ", showRegisterUse testRegister, " jump ", showRegisterUse target ]

        Halt ->
            "halt"

        PushRegister register ->
            String.concat [ "push ", showRegisterUse register ]

        PushConstant val ->
            String.concat [ "push ", showConstant val ]

        PushLabel label ->
            String.concat [ "push ", showLabel label ]

        Pop target ->
            showAssignment target "pop-stack"

        AssignCallAtLabel target label ->
            showAssignment target (showLabel label)

        AssignCallAtRegister target labelRegister ->
            showAssignment target (showRegisterUse labelRegister)

        ConstructPair target arg0 arg1 ->
            showOperation target "pair" [ arg0, arg1 ]

        First target source ->
            showOperation target "first" [ Register source ]

        Second target source ->
            showOperation target "second" [ Register source ]

        SetFirst register arg ->
            String.concat [ "set-first ", register, showArgument arg ]

        SetSecond register arg ->
            String.concat [ "set-second ", register, showArgument arg ]


showInstructions : List LabelOrInstruction -> String
showInstructions instructions =
    instructions
        |> List.map
            (\labelOrInstruction ->
                case labelOrInstruction of
                    Label label ->
                        label ++ ":"

                    Perform instruction ->
                        "  " ++ showInstruction instruction
            )
        |> String.join "\n"
