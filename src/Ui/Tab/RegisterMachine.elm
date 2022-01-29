module Ui.Tab.RegisterMachine exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import RegisterMachine.Base as RegisterMachine exposing (Controller, Machine, RuntimeError, TranslationError)
import RegisterMachine.Controllers as Controllers
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.Style.Button as Button


type alias Model =
    { controller : Controller
    , parsedMachine : Result TranslationError Machine
    , maybeRuntime : Maybe (Result RuntimeError Machine)
    }


init : InitContext Model Msg
init =
    let
        boolToInt : Bool -> Int
        boolToInt b =
            if b then
                1

            else
                0

        controller : Controller
        controller =
            -- Controllers.controller0_gcd
            Controllers.controller1_remainder

        env : RegisterMachine.RegisterEnvironment
        env =
            -- TODO: derive this automatically from the controller
            -- Dict.fromList [ ( "a", 3 * 5 * 7 ), ( "b", 3 * 5 * 5 ), ( "tmp", 0 ), ( "is-b-zero?", 0 ), ( "label-test", 0 ) ]
            Dict.fromList [ ( "a", 0 ), ( "b", 15 ), ( "is-finished?", 0 ) ]

        operationEnv : RegisterMachine.OperationEnvironment
        operationEnv =
            Dict.fromList
                [ ( "sub", RegisterMachine.makeOperation2 (\x y -> x - y) )
                , ( "less-than?", RegisterMachine.makeOperation2 (\x y -> boolToInt (x < y)) )
                , ( "add", RegisterMachine.makeOperation2 (\x y -> x + y) )
                , ( "mul", RegisterMachine.makeOperation2 (\x y -> x * y) )
                , ( "is-zero?", RegisterMachine.makeOperation1 (\x -> boolToInt (x == 0)) )
                , ( "eq?", RegisterMachine.makeOperation2 (\x y -> boolToInt (x == y)) )
                ]

        parsedMachine : Result TranslationError Machine
        parsedMachine =
            RegisterMachine.makeMachine controller env operationEnv
    in
    InitContext.setModelTo
        { controller = controller
        , parsedMachine = parsedMachine
        , maybeRuntime =
            case parsedMachine of
                Ok machine ->
                    Just (Ok machine)

                Err _ ->
                    Nothing
        }


resetRuntime : Model -> Model
resetRuntime model =
    { model
        | maybeRuntime =
            case model.parsedMachine of
                Ok machine ->
                    Just (Ok machine)

                Err _ ->
                    Nothing
    }


type Msg
    = Reset
    | Start
    | RunOneStep


update : Msg -> Context Model msg
update msg =
    case msg of
        Reset ->
            Context.update resetRuntime

        Start ->
            Context.update
                (\model ->
                    { model
                        | maybeRuntime =
                            model.maybeRuntime
                                |> Maybe.map (Result.andThen RegisterMachine.start)
                    }
                )

        RunOneStep ->
            Context.update
                (\model ->
                    { model
                        | maybeRuntime =
                            model.maybeRuntime
                                |> Maybe.map
                                    (\resultMachine ->
                                        resultMachine
                                            |> Result.andThen RegisterMachine.runOneStep
                                            |> Result.map .machine
                                    )
                    }
                )


view : Config -> Model -> Element Msg
view config model =
    -- 1. I need to display all the registers
    -- 2. I need to display the instruction block with labels
    E.column [ E.width E.fill ]
        [ E.row []
            [ Input.button Button.buttonStyle
                { onPress =
                    Just Reset
                , label = E.text "Reset"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    Just Start
                , label = E.text "Start"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    Just RunOneStep
                , label = E.text "Run one step"
                }
            ]
        , E.row [ E.width E.fill ]
            (case model.maybeRuntime of
                Nothing ->
                    []

                Just (Err runtimeError) ->
                    [ E.text "runtime error" ]

                Just (Ok machine) ->
                    [ viewRegisters (machine.env |> Dict.toList)
                    , viewStack machine.stack
                    , -- instructions
                      viewInstructions machine.instructionPointer model.controller.instructions
                    ]
            )
        ]


type LabelOrInstruction
    = Label RegisterMachine.Label
    | Perform Int RegisterMachine.Instruction


viewInstructions : Int -> RegisterMachine.InstructionBlock -> Element Msg
viewInstructions instructionPointer instructionBlock =
    let
        convertInstructionBlock : RegisterMachine.InstructionBlock -> List LabelOrInstruction
        convertInstructionBlock instructions =
            let
                update0 : RegisterMachine.LabelOrInstruction -> ( Int, List LabelOrInstruction ) -> ( Int, List LabelOrInstruction )
                update0 labelOrInstruction ( position, newInstructions ) =
                    case labelOrInstruction of
                        RegisterMachine.Label label ->
                            ( position, Label label :: newInstructions )

                        RegisterMachine.Perform instruction ->
                            ( position + 1, Perform position instruction :: newInstructions )
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
            E.row [ E.spacing 8 ] [ E.text "label ", E.row [] [ viewLabel label ] ]

        viewValue x =
            E.row [] [ E.text (String.fromInt x) ]

        paddingLeft px =
            E.paddingEach { left = px, top = 0, right = 0, bottom = 0 }

        viewOperationApplication : RegisterMachine.OperationApplication -> Element Msg
        viewOperationApplication (RegisterMachine.Operation opName registers) =
            viewOperationUse opName (registers |> List.map viewRegisterUse)

        viewInstruction : Bool -> RegisterMachine.Instruction -> Element Msg
        viewInstruction isFocused instruction =
            E.row
                (List.concat
                    [ [ E.spacing 8, paddingLeft 20 ]
                    , if isFocused then
                        [ Background.color (E.rgb255 215 215 215) ]

                      else
                        []
                    ]
                )
                (case instruction of
                    RegisterMachine.AssignRegister target source ->
                        [ viewRegisterName target, viewInstructionName "<-", viewRegisterUse source ]

                    RegisterMachine.AssignLabel target label ->
                        [ viewRegisterName target, viewInstructionName "<-", viewLabelUse label ]

                    RegisterMachine.AssignOperation target operationApplication ->
                        [ viewRegisterName target, viewInstructionName "<-", viewOperationApplication operationApplication ]

                    RegisterMachine.AssignConstant target x ->
                        [ viewRegisterName target, viewInstructionName "<-", viewValue x ]

                    RegisterMachine.JumpToLabel label ->
                        [ viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToLabelAtRegister target ->
                        [ viewInstructionName "jump", viewRegisterUse target ]

                    RegisterMachine.JumpToLabelIf testRegister label ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewLabelUse label ]

                    RegisterMachine.JumpToLabelAtRegisterIf testRegister target ->
                        [ viewInstructionName "if", viewRegisterUse testRegister, viewInstructionName "jump", viewRegisterUse target ]

                    RegisterMachine.Halt ->
                        [ viewInstructionName "halt" ]

                    RegisterMachine.PushRegister register ->
                        [ viewInstructionName "push", viewRegisterUse register ]

                    RegisterMachine.PushConstant val ->
                        [ viewInstructionName "push", viewValue val ]

                    RegisterMachine.PushLabel label ->
                        [ viewInstructionName "push", viewLabelUse label ]

                    RegisterMachine.Pop target ->
                        [ viewRegisterName target, viewInstructionName "<-", viewInstructionName "stack" ]
                )
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
                            viewInstruction (instructionPointer == position) instruction
                )
        )


viewRegisters : List ( RegisterMachine.Register, Int ) -> Element Msg
viewRegisters registers =
    let
        registerStyle =
            [ Background.color (E.rgb255 240 0 245)
            , E.padding 20
            ]

        viewRegister : RegisterMachine.Register -> Int -> Element Msg
        viewRegister name val =
            E.row [ E.spacing 10 ]
                [ E.el [ E.width (E.px 100) ] (E.text name), E.text "<-", E.el registerStyle (E.text (String.fromInt val)) ]
    in
    -- registers
    E.column [ E.width E.fill, E.spacing 5 ]
        (registers
            |> List.map (\( name, val ) -> viewRegister name val)
        )


viewStack : RegisterMachine.Stack -> Element Msg
viewStack stack =
    E.column [ E.width E.fill, E.spacing 5 ]
        (stack
            |> RegisterMachine.stackToList
            |> List.reverse
            |> List.map
                (\val ->
                    E.el [] (E.text (String.fromInt val))
                )
        )
