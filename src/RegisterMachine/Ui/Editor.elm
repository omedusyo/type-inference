module RegisterMachine.Ui.Editor exposing (..)

import Browser.Events as BE
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Lib.ZipList as ZipList exposing (ZipList)
import Lib.ZipListSelection as ZipListSelection exposing (ZipListSelection)
import List.Nonempty as NonemptyList
import RegisterMachine.Base as RegisterMachine
import RegisterMachine.Machine as RegisterMachine
import RegisterMachine.Ui.Editor.Base as Base
    exposing
        ( HorizontalDirection(..)
        , Instruction(..)
        , InstructionKind(..)
        , Node(..)
        , NodeKind(..)
        , NodeValidity(..)
        , VerticalDirection(..)
        , emptyNode
        , initialInstruction
        , initialInstructionValidity
        )
import RegisterMachine.Ui.Editor.Color as Color
import RegisterMachine.Ui.Editor.EditorToMachineInstructionsTranslator
import RegisterMachine.Ui.Editor.MachineInstructionsToEditorTranslator
import RegisterMachine.Ui.Editor.Validation as Validation exposing (validatedInstruction)
import RegisterMachine.Ui.MachineInstructions as RegisterMachineUI
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.InputCell as E
import Ui.Style.Button as Button
import Ui.Tab.PublicRegisterMachineMsg as PublicRegisterMachineMsg exposing (PublicRegisterMachineMsg)



--===UI===


type InstructionMode
    = TraversingInstructions NodeMode
    | InsertingInstruction
    | SelectingInstructions
    | Run


type NodeMode
    = TraversingNodes
    | EditingNode


type alias Model =
    -- TODO: This is wrong... it should be a sum of three models, one for each mode...
    { instructions : ZipList Instruction
    , instructionMode : InstructionMode
    , fragmentBoard : FragmentBoard
    , selectedInstructions : Maybe (ZipListSelection Instruction)
    , debugConsole : DebugConsole
    , instructionBlock : RegisterMachine.InstructionBlock
    }


init : RegisterMachine.InstructionBlock -> Effect rootMsg Msg Model
init instructionBlock =
    Effect.pure
        { instructions = ZipList.fromList exampleInstruction0 [ exampleInstruction1, exampleInstruction2, Halt ]

        -- TODO: What should be the default mode?
        -- , instructionMode = TraversingInstructions TraversingNodes
        , instructionMode = Run
        , fragmentBoard = initFragmentBoard
        , selectedInstructions = Nothing
        , debugConsole = initDebugConsole
        , instructionBlock = instructionBlock
        }



-- This is used for copy/pasting


type FragmentBoard
    = EmptyBoard
    | NonemptyBoard (ZipList Fragment)


type alias Fragment =
    NonemptyList.Nonempty Instruction


type alias DebugConsole =
    { instructionsRev : List Instruction
    }



-- ===Fragment Board===


initFragmentBoard : FragmentBoard
initFragmentBoard =
    EmptyBoard


pushFragment : Fragment -> Model -> Model
pushFragment fragment model =
    { model
        | fragmentBoard =
            case model.fragmentBoard of
                EmptyBoard ->
                    NonemptyBoard (ZipList.singleton fragment)

                NonemptyBoard fragments ->
                    NonemptyBoard (fragments |> ZipList.cons fragment)
    }


pasteAndPopFragment : VerticalDirection -> Model -> Model
pasteAndPopFragment direction =
    pasteFragment direction >> popFragment


popFragment : Model -> Model
popFragment ({ fragmentBoard } as model) =
    case fragmentBoard of
        EmptyBoard ->
            model

        NonemptyBoard fragments ->
            { model
                | fragmentBoard =
                    if ZipList.isSingleton fragments then
                        EmptyBoard

                    else
                        NonemptyBoard (fragments |> ZipList.deleteAndFocusLeft)
            }


pasteFragment : VerticalDirection -> Model -> Model
pasteFragment direction ({ fragmentBoard, instructions } as model) =
    case fragmentBoard of
        EmptyBoard ->
            model

        NonemptyBoard fragments ->
            { model
                | instructions =
                    case direction of
                        Up ->
                            instructions |> ZipList.insertListLeft (ZipList.current fragments |> NonemptyList.toList)

                        Down ->
                            instructions |> ZipList.insertListRight (ZipList.current fragments |> NonemptyList.toList)
            }


moveFragment : VerticalDirection -> Model -> Model
moveFragment direction ({ fragmentBoard } as model) =
    { model
        | fragmentBoard =
            case fragmentBoard of
                EmptyBoard ->
                    EmptyBoard

                NonemptyBoard fragments ->
                    case direction of
                        Up ->
                            NonemptyBoard (fragments |> ZipList.left)

                        Down ->
                            NonemptyBoard (fragments |> ZipList.right)
    }



-- ===Selection Mode===


setModeToSelectInstructions : Model -> Model
setModeToSelectInstructions model =
    { model
        | instructionMode = SelectingInstructions
        , selectedInstructions = Just (ZipListSelection.fromZipList model.instructions)
    }


setModeToRun : Model -> Model
setModeToRun model =
    { model | instructionMode = Run }


selectionMovement : VerticalDirection -> Model -> Model
selectionMovement direction model =
    case direction of
        Up ->
            { model | selectedInstructions = model.selectedInstructions |> Maybe.map ZipListSelection.up }

        Down ->
            { model | selectedInstructions = model.selectedInstructions |> Maybe.map ZipListSelection.down }



-- ===Debug Console===


initDebugConsole : DebugConsole
initDebugConsole =
    { instructionsRev = [] }



-- ===Instructions/Nodes===
-- TODO: Delete the below test instructions/functions


src =
    emptyNode Static Base.registerExpectation


opName =
    emptyNode Static Base.operationNameExpectation


argDyn =
    emptyNode Dynamic Base.argExpectation


lbl =
    emptyNode Static Base.labelExpectation


exampleInstruction0 : Instruction
exampleInstruction0 =
    Instruction OperationApplicationKind (ZipList.fromList src [ opName, argDyn, argDyn, argDyn ]) initialInstructionValidity


exampleInstruction1 : Instruction
exampleInstruction1 =
    Instruction OperationApplicationKind (ZipList.fromList src [ opName, argDyn ]) initialInstructionValidity


exampleInstruction2 : Instruction
exampleInstruction2 =
    Instruction LabelKind (ZipList.fromList lbl []) initialInstructionValidity


type Msg
    = SetModeTo InstructionMode
      -- Instructions
    | InstructionMovement VerticalDirection
    | SwapInstruction VerticalDirection
    | InstructionEdit
    | InstructionInsertion VerticalDirection
    | ChangeInstructionTo InstructionKind
    | DeleteInstruction
    | ConvertAssignmentToOperation
    | DuplicateInstruction VerticalDirection
    | JumpToBoundaryInstruction VerticalDirection
      -- Nodes
    | NodeMovement HorizontalDirection
    | NodeEdit String
    | NodeInsertion HorizontalDirection
    | JumpToBoundaryNode HorizontalDirection
    | DeleteNode
      -- Fragment Board
    | PushFragment
    | PasteFragment VerticalDirection
    | PasteAndPopFragment VerticalDirection
    | FragmentMovement VerticalDirection
      -- Selection
    | SelectionMovement VerticalDirection
      -- Debugging
    | DebugCurrentInstruction
    | ResetDebugConsole
      -- Execution
    | RegisterMachineExecutionKeyPress PublicRegisterMachineMsg


moveInstruction : VerticalDirection -> Model -> Model
moveInstruction direction model =
    { model
        | instructions =
            case direction of
                Up ->
                    model.instructions |> ZipList.left

                Down ->
                    model.instructions |> ZipList.right
    }


swapInstruction : VerticalDirection -> Model -> Model
swapInstruction direction model =
    { model
        | instructions =
            case direction of
                Up ->
                    model.instructions |> ZipList.swapWithLeft

                Down ->
                    model.instructions |> ZipList.swapWithRight
    }
        |> moveInstruction direction


duplicateInstruction : VerticalDirection -> Model -> Model
duplicateInstruction direction model =
    case direction of
        Up ->
            { model | instructions = ZipList.duplicateLeft model.instructions }

        Down ->
            { model | instructions = ZipList.duplicateRight model.instructions }


jumpToBoundaryInstruction : VerticalDirection -> Model -> Model
jumpToBoundaryInstruction direction model =
    case direction of
        Up ->
            { model | instructions = ZipList.jumpStart model.instructions }

        Down ->
            { model | instructions = ZipList.jumpEnd model.instructions }


insertFutureInstruction : VerticalDirection -> Model -> Model
insertFutureInstruction direction model =
    { model
        | instructions =
            model.instructions
                |> (case direction of
                        Up ->
                            ZipList.insertLeft (FutureInstruction Down)

                        Down ->
                            ZipList.insertRight (FutureInstruction Up)
                   )
    }


traverseNodes : HorizontalDirection -> Model -> Model
traverseNodes direction =
    case direction of
        Left ->
            updateNodesOfCurrentInstructionWithoutValidation ZipList.left

        Right ->
            updateNodesOfCurrentInstructionWithoutValidation ZipList.right


updateCurrentInstructionWithoutValidation : (Instruction -> Instruction) -> Model -> Model
updateCurrentInstructionWithoutValidation f model =
    { model
        | instructions =
            model.instructions
                |> ZipList.updateCurrent f
    }


updateCurrentNodesWithoutValidation : (ZipList Node -> Instruction -> ZipList Node) -> Model -> Model
updateCurrentNodesWithoutValidation f =
    updateCurrentInstructionWithoutValidation
        (\instruction ->
            case instruction of
                Instruction instructionKind nodes validation ->
                    Instruction instructionKind (f nodes instruction) validation

                _ ->
                    instruction
        )


getCurrentNodes : Model -> Maybe (ZipList Node)
getCurrentNodes model =
    case ZipList.current model.instructions of
        Instruction _ nodes _ ->
            Just nodes

        _ ->
            Nothing


getCurrentInstruction : Model -> Instruction
getCurrentInstruction model =
    ZipList.current model.instructions


setCurrentNodesWithoutValidation : ZipList Node -> Model -> Model
setCurrentNodesWithoutValidation nodes =
    updateCurrentNodesWithoutValidation (\_ _ -> nodes)


updateCurrentNodeWithoutValidation : (Node -> Node) -> Model -> Model
updateCurrentNodeWithoutValidation f =
    updateCurrentInstructionWithoutValidation
        (\instruction ->
            case instruction of
                Instruction instructionKind nodes validation ->
                    Instruction instructionKind (nodes |> ZipList.updateCurrent f) validation

                _ ->
                    instruction
        )


updateNodesOfCurrentInstructionWithoutValidation : (ZipList Node -> ZipList Node) -> Model -> Model
updateNodesOfCurrentInstructionWithoutValidation f model =
    { model
        | instructions =
            model.instructions
                |> ZipList.updateCurrent
                    (\instruction ->
                        case instruction of
                            Instruction kind nodes validation ->
                                Instruction kind (f nodes) validation

                            _ ->
                                instruction
                    )
    }



-- TODO: rename this to insertion of arguments


insertAndEditNodeWithoutValidation : HorizontalDirection -> Model -> Model
insertAndEditNodeWithoutValidation direction model =
    case getCurrentNodes model of
        Just nodes ->
            case ZipList.current nodes of
                -- TODO: Node validation
                Node Dynamic _ _ _ ->
                    model
                        |> setCurrentNodesWithoutValidation
                            (case direction of
                                Left ->
                                    nodes
                                        |> ZipList.insertLeft (Node Dynamic UnfinishedNode Base.argExpectation "")
                                        |> ZipList.left

                                Right ->
                                    nodes
                                        |> ZipList.insertRight (Node Dynamic UnfinishedNode Base.argExpectation "")
                                        |> ZipList.right
                            )
                        |> setModeToEditing

                _ ->
                    model

        Nothing ->
            model


deleteCurrentNodeWithValidation : Model -> Model
deleteCurrentNodeWithValidation model =
    case ZipList.current model.instructions of
        Instruction instructionKind nodes _ ->
            -- You can delete a dynamic node. You can't delete a static node.
            if isCurrentNodeStatic nodes then
                model

            else
                case instructionKind of
                    OperationApplicationKind ->
                        if ZipList.length nodes <= 3 then
                            model

                        else
                            -- But you can't delete every dynamic node.
                            -- If the current instruction is application, then we can't allow empty argument list
                            model
                                |> setCurrentNodesWithoutValidation (ZipList.deleteAndFocusRight nodes)
                                |> validateCurrentInstruction

                    _ ->
                        model
                            |> setCurrentNodesWithoutValidation (ZipList.deleteAndFocusRight nodes)
                            |> validateCurrentInstruction

        _ ->
            model


jumpToBoundaryNode : HorizontalDirection -> Model -> Model
jumpToBoundaryNode direction =
    case direction of
        Left ->
            updateNodesOfCurrentInstructionWithoutValidation ZipList.jumpStart

        Right ->
            updateNodesOfCurrentInstructionWithoutValidation ZipList.jumpEnd


isCurrentNodeStatic : ZipList Node -> Bool
isCurrentNodeStatic nodes =
    case ZipList.current nodes of
        Node Static _ _ _ ->
            True

        _ ->
            False


deleteCurrentInstruction : Model -> Model
deleteCurrentInstruction model =
    { model
        | instructions =
            case ZipList.current model.instructions of
                FutureInstruction direction ->
                    case direction of
                        Up ->
                            ZipList.deleteAndFocusLeft model.instructions

                        Down ->
                            ZipList.deleteAndFocusRight model.instructions

                _ ->
                    ZipList.deleteAndFocusRight model.instructions
    }


setModeToEditing : Model -> Model
setModeToEditing model =
    case ZipList.current model.instructions of
        Instruction _ _ _ ->
            { model | instructionMode = TraversingInstructions EditingNode }

        _ ->
            model


changeInstructionWithoutValidationTo : InstructionKind -> Model -> Model
changeInstructionWithoutValidationTo instructionKind model =
    model
        |> updateCurrentInstructionWithoutValidation (\_ -> initialInstruction instructionKind)
        |> setModeToTraversing


setModeToTraversing : Model -> Model
setModeToTraversing model =
    case ZipList.current model.instructions of
        FutureInstruction direction ->
            { model | instructionMode = TraversingInstructions TraversingNodes }
                |> deleteCurrentInstruction

        _ ->
            case model.instructionMode of
                InsertingInstruction ->
                    { model | instructionMode = TraversingInstructions TraversingNodes }

                _ ->
                    { model | instructionMode = TraversingInstructions TraversingNodes }
                        |> validateCurrentInstruction


setModeToInsertInstruction : Model -> Model
setModeToInsertInstruction model =
    { model | instructionMode = InsertingInstruction }


validateCurrentInstruction : Model -> Model
validateCurrentInstruction =
    updateCurrentInstructionWithoutValidation
        (\instruction ->
            case instruction of
                Instruction instructionKind nodes _ ->
                    validatedInstruction instructionKind nodes

                Halt ->
                    instruction

                FutureInstruction _ ->
                    instruction
        )


update : (PublicRegisterMachineMsg -> Cmd rootMsg) -> Msg -> Action rootMsg Msg Model
update triggerRegisterMachineMsg msg =
    case msg of
        InstructionMovement direction ->
            Context.from (moveInstruction direction)

        SwapInstruction direction ->
            Context.from (swapInstruction direction)

        InstructionEdit ->
            Context.from setModeToInsertInstruction

        InstructionInsertion direction ->
            Context.from (insertFutureInstruction direction >> moveInstruction direction >> setModeToInsertInstruction)

        ChangeInstructionTo instructionKind ->
            Context.from (changeInstructionWithoutValidationTo instructionKind >> validateCurrentInstruction)

        NodeMovement direction ->
            Context.from (traverseNodes direction)

        DeleteInstruction ->
            Context.from (\model -> { model | instructions = ZipList.deleteAndFocusRight model.instructions })

        ConvertAssignmentToOperation ->
            Context.from
                (updateCurrentInstructionWithoutValidation
                    (\instruction ->
                        let
                            newInstruction =
                                case instruction of
                                    Instruction AssignmentKind nodes validity ->
                                        Instruction
                                            OperationApplicationKind
                                            ((nodes |> ZipList.updateLast Validation.setNodeToOperationNameNode) |> ZipList.insertAtEnd Base.argNode)
                                            validity

                                    _ ->
                                        instruction
                        in
                        newInstruction
                    )
                    >> validateCurrentInstruction
                )

        DuplicateInstruction direction ->
            Context.from (duplicateInstruction direction >> moveInstruction direction)

        JumpToBoundaryInstruction direction ->
            Context.from (jumpToBoundaryInstruction direction)

        SetModeTo instructionMode ->
            case instructionMode of
                TraversingInstructions nodeMode ->
                    case nodeMode of
                        TraversingNodes ->
                            Context.from setModeToTraversing

                        EditingNode ->
                            Context.from setModeToEditing

                InsertingInstruction ->
                    Context.from setModeToInsertInstruction

                SelectingInstructions ->
                    Context.from setModeToSelectInstructions

                Run ->
                    Context.from setModeToRun

        NodeEdit str ->
            -- TODO: Node validation
            Context.from (updateCurrentNodeWithoutValidation (\(Node nodeKind nodeValidation nodeExpectation _) -> Node nodeKind nodeValidation nodeExpectation str))

        NodeInsertion direction ->
            Context.from (insertAndEditNodeWithoutValidation direction)

        DeleteNode ->
            Context.from deleteCurrentNodeWithValidation

        PushFragment ->
            Context.from
                (\model ->
                    let
                        currentInstruction =
                            getCurrentInstruction model
                    in
                    model |> pushFragment (NonemptyList.singleton currentInstruction)
                )

        PasteFragment direction ->
            Context.from (pasteFragment direction)

        PasteAndPopFragment direction ->
            Context.from (pasteAndPopFragment direction)

        FragmentMovement direction ->
            Context.from (moveFragment direction)

        SelectionMovement direction ->
            Context.from (selectionMovement direction)

        JumpToBoundaryNode direction ->
            Context.from (jumpToBoundaryNode direction)

        DebugCurrentInstruction ->
            Context.from
                (\({ debugConsole } as model) ->
                    let
                        currentInstruction =
                            getCurrentInstruction model

                        _ =
                            Debug.log "CURRENT-INSTRUCTION" currentInstruction
                    in
                    { model | debugConsole = { debugConsole | instructionsRev = currentInstruction :: debugConsole.instructionsRev } }
                )

        ResetDebugConsole ->
            Context.from (\({ debugConsole } as model) -> { model | debugConsole = { debugConsole | instructionsRev = [] } })

        RegisterMachineExecutionKeyPress publicRegisterMachineMsg ->
            Context.rootCommand (triggerRegisterMachineMsg publicRegisterMachineMsg)


view : RegisterMachine.InstructionPointer -> Model -> Element Msg
view currentInstructionPointer ({ instructions } as model) =
    E.column []
        [ case model.instructionMode of
            TraversingInstructions nodeMode ->
                case nodeMode of
                    TraversingNodes ->
                        E.el [] (E.text "Moving")

                    EditingNode ->
                        E.el [] (E.text "Editing")

            InsertingInstruction ->
                E.el [] (E.text "Inserting")

            SelectingInstructions ->
                E.el [] (E.text "Selecting")

            Run ->
                E.el [] (E.text "Running")
        , E.el []
            (case model.instructionMode of
                Run ->
                    RegisterMachineUI.viewInstructions currentInstructionPointer model.instructionBlock

                _ ->
                    E.column []
                        [ let
                            bgColor =
                                E.rgb255 215 215 215
                          in
                          E.column []
                            (case model.instructionMode of
                                SelectingInstructions ->
                                    case model.selectedInstructions of
                                        Just instructionSelection ->
                                            instructionSelection
                                                |> ZipListSelection.mapToList
                                                    { current =
                                                        \instruction ->
                                                            E.el [ Background.color bgColor ] (viewInstruction { isInstructionSelected = True, isNodeSelected = False } model.instructionMode instruction)
                                                    , others =
                                                        \instruction ->
                                                            E.el [] (viewInstruction { isInstructionSelected = False, isNodeSelected = False } model.instructionMode instruction)
                                                    }

                                        Nothing ->
                                            [ viewKeyword "---should not be ever displayed---" ]

                                _ ->
                                    instructions
                                        |> ZipList.mapToList
                                            { current =
                                                \instruction ->
                                                    E.el [ Background.color bgColor ] (viewInstruction { isInstructionSelected = True, isNodeSelected = True } model.instructionMode instruction)
                                            , others =
                                                \instruction ->
                                                    E.el [] (viewInstruction { isInstructionSelected = False, isNodeSelected = False } model.instructionMode instruction)
                                            }
                            )

                        -- Fragment Board
                        , E.el [] (E.text "===Fragment Board===")
                        , viewFragmentBoard model

                        -- Debugging
                        , E.el [] (E.text "===Debugging===")
                        , viewDebuggingConsole model
                        ]
            )
        ]


viewFragmentBoard : Model -> Element Msg
viewFragmentBoard { fragmentBoard } =
    let
        viewFragment : Fragment -> Element Msg
        viewFragment fragment =
            E.column []
                (fragment
                    |> NonemptyList.toList
                    |> List.map (viewInstruction { isInstructionSelected = False, isNodeSelected = False } (TraversingInstructions TraversingNodes))
                )
    in
    case fragmentBoard of
        EmptyBoard ->
            E.text ""

        NonemptyBoard fragments ->
            E.column [ E.spacing 15 ]
                (fragments
                    |> ZipList.mapToList
                        { current =
                            \currentFragment ->
                                E.el [ Border.solid, Border.widthEach { left = 1, bottom = 0, right = 0, top = 0 } ] (viewFragment currentFragment)
                        , others =
                            \fragment ->
                                E.el [] (viewFragment fragment)
                        }
                )


viewDebuggingConsole : Model -> Element Msg
viewDebuggingConsole model =
    E.column []
        [ Input.button Button.buttonStyle
            { onPress =
                Just ResetDebugConsole
            , label = E.text "reset"
            }
        , E.column []
            (model.debugConsole.instructionsRev
                |> List.reverse
                |> List.map
                    (\instruction ->
                        case instruction of
                            Base.Instruction instructionKind ( revLeftNodes, selectedNode, rightNodes ) instructionValidity ->
                                let
                                    viewDebuggedNode : Bool -> Node -> Element Msg
                                    viewDebuggedNode isSelected ((Node nodeKind nodeValidity nodeExpectations text) as node) =
                                        E.row [ E.spacing 5 ]
                                            [ E.el [ Font.bold ] (E.text "Node")
                                            , E.text <|
                                                case nodeKind of
                                                    Static ->
                                                        "S"

                                                    Dynamic ->
                                                        "D"
                                            , E.el [ Font.bold ] (E.text "[ ")
                                            , -- This shows node's validity already
                                              viewNode isSelected True TraversingNodes node
                                            , E.row []
                                                [ E.el [ Font.bold, Font.color (E.rgb 0 0 1) ] (E.text ": {")
                                                , E.row []
                                                    (nodeExpectations
                                                        |> List.map
                                                            (\nodeExpectation ->
                                                                E.text <|
                                                                    case nodeExpectation of
                                                                        Base.RegisterName ->
                                                                            "reg-name"

                                                                        Base.RegisterUse ->
                                                                            "reg-use"

                                                                        Base.Label ->
                                                                            "label"

                                                                        Base.LabelUse ->
                                                                            "label-use"

                                                                        Base.Integer ->
                                                                            "int"

                                                                        Base.Nil ->
                                                                            "nil"

                                                                        Base.OperationName ->
                                                                            "op"
                                                            )
                                                        |> List.intersperse (E.text ", ")
                                                    )
                                                , E.el [ Font.bold, Font.color (E.rgb 0 0 1) ] (E.text "}")
                                                ]
                                            , E.el [ Font.bold ] (E.text "]")
                                            ]
                                in
                                E.row [ E.spacing 10 ]
                                    [ E.el [ Font.bold, Font.color (E.rgb 0 0 1) ] <|
                                        E.text <|
                                            case instructionKind of
                                                LabelKind ->
                                                    "Label"

                                                OperationApplicationKind ->
                                                    "OperationApplication"

                                                AssignmentKind ->
                                                    "Assignment"

                                                JumpKind ->
                                                    "Jump"

                                                JumpIfKind ->
                                                    "JumpIf"

                                                PushKind ->
                                                    "Push"

                                                HaltKind ->
                                                    "Halt"
                                    , case instructionValidity of
                                        Base.EveryNodeIsValid ->
                                            E.el [ Font.color (E.rgb 0 1 0) ] (E.text "all-valid")

                                        Base.ContainsErrorNodes ->
                                            E.el [ Font.color (E.rgb 1 0 0) ] (E.text "has-errors")

                                        Base.ContainsUnfinishedNodes ->
                                            E.text "unfinished"

                                        -- WrongArity { expected : ExpectedArity, received : Int } ->
                                        Base.WrongArity { expected, received } ->
                                            E.text <|
                                                String.concat
                                                    [ "wrong-arity(expected "
                                                    , case expected of
                                                        Base.Atleast x ->
                                                            "atleast" ++ String.fromInt x

                                                        Base.Exactly x ->
                                                            "exactly" ++ String.fromInt x
                                                    , " received "
                                                    , String.fromInt received
                                                    , ")"
                                                    ]
                                    , E.row [ E.spacing 10 ]
                                        (List.concat
                                            [ revLeftNodes
                                                |> List.reverse
                                                |> List.map (viewDebuggedNode False)
                                            , [ viewDebuggedNode True selectedNode ]
                                            , rightNodes
                                                |> List.map (viewDebuggedNode False)
                                            ]
                                            |> List.intersperse (E.el [] (E.text ","))
                                        )
                                    ]

                            Base.Halt ->
                                Debug.todo ""

                            Base.FutureInstruction verticalDirection ->
                                Debug.todo ""
                    )
            )
        ]


viewKeyword : String -> Element Msg
viewKeyword name =
    E.el [ Font.heavy ] (E.text name)


viewInstruction : { isInstructionSelected : Bool, isNodeSelected : Bool } -> InstructionMode -> Instruction -> Element Msg
viewInstruction { isInstructionSelected, isNodeSelected } instructionMode instruction =
    let
        viewBareInstruction : NodeMode -> Element Msg
        viewBareInstruction nodeMode =
            case instruction of
                Halt ->
                    viewKeyword "Halt"

                FutureInstruction _ ->
                    viewKeyword "---should not be ever displayed---"

                Instruction kind nodes validation ->
                    -- TODO: Display the validation information
                    case kind of
                        LabelKind ->
                            E.row []
                                [ viewKeyword "label ", viewNode isNodeSelected isInstructionSelected nodeMode (ZipList.current nodes), viewKeyword ":" ]

                        OperationApplicationKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isSourceSelected, source ) :: ( isOperationNameSelected, operationName ) :: arguments ->
                                        List.concat
                                            [ [ viewNode (isSourceSelected && isNodeSelected) isInstructionSelected nodeMode source
                                              , viewKeyword " <- "
                                              , viewNode (isOperationNameSelected && isNodeSelected) isInstructionSelected nodeMode operationName
                                              , viewKeyword "("
                                              ]
                                            , arguments
                                                |> List.map (\( isArgSelected, arg ) -> viewNode (isArgSelected && isNodeSelected) isInstructionSelected nodeMode arg)
                                                |> List.intersperse (viewKeyword ", ")
                                            , [ viewKeyword ")" ]
                                            ]

                                    _ ->
                                        [ E.text "error when viewing OperationApplicationKind" ]
                                )

                        AssignmentKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isSourceSelected, source ) :: ( isTargetSelected, target ) :: [] ->
                                        [ viewNode (isSourceSelected && isNodeSelected) isInstructionSelected nodeMode source
                                        , viewKeyword " <- "
                                        , viewNode (isTargetSelected && isNodeSelected) isInstructionSelected nodeMode target
                                        ]

                                    _ ->
                                        [ E.text "error when viewing AssignmentKind" ]
                                )

                        JumpKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isArgSelected, arg ) :: [] ->
                                        [ viewKeyword "jump "
                                        , viewNode (isArgSelected && isNodeSelected) isInstructionSelected nodeMode arg
                                        ]

                                    _ ->
                                        [ E.text "error when viewing JumpKind" ]
                                )

                        JumpIfKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isTestSelected, test ) :: ( isArgSelected, arg ) :: [] ->
                                        [ viewKeyword "if "
                                        , viewNode (isTestSelected && isNodeSelected) isInstructionSelected nodeMode test
                                        , viewKeyword " jump "
                                        , viewNode (isArgSelected && isNodeSelected) isInstructionSelected nodeMode arg
                                        ]

                                    _ ->
                                        [ E.text "error when viewing JumpIfKind" ]
                                )

                        HaltKind ->
                            -- TODO: remove
                            viewKeyword "Halt"

                        PushKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isArgSelected, arg ) :: [] ->
                                        [ viewKeyword "push "
                                        , viewNode (isArgSelected && isNodeSelected) isInstructionSelected nodeMode arg
                                        ]

                                    _ ->
                                        [ E.text "error when viewing JumpKind" ]
                                )
    in
    case instructionMode of
        InsertingInstruction ->
            if isInstructionSelected then
                E.column []
                    [ E.row []
                        ([ E.row [] [ viewKeyword "q:", E.text "label" ]
                         , E.row [] [ viewKeyword "w:", E.text "jump" ]
                         , E.row [] [ viewKeyword "e:", E.text "if-jump" ]
                         ]
                            |> List.intersperse (E.text " ")
                        )
                    , E.row []
                        ([ E.row [] [ viewKeyword "a:", E.text "apply" ]
                         , E.row [] [ viewKeyword "s:", E.text "assign" ]
                         , E.row [] [ viewKeyword "d:", E.text "push" ]
                         , E.row [] [ viewKeyword "f:", E.text "halt" ]
                         ]
                            |> List.intersperse (E.text " ")
                        )
                    ]

            else
                viewBareInstruction TraversingNodes

        TraversingInstructions nodeMode ->
            viewBareInstruction nodeMode

        SelectingInstructions ->
            viewBareInstruction TraversingNodes

        Run ->
            -- TODO
            E.text "running..."


viewNode : Bool -> Bool -> NodeMode -> Node -> Element Msg
viewNode isSelected isInstructionSelected nodeMode (Node _ nodeValidation nodeExpectation str) =
    let
        viewStr str0 =
            case nodeValidation of
                UnfinishedNode ->
                    viewHole

                ErrorNode ->
                    E.el [ Background.color Color.error ] (E.text str0)

                ValidNode entityKind ->
                    case entityKind of
                        Base.RegisterName ->
                            E.el [ Font.color Color.register ] (E.text str0)

                        Base.RegisterUse ->
                            E.el [ Font.color Color.register ] (E.text str0)

                        Base.Label ->
                            E.el [ Font.color Color.label ] (E.text str0)

                        Base.LabelUse ->
                            E.el [ Font.color Color.label ] (E.text str0)

                        Base.Integer ->
                            E.el [] (E.text str0)

                        Base.Nil ->
                            E.el [] (E.text str0)

                        Base.OperationName ->
                            E.el [] (E.text str0)
    in
    if isSelected && isInstructionSelected then
        case nodeMode of
            TraversingNodes ->
                E.el [ Border.width 1, Border.solid ] (viewStr str)

            EditingNode ->
                E.inputCell 19 str NodeEdit

    else
        E.el [] (viewStr str)


viewLabel label =
    E.el [ Font.color (E.rgb255 239 151 0) ] (E.text label)


viewLabelUse label =
    viewLabel (":" ++ label)


viewLabelIntroduction label =
    E.row [ E.spacing 8 ] [ E.text "label ", E.row [] [ viewLabel label ] ]


viewHole : Element Msg
viewHole =
    let
        r =
            5

        d =
            2 * r
    in
    E.el
        [ E.width (E.px d)
        , E.height (E.px d)
        , Border.rounded r
        , Background.color (E.rgb255 183 183 183)
        ]
        (E.text "")



-- ===Keybindings===


type alias KeyCode =
    String


traverseModeKeyBindings : Dict KeyCode Msg
traverseModeKeyBindings =
    Dict.fromList
        [ -- ===Instructions===
          ( "k", InstructionMovement Up )
        , ( "j", InstructionMovement Down )
        , ( "K", SwapInstruction Up )
        , ( "J", SwapInstruction Down )
        , ( "i", InstructionEdit )
        , ( "o", InstructionInsertion Down )
        , ( "O", InstructionInsertion Up )
        , ( "f", DuplicateInstruction Down )
        , ( "F", DuplicateInstruction Up )
        , ( "3", JumpToBoundaryInstruction Up )
        , ( "4", JumpToBoundaryInstruction Down )

        -- ===Nodes===
        , ( "s", NodeMovement Left )
        , ( "d", NodeMovement Right )
        , ( "e", SetModeTo (TraversingInstructions EditingNode) )
        , ( "<", NodeInsertion Left )
        , ( ",", NodeInsertion Right )
        , ( "X", DeleteInstruction )
        , ( "x", DeleteNode )
        , ( "(", ConvertAssignmentToOperation )
        , ( "#", JumpToBoundaryNode Left )
        , ( "$", JumpToBoundaryNode Right )

        -- ===Fragment Board===
        , ( "c", PushFragment )
        , ( "p", PasteFragment Down )
        , ( "P", PasteFragment Up )
        , ( "v", PasteAndPopFragment Down )
        , ( "V", PasteAndPopFragment Up )

        -- Terrible keybindings
        , ( "5", FragmentMovement Down )
        , ( "6", FragmentMovement Up )

        -- ===Selection===
        , ( "m", SetModeTo SelectingInstructions )

        -- ===Run mode===
        , ( "r", SetModeTo Run )

        -- ===Debugger===
        , ( "?", DebugCurrentInstruction )
        ]


insertionModeKeyBindings : Dict KeyCode InstructionKind
insertionModeKeyBindings =
    Dict.fromList
        [ ( "q", LabelKind )
        , ( "l", LabelKind )
        , ( "w", JumpKind )
        , ( "e", JumpIfKind )
        , ( "a", OperationApplicationKind )
        , ( "s", AssignmentKind )
        , ( "d", PushKind )
        , ( "f", HaltKind )
        ]


selectionModeKeyBindings : Dict KeyCode Msg
selectionModeKeyBindings =
    Dict.fromList
        [ ( "k", SelectionMovement Up )
        , ( "j", SelectionMovement Down )

        -- TODO: X delete
        -- TODO: c copy
        ]


runModeKeyBindings : Dict KeyCode Msg
runModeKeyBindings =
    Dict.fromList
        [ ( "r", RegisterMachineExecutionKeyPress PublicRegisterMachineMsg.Step )
        , ( "f", RegisterMachineExecutionKeyPress PublicRegisterMachineMsg.StepUntilNextJump )
        , ( "c", RegisterMachineExecutionKeyPress PublicRegisterMachineMsg.StepUntilHalted )
        , ( "z", RegisterMachineExecutionKeyPress PublicRegisterMachineMsg.Reset )
        , ( "e", SetModeTo (TraversingInstructions TraversingNodes) )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    BE.onKeyUp
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\keyCode ->
                    -- let
                    --     _ =
                    --         Debug.log "key == " keyCode
                    -- in
                    case model.instructionMode of
                        TraversingInstructions nodeMode ->
                            case nodeMode of
                                TraversingNodes ->
                                    case Dict.get keyCode traverseModeKeyBindings of
                                        Just msg ->
                                            Decode.succeed msg

                                        Nothing ->
                                            Decode.fail ""

                                EditingNode ->
                                    case keyCode of
                                        "Escape" ->
                                            Decode.succeed (SetModeTo (TraversingInstructions TraversingNodes))

                                        _ ->
                                            Decode.fail ""

                        InsertingInstruction ->
                            case Dict.get keyCode insertionModeKeyBindings of
                                Just instructionKind ->
                                    Decode.succeed (ChangeInstructionTo instructionKind)

                                Nothing ->
                                    case keyCode of
                                        "Escape" ->
                                            Decode.succeed (SetModeTo (TraversingInstructions TraversingNodes))

                                        _ ->
                                            Decode.fail ""

                        SelectingInstructions ->
                            case Dict.get keyCode selectionModeKeyBindings of
                                Just msg ->
                                    Decode.succeed msg

                                Nothing ->
                                    case keyCode of
                                        "Escape" ->
                                            Decode.succeed (SetModeTo (TraversingInstructions TraversingNodes))

                                        _ ->
                                            Decode.fail ""

                        Run ->
                            case Dict.get keyCode runModeKeyBindings of
                                Just msg ->
                                    Decode.succeed msg

                                Nothing ->
                                    Decode.fail ""
                )
        )
