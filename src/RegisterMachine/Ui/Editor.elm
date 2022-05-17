module RegisterMachine.Ui.Editor exposing (..)

import Browser.Dom as Dom
import Browser.Events as BE
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Lib.ZipList as ZipList exposing (ZipList)
import Task
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.InputCell as E


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


type NodeKind
    = Static
    | Dynamic


type Node
    = Node NodeKind String


type RegisterUse
    = WAt1


type ReturnsValue
    = -- Register Use, Constant, Label
      Wat2


type ReturnsLabel
    = -- RegisterUse (that contains a label) or a label
      Wat


type
    Source
    -- Register Use, Constant, Label
    -- Operation Application
    = NADA



-- type Value =
--   Label Constant Register
--===UI===


type InstructionMode
    = TraversingInstructions NodeMode
    | InsertingInstruction


type NodeMode
    = TraversingNodes
    | EditingNode


type Instruction
    = Instruction InstructionKind (ZipList Node)
    | Halt
      -- The only way to have a future instruction is when the user wishes to insert a completely new instruction.
      -- Then we temporarily create the FutureInstruction until the user decides with which concerete instructio to replace it with.
      -- But if the user presses Esc during his decision, the current FutureInstruction should be deleted.
      -- After the deletion in which direction should we move to? Up or Down? That's why we have the VerticalDirection argument.
    | FutureInstruction VerticalDirection


type alias Model =
    { instructions : ZipList Instruction
    , instructionMode : InstructionMode
    }


emptyStaticNode : Node
emptyStaticNode =
    Node Static ""


emptyDynamicNode : Node
emptyDynamicNode =
    Node Dynamic ""


initialInstruction : InstructionKind -> Instruction
initialInstruction instructionKind =
    case instructionKind of
        LabelKind ->
            -- label _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])

        OperationApplicationKind ->
            -- _ <- _(_)
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode, emptyDynamicNode ])

        AssignmentKind ->
            -- _ <- _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode ])

        JumpKind ->
            -- jump _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])

        JumpIfKind ->
            -- if _ jump _
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [ emptyStaticNode ])

        HaltKind ->
            Halt

        PushKind ->
            Instruction instructionKind
                (ZipList.fromList emptyStaticNode [])


instruction0 : Instruction
instruction0 =
    Instruction OperationApplicationKind (ZipList.fromList (Node Static "") [ Node Static "", Node Dynamic "", Node Dynamic "", Node Dynamic "" ])


instruction1 : Instruction
instruction1 =
    Instruction OperationApplicationKind (ZipList.fromList (Node Static "") [ Node Static "", Node Dynamic "" ])


instruction2 : Instruction
instruction2 =
    Instruction LabelKind (ZipList.fromList (Node Static "") [])


init : InitContext Msg Model
init =
    InitContext.setModelTo
        { instructions = ZipList.fromList instruction0 [ instruction1, instruction2, Halt ]
        , instructionMode = TraversingInstructions TraversingNodes
        }


type HorizontalDirection
    = Left
    | Right


type VerticalDirection
    = Up
    | Down


type Msg
    = SetModeTo InstructionMode
      -- Instructions
    | InstructionMovement VerticalDirection
    | InstructionEdit
    | InstructionInsertion VerticalDirection
    | ChangeInstructionTo InstructionKind
    | DeleteInstruction
      -- Nodes
    | NodeMovement HorizontalDirection
    | NodeEdit String
    | NodeInsertion HorizontalDirection
    | DeleteNode


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


moveNode : HorizontalDirection -> Model -> Model
moveNode direction model =
    { model
        | instructions =
            model.instructions
                |> ZipList.updateCurrent
                    (\instruction ->
                        case instruction of
                            Instruction kind nodes ->
                                case direction of
                                    Left ->
                                        Instruction kind (nodes |> ZipList.left)

                                    Right ->
                                        Instruction kind (nodes |> ZipList.right)

                            _ ->
                                instruction
                    )
    }


updateCurrentInstruction : (Instruction -> Instruction) -> Model -> Model
updateCurrentInstruction f model =
    { model
        | instructions =
            model.instructions
                |> ZipList.updateCurrent f
    }


updateCurrentNodes : (ZipList Node -> ZipList Node) -> Model -> Model
updateCurrentNodes f =
    updateCurrentInstruction
        (\instruction ->
            case instruction of
                Instruction instructionKind nodes ->
                    Instruction instructionKind (f nodes)

                _ ->
                    instruction
        )


getCurrentNodes : Model -> Maybe (ZipList Node)
getCurrentNodes model =
    case ZipList.current model.instructions of
        Instruction _ nodes ->
            Just nodes

        _ ->
            Nothing


setCurrentNodes : ZipList Node -> Model -> Model
setCurrentNodes nodes =
    updateCurrentNodes (\_ -> nodes)


updateCurrentNode : (Node -> Node) -> Model -> Model
updateCurrentNode f =
    updateCurrentInstruction
        (\instruction ->
            case instruction of
                Instruction instructionKind nodes ->
                    Instruction instructionKind
                        (nodes
                            |> ZipList.updateCurrent f
                        )

                _ ->
                    instruction
        )


insertAndEditNode : HorizontalDirection -> Model -> Model
insertAndEditNode direction model =
    case getCurrentNodes model of
        Just nodes ->
            case ZipList.current nodes of
                Node Dynamic str ->
                    model
                        |> setCurrentNodes
                            (case direction of
                                Left ->
                                    nodes
                                        |> ZipList.insertLeft (Node Dynamic "")
                                        |> ZipList.left

                                Right ->
                                    nodes
                                        |> ZipList.insertRight (Node Dynamic "")
                                        |> ZipList.right
                            )
                        |> setModeToEditing

                _ ->
                    model

        Nothing ->
            model


isCurrentNodeStatic : ZipList Node -> Bool
isCurrentNodeStatic nodes =
    case ZipList.current nodes of
        Node Static _ ->
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
        Instruction _ _ ->
            { model | instructionMode = TraversingInstructions EditingNode }

        _ ->
            model


changeInstructionTo : InstructionKind -> Model -> Model
changeInstructionTo instructionKind model =
    model
        |> updateCurrentInstruction (\_ -> initialInstruction instructionKind)
        |> setModeToTraversing


setModeToTraversing : Model -> Model
setModeToTraversing model =
    case ZipList.current model.instructions of
        FutureInstruction direction ->
            { model | instructionMode = TraversingInstructions TraversingNodes }
                |> deleteCurrentInstruction

        _ ->
            { model | instructionMode = TraversingInstructions TraversingNodes }


setModeToInsertInstruction : Model -> Model
setModeToInsertInstruction model =
    { model | instructionMode = InsertingInstruction }


update : Msg -> Context rootMsg Msg Model
update msg =
    case msg of
        InstructionMovement direction ->
            Context.update (moveInstruction direction)

        InstructionEdit ->
            Context.update setModeToInsertInstruction

        InstructionInsertion direction ->
            Context.update (insertFutureInstruction direction >> moveInstruction direction >> setModeToInsertInstruction)

        ChangeInstructionTo instructionKind ->
            Context.update (changeInstructionTo instructionKind)

        NodeMovement direction ->
            Context.update (moveNode direction)

        DeleteInstruction ->
            Context.update (\model -> { model | instructions = ZipList.deleteAndFocusRight model.instructions })

        SetModeTo instructionMode ->
            case instructionMode of
                TraversingInstructions nodeMode ->
                    case nodeMode of
                        TraversingNodes ->
                            Context.update setModeToTraversing

                        EditingNode ->
                            Context.update setModeToEditing

                InsertingInstruction ->
                    Context.update setModeToInsertInstruction

        NodeEdit str ->
            Context.update (updateCurrentNode (\(Node nodeKind _) -> Node nodeKind str))

        NodeInsertion direction ->
            Context.update (insertAndEditNode direction)

        DeleteNode ->
            Context.update
                (updateCurrentNodes
                    -- TODO: Nope... you can delete a dynamic node. You can't delete a static node.
                    -- But you can't delete every dynamic node.
                    -- You can only delete a dynamic node whose left neighbour is dynamic
                    (\nodes ->
                        if isCurrentNodeStatic nodes || ZipList.isSingleton nodes then
                            nodes

                        else
                            ZipList.deleteAndFocusRight nodes
                    )
                )


view : Model -> Element Msg
view ({ instructions } as model) =
    E.column []
        [ case model.instructionMode of
            TraversingInstructions nodeMode ->
                case nodeMode of
                    TraversingNodes ->
                        E.el [] (E.text "Moving")

                    EditingNode ->
                        E.el [] (E.text "Editing")

            InsertingInstruction ->
                E.el [] (E.text "Inserting Instruction")
        , E.column []
            (instructions
                |> ZipList.mapToList
                    { current =
                        \instruction ->
                            E.el [ Background.color (E.rgb255 215 215 215) ] (viewInstruction True model.instructionMode instruction)
                    , others =
                        \instruction ->
                            E.el [] (viewInstruction False model.instructionMode instruction)
                    }
            )
        ]


viewKeyword : String -> Element Msg
viewKeyword name =
    E.el [ Font.heavy ] (E.text name)


viewInstruction : Bool -> InstructionMode -> Instruction -> Element Msg
viewInstruction isInstructionSelected instructionMode instruction =
    let
        viewBareInstruction : NodeMode -> Element Msg
        viewBareInstruction nodeMode =
            case instruction of
                Halt ->
                    viewKeyword "Halt"

                FutureInstruction _ ->
                    viewKeyword "---should not be ever displayed---"

                Instruction kind nodes ->
                    case kind of
                        LabelKind ->
                            E.row []
                                [ viewKeyword "label ", viewNode True isInstructionSelected nodeMode (ZipList.current nodes), viewKeyword ":" ]

                        OperationApplicationKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isSourceSelected, source ) :: ( isOperationNameSelected, operationName ) :: arguments ->
                                        List.concat
                                            [ [ viewNode isSourceSelected isInstructionSelected nodeMode source
                                              , viewKeyword " <- "
                                              , viewNode isOperationNameSelected isInstructionSelected nodeMode operationName
                                              , viewKeyword "("
                                              ]
                                            , arguments
                                                |> List.map (\( isArgSelected, arg ) -> viewNode isArgSelected isInstructionSelected nodeMode arg)
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
                                        [ viewNode isSourceSelected isInstructionSelected nodeMode source
                                        , viewKeyword " <- "
                                        , viewNode isTargetSelected isInstructionSelected nodeMode target
                                        ]

                                    _ ->
                                        [ E.text "error when viewing AssignmentKind" ]
                                )

                        JumpKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isArgSelected, arg ) :: [] ->
                                        [ viewKeyword "jump "
                                        , viewNode isArgSelected isInstructionSelected nodeMode arg
                                        ]

                                    _ ->
                                        [ E.text "error when viewing JumpKind" ]
                                )

                        JumpIfKind ->
                            E.row []
                                (case ZipList.mapToTaggedList nodes of
                                    ( isTestSelected, test ) :: ( isArgSelected, arg ) :: [] ->
                                        [ viewKeyword "if "
                                        , viewNode isTestSelected isInstructionSelected nodeMode test
                                        , viewKeyword " jump "
                                        , viewNode isArgSelected isInstructionSelected nodeMode arg
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
                                        , viewNode isArgSelected isInstructionSelected nodeMode arg
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


viewNode : Bool -> Bool -> NodeMode -> Node -> Element Msg
viewNode isSelected isInstructionSelected nodeMode (Node nodeKind str) =
    let
        viewStr str0 =
            if str == "" then
                viewHole

            else
                E.text str
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


type alias KeyCode =
    String


traverseModeKeyBindings : Dict KeyCode Msg
traverseModeKeyBindings =
    Dict.fromList
        [ ( "k", InstructionMovement Up )
        , ( "j", InstructionMovement Down )
        , ( ",", DeleteInstruction )
        , ( "i", InstructionEdit )
        , ( "h", InstructionInsertion Down )
        , ( "l", InstructionInsertion Up )
        , ( "s", NodeMovement Left )
        , ( "d", NodeMovement Right )
        , ( "e", SetModeTo (TraversingInstructions EditingNode) )
        , ( "a", NodeInsertion Left )
        , ( "f", NodeInsertion Right )
        , ( "x", DeleteNode )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    BE.onKeyUp
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\keyCode ->
                    -- let
                    --     wat =
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
                )
        )
