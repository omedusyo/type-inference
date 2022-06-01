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
import RegisterMachine.Ui.Base as Base
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
import RegisterMachine.Ui.Color as Color
import RegisterMachine.Ui.Validation as Validation exposing (validatedInstruction)
import Task
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)
import Ui.InputCell as E



--===UI===


type InstructionMode
    = TraversingInstructions NodeMode
    | InsertingInstruction


type NodeMode
    = TraversingNodes
    | EditingNode


type alias Model =
    { instructions : ZipList Instruction
    , instructionMode : InstructionMode
    }



-- TODO: Delete the below test instructions/functions


src =
    emptyNode Static Base.registerExpectation


opName =
    emptyNode Static Base.operationNameExpectation


argDyn =
    emptyNode Dynamic Base.argExpectation


lbl =
    emptyNode Static Base.labelExpectation


instruction0 : Instruction
instruction0 =
    Instruction OperationApplicationKind (ZipList.fromList src [ opName, argDyn, argDyn, argDyn ]) initialInstructionValidity


instruction1 : Instruction
instruction1 =
    Instruction OperationApplicationKind (ZipList.fromList src [ opName, argDyn ]) initialInstructionValidity


instruction2 : Instruction
instruction2 =
    Instruction LabelKind (ZipList.fromList lbl []) initialInstructionValidity


init : InitContext Msg Model
init =
    InitContext.setModelTo
        { instructions = ZipList.fromList instruction0 [ instruction1, instruction2, Halt ]
        , instructionMode = TraversingInstructions TraversingNodes
        }


type Msg
    = SetModeTo InstructionMode
      -- Instructions
    | InstructionMovement VerticalDirection
    | SwapInstruction VerticalDirection
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
traverseNodes direction model =
    { model
        | instructions =
            model.instructions
                |> ZipList.updateCurrent
                    (\instruction ->
                        case instruction of
                            Instruction kind nodes validation ->
                                case direction of
                                    Left ->
                                        Instruction kind (nodes |> ZipList.left) validation

                                    Right ->
                                        Instruction kind (nodes |> ZipList.right) validation

                            _ ->
                                instruction
                    )
    }


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


update : Msg -> Context rootMsg Msg Model
update msg =
    case msg of
        InstructionMovement direction ->
            Context.update (moveInstruction direction)

        SwapInstruction direction ->
            Context.update (swapInstruction direction)

        InstructionEdit ->
            Context.update setModeToInsertInstruction

        InstructionInsertion direction ->
            Context.update (insertFutureInstruction direction >> moveInstruction direction >> setModeToInsertInstruction)

        ChangeInstructionTo instructionKind ->
            Context.update (changeInstructionWithoutValidationTo instructionKind >> validateCurrentInstruction)

        NodeMovement direction ->
            Context.update (traverseNodes direction)

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
            -- TODO: Node validation
            Context.update (updateCurrentNodeWithoutValidation (\(Node nodeKind nodeValidation nodeExpectation _) -> Node nodeKind nodeValidation nodeExpectation str))

        NodeInsertion direction ->
            Context.update (insertAndEditNodeWithoutValidation direction)

        DeleteNode ->
            Context.update deleteCurrentNodeWithValidation


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

                Instruction kind nodes validation ->
                    -- TODO: Display the validation information
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
        [ ( "k", InstructionMovement Up )
        , ( "j", InstructionMovement Down )
        , ( "K", SwapInstruction Up )
        , ( "J", SwapInstruction Down )
        , ( ",", DeleteInstruction )
        , ( "i", InstructionEdit )
        , ( "o", InstructionInsertion Down )
        , ( "O", InstructionInsertion Up )
        , ( "s", NodeMovement Left )
        , ( "d", NodeMovement Right )
        , ( "e", SetModeTo (TraversingInstructions EditingNode) )
        , ( "W", NodeInsertion Left )
        , ( "w", NodeInsertion Right )
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
