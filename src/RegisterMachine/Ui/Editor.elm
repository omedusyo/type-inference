module RegisterMachine.Ui.Editor exposing (..)

import Browser.Dom as Dom
import Browser.Events as BE
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HtmlAttributes
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Lib.ZipList as ZipList exposing (ZipList)
import Task
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)


type WithHole a
    = Hole
    | Element a



-- type InputElement =
--     Closed
--   |
-- type LabelElement
--     = Closed
--     |


type
    InstructionKind
    -- contains only instruction kinds for instructions that have atleast one node
    = LabelKind
    | OperationApplicationKind
    | AssignmentKind
    | JumpKind
    | JumpIfKind
    | HaltKind
    | PushKind


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


type EditingMode
    = Moving
    | Editing


type Instruction
    = Instruction InstructionKind (ZipList Node)
    | Halt


type alias Model =
    { instructions : ZipList Instruction
    , editingMode : EditingMode
    }


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
        , editingMode = Moving
        }


type HorizontalDirection
    = Left
    | Right


type VerticalDirection
    = Up
    | Down


type Msg
    = NoOp
    | LineMovement VerticalDirection
    | NodeMovement HorizontalDirection
    | DeleteLine
    | SetMovingMode
    | SetEditingMode
    | NodeEdit String
    | NodeInsertion HorizontalDirection
    | DeleteNode


moveLine : VerticalDirection -> Model -> Model
moveLine direction model =
    { model
        | instructions =
            case direction of
                Up ->
                    model.instructions |> ZipList.left

                Down ->
                    model.instructions |> ZipList.right
    }


moveNode : HorizontalDirection -> Model -> Model
moveNode direction model =
    { model
        | instructions =
            case direction of
                Left ->
                    model.instructions
                        |> ZipList.updateCurrent
                            (\instruction ->
                                case instruction of
                                    Instruction kind nodes ->
                                        Instruction kind (nodes |> ZipList.left)

                                    Halt ->
                                        instruction
                            )

                Right ->
                    model.instructions
                        |> ZipList.updateCurrent
                            (\instruction ->
                                case instruction of
                                    Instruction kind nodes ->
                                        Instruction kind (nodes |> ZipList.right)

                                    Halt ->
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

                Halt ->
                    instruction
        )


getCurrentNodes : Model -> Maybe (ZipList Node)
getCurrentNodes model =
    case ZipList.current model.instructions of
        Instruction _ nodes ->
            Just nodes

        Halt ->
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

                Halt ->
                    instruction
        )


insertAndEditNode : HorizontalDirection -> Context rootMsg Msg Model
insertAndEditNode direction =
    Context.updateWithCommand <|
        \model ->
            case getCurrentNodes model of
                Just nodes ->
                    let
                        maybeNewNodes =
                            case ZipList.current nodes of
                                Node Dynamic str ->
                                    case direction of
                                        Left ->
                                            Just
                                                (nodes
                                                    |> ZipList.insertLeft (Node Dynamic "")
                                                    |> ZipList.left
                                                )

                                        Right ->
                                            Just
                                                (nodes
                                                    |> ZipList.insertRight (Node Dynamic "")
                                                    |> ZipList.right
                                                )

                                _ ->
                                    Nothing
                    in
                    case maybeNewNodes of
                        Just newNodes ->
                            ( model |> setCurrentNodes newNodes |> setEditingMode, focusSearchBox )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


isCurrentNodeStatic : ZipList Node -> Bool
isCurrentNodeStatic nodes =
    case ZipList.current nodes of
        Node Static _ ->
            True

        _ ->
            False


deleteCurrentLine : Model -> Model
deleteCurrentLine model =
    { model | instructions = ZipList.delete model.instructions }


setEditingMode : Model -> Model
setEditingMode model =
    case ZipList.current model.instructions of
        Instruction _ _ ->
            { model | editingMode = Editing }

        Halt ->
            model


update : Msg -> Context rootMsg Msg Model
update msg =
    case msg of
        NoOp ->
            Context.none

        LineMovement direction ->
            Context.update (moveLine direction)

        NodeMovement direction ->
            Context.update (moveNode direction)

        DeleteLine ->
            Context.update (\model -> { model | instructions = ZipList.delete model.instructions })

        SetMovingMode ->
            Context.update (\model -> { model | editingMode = Moving })

        SetEditingMode ->
            Context.update setEditingMode
                |> Context.performCmd focusSearchBox

        NodeEdit str ->
            Context.update (updateCurrentNode (\(Node nodeKind _) -> Node nodeKind str))

        NodeInsertion direction ->
            insertAndEditNode direction

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
                            ZipList.delete nodes
                    )
                )


focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus editorId)


editorId : String
editorId =
    "my-special-editor"


view : Model -> Element Msg
view ({ instructions } as model) =
    E.column []
        [ case model.editingMode of
            Moving ->
                E.el [] (E.text "Moving")

            Editing ->
                E.el [] (E.text "Editing")
        , E.column []
            (instructions
                |> ZipList.mapToList
                    { current =
                        \instruction ->
                            E.el [ Background.color (E.rgb255 215 215 215) ] (viewInstruction True model.editingMode instruction)
                    , others =
                        \instruction ->
                            E.el [] (viewInstruction False model.editingMode instruction)
                    }
            )
        ]


viewInstruction : Bool -> EditingMode -> Instruction -> Element Msg
viewInstruction isInstructionSelected editingMode instruction =
    case instruction of
        Halt ->
            E.text "Halt"

        Instruction kind nodes ->
            case kind of
                LabelKind ->
                    E.row []
                        [ E.text "label ", viewNode True isInstructionSelected editingMode (ZipList.current nodes), E.text ":" ]

                OperationApplicationKind ->
                    E.row []
                        (case ZipList.mapToTaggedList nodes of
                            ( isSourceSelected, source ) :: ( isOperationNameSelected, operationName ) :: arguments ->
                                List.concat
                                    [ [ viewNode isSourceSelected isInstructionSelected editingMode source
                                      , E.text " <- "
                                      , viewNode isOperationNameSelected isInstructionSelected editingMode operationName
                                      , E.text "("
                                      ]
                                    , arguments
                                        |> List.map (\( isArgSelected, arg ) -> viewNode isArgSelected isInstructionSelected editingMode arg)
                                        |> List.intersperse (E.text ", ")
                                    , [ E.text ")" ]
                                    ]

                            _ ->
                                [ E.text "error when viewing OperationApplicationKind" ]
                        )

                AssignmentKind ->
                    Debug.todo ""

                JumpKind ->
                    Debug.todo ""

                JumpIfKind ->
                    Debug.todo ""

                HaltKind ->
                    Debug.todo ""

                PushKind ->
                    Debug.todo ""


viewNode : Bool -> Bool -> EditingMode -> Node -> Element Msg
viewNode isSelected isInstructionSelected editingMode (Node nodeKind str) =
    let
        viewStr str0 =
            if str == "" then
                viewHole

            else
                E.text str
    in
    if isSelected && isInstructionSelected then
        case editingMode of
            Moving ->
                E.el [ Border.width 1, Border.solid ] (viewStr str)

            Editing ->
                nodeInput str

    else
        E.el [] (viewStr str)


nodeInput : String -> Element Msg
nodeInput str =
    E.el []
        (E.html
            (Html.input
                [ HtmlAttributes.id editorId
                , HtmlEvents.onInput NodeEdit
                , HtmlAttributes.value str
                ]
                []
            )
        )


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
                    case model.editingMode of
                        Moving ->
                            case keyCode of
                                "k" ->
                                    Decode.succeed (LineMovement Up)

                                "j" ->
                                    Decode.succeed (LineMovement Down)

                                "s" ->
                                    Decode.succeed (NodeMovement Left)

                                "d" ->
                                    Decode.succeed (NodeMovement Right)

                                "," ->
                                    Decode.succeed DeleteLine

                                "e" ->
                                    Decode.succeed SetEditingMode

                                "a" ->
                                    Decode.succeed (NodeInsertion Left)

                                "f" ->
                                    Decode.succeed (NodeInsertion Right)

                                "x" ->
                                    Decode.succeed DeleteNode

                                _ ->
                                    Decode.fail ""

                        Editing ->
                            case keyCode of
                                "Escape" ->
                                    Decode.succeed SetMovingMode

                                _ ->
                                    Decode.fail ""
                )
        )
