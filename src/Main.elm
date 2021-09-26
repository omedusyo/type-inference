module Main exposing (..)

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Evaluation as L exposing (ThunkContext)
import Html as H exposing (Html)
import Inference as L
import LambdaBasics as L exposing (Term, Type)
import List.Extra as List
import Return exposing (Return)
import Show as L
import StatefulWithErr as State
import TermParser as L
import TypeVarContext as L
import Value as L exposing (Value)


blue =
    E.rgb255 142 207 245


insertAfter : Int -> a -> List a -> List a
insertAfter n a xs0 =
    if n == -1 then
        a :: xs0

    else
        case xs0 of
            [] ->
                []

            x :: xs1 ->
                x :: insertAfter (n - 1) a xs1



-- insertAfter 1 a [x, y, z]
-- x :: insertAfter 0 a [y, z]
-- x :: a :: y :: z :: []
-- ===MODEL===


type alias Model =
    { bindings : List Binding
    , moduleModel : ModuleModel
    , tab : Tab
    }


type alias ModuleModel =
    { moduleInput : String
    , -- Nothing means haven't parsed anything yet
      parsedModule : Maybe (Result L.TermParsingError L.ModuleTerm)

    -- TODO: repl stuff
    }


initModuleModel : ModuleModel
initModuleModel =
    { moduleInput = "(module \n\n)"
    , parsedModule = Nothing
    }


type Tab
    = ProgramBindings
    | Module
    | Help


tabs : List Tab
tabs =
    [ ProgramBindings, Module, Help ]


initTab : Tab
initTab =
    Module


tabToString : Tab -> String
tabToString tab =
    case tab of
        ProgramBindings ->
            "Program"

        Module ->
            "Module"

        Help ->
            "Help"


initModel : Model
initModel =
    { bindings = [ exampleBinding ]
    , moduleModel = initModuleModel
    , tab = initTab
    }


type alias BindingName =
    String


type alias Binding =
    { name : BindingName
    , input : String
    , -- Nothing means haven't parsed anything yet
      parsedTerm : Maybe (Result L.TermParsingError Term)
    , -- Nothing means haven't evaled the term yet
      evaledTerm : Maybe (Result (List L.EvalError) ( ThunkContext, Value ))
    , inferedType : Maybe (Result (List L.TypeError) ( L.TermVarContext, L.TypeVarContext, Type ))
    }


initBinding : Binding
initBinding =
    { name = ""
    , input = ""
    , parsedTerm = Nothing
    , evaledTerm = Nothing
    , inferedType = Nothing
    }


exampleBinding : Binding
exampleBinding =
    let
        input0 =
            """(let 
    (fn { p .
        (match-pair $p
            { (pair x y) . (pair $y $x) }) })
    { flip .
       (pair
           (@ $flip (pair 0n0 0n1) )
           (@ $flip (pair true false) ) )
    })"""

        input1 =
            "(pair (if true { $a } { $b }) (pair (if true{0n0}{$a}) (if true{0n1}{$b}))  )"

        input2 =
            "(pair (if true { $a } { $b }) (pair  (if true{0n1}{$b}) (if true{0n0}{$a}))  )"

        input3 =
            "(pair  (pair (if true{0n0}{$a}) (if true{0n1}{$b})) (if true { $a } { $b }) )"

        input4 =
            "(fn { f p . (match-pair $p { (pair x y) . (@ $f $x $y) }) })"

        input5 =
            "(let (fn { x . $x }) { f . $f })"

        input6 =
            """(let
        (fn {f x . (@ $f (@ $f $x) ) })
        { twice .
           (let
              (fn { x . (succ $x) })
              { plus-one .
                 (let (fn { b .  (if $b { false } { true }) })
                   { not .
                      (pair $twice $plus-one )
                   })
              })
        })
        """

        input7 =
            "(@ (fn {. 0n0}) )"

        input =
            input0

        termResult =
            L.parseTerm input
    in
    { name = "foo"
    , input = input
    , parsedTerm = Just termResult
    , evaledTerm =
        case termResult of
            Ok term ->
                Just (L.eval0 term)

            _ ->
                Nothing
    , inferedType =
        case termResult of
            Ok term ->
                Just (L.infer0 term)

            _ ->
                Nothing
    }



-- ===MSG===


type alias BindingPosition =
    Int


type Msg
    = ChangeTab Tab
    | BindingMsg BindingPosition BindingMsg
    | ModuleMsg ModuleMsg
    | InsertBindingAtTheStart
    | InsertBindingAfter BindingPosition


type BindingMsg
    = InputChanged String
    | InferButtonClicked
    | RunButtonClicked


type ModuleMsg
    = ModuleInputChanged String


isParsedSuccesfully : Binding -> Bool
isParsedSuccesfully model =
    case model.parsedTerm of
        Just (Ok term) ->
            True

        _ ->
            False


isModuleParsedSuccesfully : ModuleModel -> Bool
isModuleParsedSuccesfully model =
    case model.parsedModule of
        Just (Ok term) ->
            True

        _ ->
            False


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ChangeTab tab ->
            { model | tab = tab }
                |> Return.singleton

        BindingMsg n bindingMsg ->
            case List.getAt n model.bindings of
                Just binding ->
                    updateBinding bindingMsg binding
                        |> Return.map
                            (\newBinding ->
                                { model
                                    | bindings = List.setAt n newBinding model.bindings
                                }
                            )
                        |> Return.mapCmd (BindingMsg n)

                Nothing ->
                    model
                        |> Return.singleton

        ModuleMsg moduleMsg ->
            updateModuleModel moduleMsg model.moduleModel
                |> Return.map
                    (\moduleModel ->
                        { model | moduleModel = moduleModel }
                    )
                |> Return.mapCmd ModuleMsg

        InsertBindingAtTheStart ->
            { model | bindings = initBinding :: model.bindings }
                |> Return.singleton

        InsertBindingAfter n ->
            { model | bindings = insertAfter n initBinding model.bindings }
                |> Return.singleton


updateBinding : BindingMsg -> Binding -> Return BindingMsg Binding
updateBinding msg model =
    case msg of
        InputChanged input ->
            { model
                | input = input
                , parsedTerm = Just (L.parseTerm input)
                , evaledTerm = Nothing
                , inferedType = Nothing
            }
                |> Return.singleton

        RunButtonClicked ->
            case model.parsedTerm of
                Just (Ok term) ->
                    { model | evaledTerm = Just (L.eval0 term) }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        InferButtonClicked ->
            case model.parsedTerm of
                Just (Ok term) ->
                    { model | inferedType = Just (L.infer0 term) }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton


updateModuleModel : ModuleMsg -> ModuleModel -> Return ModuleMsg ModuleModel
updateModuleModel msg model =
    case msg of
        ModuleInputChanged input ->
            { model
                | moduleInput = input
                , parsedModule = Just (L.parseModuleTerm input)
            }
                |> Return.singleton



-- ===VIEW===


buttonStyle =
    [ Background.color blue
    , E.paddingXY 9 4
    , Border.rounded 2
    ]


viewBinding : Binding -> Element BindingMsg
viewBinding binding =
    let
        heightPx =
            300
    in
    E.column [ E.width E.fill ]
        [ E.row []
            [ Input.button buttonStyle
                { onPress =
                    if isParsedSuccesfully binding then
                        Just RunButtonClicked

                    else
                        Nothing
                , label = E.text "Run"
                }
            , Input.button buttonStyle
                { onPress =
                    if isParsedSuccesfully binding then
                        Just InferButtonClicked

                    else
                        Nothing
                , label = E.text "Infer"
                }
            ]
        , E.el [] (E.text binding.name)
        , E.row [ E.width E.fill, E.paddingEach { top = 5, right = 0, bottom = 0, left = 0 } ]
            [ Input.multiline
                [ E.height (E.px heightPx)
                , E.width E.fill
                ]
                { onChange = InputChanged
                , text = binding.input
                , placeholder = Nothing
                , label = Input.labelHidden "what is this?"
                , spellcheck = False
                }
            , E.el
                [ E.height (E.px heightPx)
                , E.width E.fill
                , E.paddingEach { top = 0, right = 0, bottom = 0, left = 10 }
                ]
                (E.column
                    []
                    [ E.text "TERMS & VALUES"
                    , E.text
                        (String.concat
                            [ "term = "
                            , case binding.parsedTerm of
                                Nothing ->
                                    ""

                                Just result ->
                                    case result of
                                        Ok term ->
                                            L.showTerm term

                                        Err err ->
                                            "Parsing Error"
                            ]
                        )
                    , E.el []
                        (case binding.evaledTerm of
                            Nothing ->
                                E.text ""

                            Just result ->
                                case result of
                                    Ok ( thunkContext, val ) ->
                                        E.column []
                                            [ E.text
                                                (String.concat
                                                    [ "next-thunk-id = "
                                                    , String.fromInt thunkContext.nextThunkId
                                                    ]
                                                )
                                            , E.text
                                                (String.concat
                                                    [ "thunk-context = "
                                                    , L.showThunks thunkContext
                                                    ]
                                                )
                                            , E.text
                                                (String.concat
                                                    [ "value = "
                                                    , L.showValue val
                                                    ]
                                                )
                                            ]

                                    Err err ->
                                        E.text "Evaluation Error"
                        )
                    , E.text "TYPE INFERENCE"
                    , E.column []
                        (case binding.inferedType of
                            Nothing ->
                                []

                            Just result ->
                                case result of
                                    Ok ( termVarContext, { nextTypeVar, typeVarStack, equations } as typeVarContext, type0 ) ->
                                        [ E.text
                                            (String.concat
                                                [ "term-var-context = "
                                                , L.showTermVarContext termVarContext
                                                ]
                                            )
                                        , E.text
                                            (String.concat
                                                [ "next-type-var = "
                                                , "'" ++ String.fromInt nextTypeVar
                                                ]
                                            )
                                        , E.text
                                            (String.concat
                                                [ "stack = "
                                                , L.showTypeVarStack typeVarStack
                                                ]
                                            )
                                        , E.text
                                            (String.concat
                                                [ "equations = "
                                                , L.showEquations equations
                                                ]
                                            )
                                        , E.text
                                            (String.concat
                                                [ "type = "
                                                , L.showType type0
                                                ]
                                            )
                                        , E.text
                                            (String.concat
                                                [ "expanded-type = "
                                                , case State.run (L.expandType type0) typeVarContext of
                                                    Ok ( _, type1 ) ->
                                                        L.showType type1

                                                    Err typeErrors ->
                                                        typeErrors
                                                            |> List.map L.showTypeError
                                                            |> String.join ", "
                                                ]
                                            )
                                        ]

                                    Err typeErrors ->
                                        [ E.text
                                            (String.concat
                                                [ "type-error = "
                                                , typeErrors
                                                    |> List.map L.showTypeError
                                                    |> String.join ", "
                                                ]
                                            )
                                        ]
                        )
                    ]
                )
            ]
        ]


view : Model -> Element Msg
view model =
    E.column [ E.width E.fill, E.padding 10 ]
        [ E.row []
            (tabs
                |> List.map
                    (\tab ->
                        Input.button buttonStyle
                            { onPress = Just (ChangeTab tab)
                            , label = E.text (tabToString tab)
                            }
                    )
            )
        , case model.tab of
            ProgramBindings ->
                E.column [ E.width E.fill ]
                    [ Input.button buttonStyle
                        { onPress = Just InsertBindingAtTheStart
                        , label = E.text "+"
                        }
                    , E.column [ E.width E.fill ]
                        (model.bindings
                            |> List.indexedMap
                                (\i binding ->
                                    E.column [ E.width E.fill ]
                                        [ viewBinding binding |> E.map (BindingMsg i)
                                        , Input.button buttonStyle
                                            { onPress = Just (InsertBindingAfter i)
                                            , label = E.text "+"
                                            }
                                        ]
                                )
                        )
                    ]

            Module ->
                viewModule model.moduleModel
                    |> E.map ModuleMsg

            Help ->
                viewHelp
        ]


viewModule : ModuleModel -> Element ModuleMsg
viewModule moduleModel =
    E.column [ E.width E.fill ]
        [ E.row [ E.width E.fill ]
            [ Input.multiline
                [ E.height (E.px 350)
                , E.width E.fill
                ]
                { onChange = ModuleInputChanged
                , text = moduleModel.moduleInput
                , placeholder = Nothing
                , label = Input.labelHidden "what is this?"
                , spellcheck = False
                }
            ]
        , E.el [ E.width E.fill ]
            (case moduleModel.parsedModule of
                Just result ->
                    case result of
                        Ok module0 ->
                            E.text (L.showModuleTerm module0)

                        Err _ ->
                            E.text "parsing error"

                Nothing ->
                    E.text ""
            )
        ]


viewHelp : Element Msg
viewHelp =
    E.column [ E.width E.fill ]
        [ E.text "example: `(fn { p . (match-pair $p { (pair x y) . (pair $y $x) }) })`"
        , E.text "which in more standard lambda notation would be something like: `\\p. case p of (x, y) -> (y, x)`"
        , E.text "SYNTAX"
        , E.text "Constants"
        , E.text "  true, false"
        , E.text "  0n0, 0n1, 0n2, 0n3, ..."
        , E.text "  empty-list"
        , E.text ""
        , E.text "Variable Use"
        , E.text "  $foo // when using a variable, you have to prepend a dollar sign to it"
        , E.text ""
        , E.text "Simple Operators"
        , E.text "  (pair e e')"
        , E.text "  (@ f x) // this is function application"
        , E.text "  (@ f x y)"
        , E.text "  (left e)"
        , E.text "  (right e)"
        , E.text "  (succ n)      // this is the natural numbers successor function"
        , E.text "  (cons x xs) // this is consing of an element to a list"
        , E.text ""
        , E.text "Bindings Operators"
        , E.text "  (fn { x . body })"
        , E.text "  (fn { x y . body })"
        , E.text "  (match-pair pairExp { (pair x y) . body })"
        , E.text "  (if e { e1 } { e2 })"
        , E.text "  (sum-case e { (left x) . e1 } { (right y) . e2 })"
        , E.text "  (nat-loop   n initState { i s . body })"
        , E.text "  (list-loop xs initState { x s . body })"
        , E.text "  (let exp { x . body }) // standard syntax `let x = exp in body`"
        , E.text "  (fn {. body }) // freezes computation"
        , E.text "  (@ thunk) // forces computation"
        ]



-- ===MAIN===


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> Return.singleton initModel
        , update = update
        , subscriptions = subscriptions
        , view = \model -> E.layout [] (view model)
        }



-- ===SUBSCRIPTIONS===


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
