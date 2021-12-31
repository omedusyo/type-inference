module Main exposing (..)

import Browser
import Calculus.Base as L exposing (ModuleTerm, Term, Type)
import Calculus.Evaluation.Evaluation as L exposing (ThunkContext)
import Calculus.Evaluation.Value as Value exposing (Value)
import Calculus.Example
import Calculus.Parser as LambdaParser
import Calculus.Show as L
import Calculus.Type.Inference as L
import Calculus.Type.TypeVarContext as L
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as H exposing (Html)
import Lib.Parser.Error as PError
import Lib.Parser.Parser as NewParser
import Lib.State.StatefulWithErr as State
import List.Extra as List
import Return exposing (Return)


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
      parsedModule : Maybe (Result (PError.Error LambdaParser.ExpectedModuleTerm) ModuleTerm)
    , evaledModule : Maybe (Result (List L.EvalError) Value.ModuleValue)
    , env : Value.Environment
    , -- ===REPL===
      replInput : String
    , parsedTerm : Maybe (Result (PError.Error LambdaParser.ExpectedTerm) Term)
    , evaledTerm : Maybe (Result (List L.EvalError) ( ThunkContext, Value ))
    }


parseModule : ModuleModel -> ModuleModel
parseModule model =
    { model
        | parsedModule = Just (LambdaParser.runModuleTerm model.moduleInput)
    }


evalModule : ModuleModel -> ModuleModel
evalModule model =
    { model
        | evaledModule =
            case model.parsedModule of
                Just parsedModule ->
                    case parsedModule of
                        Ok moduleTerm ->
                            Just (L.evalModule0 moduleTerm)

                        Err _ ->
                            Nothing

                Nothing ->
                    Nothing
    }


openModule : ModuleModel -> ModuleModel
openModule model =
    { model
        | env =
            case model.parsedModule of
                Just parsedModule ->
                    case parsedModule of
                        Ok moduleTerm ->
                            let
                                evaledModuleResult =
                                    L.evalModule0 moduleTerm
                            in
                            case evaledModuleResult of
                                Ok moduleVal ->
                                    L.openModule moduleVal Value.emptyEnvironment

                                Err _ ->
                                    Value.emptyEnvironment

                        Err _ ->
                            Value.emptyEnvironment

                Nothing ->
                    Value.emptyEnvironment
    }


initModuleModel : ModuleModel
initModuleModel =
    let
        input1 =
            """module {

  let-module Nat = module {
    let-term plus = \\{ x y .
      fold-nat $x
        { zero . $y }
        { succ(state) . succ($state) }
    };

    let-term multiply = \\{ x y .
      fold-nat $x
        { zero . 00 }
        { succ(state) . [$plus $y $state] }
    };

    let-term exp = \\{ base exponent .
      fold-nat $exponent
        { zero . 01 }
        { succ(state) . [$multiply $base $state] }
    };

  };

  let-module List = module {
    let-term map = \\{ f xs .
      fold-list $xs
        { empty . empty }
        { cons(x state) . cons([$f $x] $state) }
    };

    let-term concat = \\{ xs ys .
      fold-list $xs
        { empty . $ys }
        { cons(x state) . cons($x $state) }
    };

    let-term singleton = \\{ x . cons($x empty) };

    let-term and-then = \\{ f xs .
      fold-list $xs
        { empty . empty }
        { cons(x state) . [$concat [$f $x] $state] }
    };
  };

  let-term square = \\{ x . [/($Nat multiply) $x $x] };
}
"""

        input =
            input1
    in
    { moduleInput = input
    , parsedModule = Nothing
    , evaledModule = Nothing
    , env = Value.emptyEnvironment
    , replInput = ""
    , parsedTerm = Nothing
    , evaledTerm = Nothing
    }
        |> parseModule
        |> evalModule
        |> openModule


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
      parsedTerm : Maybe (Result (PError.Error LambdaParser.ExpectedTerm) Term)
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
            """let flip = \\{ p .
    match-pair $p
        { pair(x y) . pair($y $x) }
};
pair(
    [$flip pair(00 01)],
    [$flip pair(true false)]
)
"""

        input1 =
            """pair(
    match-bool true { true . $a } { false . $b },
    pair(
        match-bool true { true . 00 } { false . $a },
        match-bool true { true . 01 } { false . $b },
    )
)"""

        input2 =
            """[ \\{x . succ($x)} 04 ]
"""

        input4 =
            -- { pair(x y) . [$f $x $y] }
            """\\{ f p .
    match-pair $p
        { pair(x y) . [$f $x $y] }
}
"""

        input5 =
            -- "(let (fn { x . $x }) { f . $f })"
            """let f = \\{ x . $x };
$f"""

        input6 =
            """let twice = \\{ f x .
    [$f [$f $x]]
};
let plus-one = \\{ x . succ($x) };
let not = \\{ b . match-bool $b { true . false } { false . true } };
pair($twice $plus-one)
"""

        input7 =
            "delay {. 00}"

        input =
            input0

        termResult =
            LambdaParser.runTerm input
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
    | ModuleRunButtonClicked
    | ReplInputChanged String
    | ReplRunButtonClicked


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
                , parsedTerm = Just (LambdaParser.runTerm input)
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
                , parsedModule = Just (LambdaParser.runModuleTerm input)
                , evaledTerm = Nothing
                , env = Value.emptyEnvironment
            }
                |> Return.singleton

        ModuleRunButtonClicked ->
            { model
                | evaledTerm = Nothing
                , env = Value.emptyEnvironment
            }
                |> evalModule
                |> openModule
                |> Return.singleton

        ReplInputChanged input ->
            let
                parsedTerm =
                    Just (LambdaParser.runTerm input)
            in
            { model
                | replInput = input
                , parsedTerm = parsedTerm
                , evaledTerm = Nothing
            }
                |> Return.singleton

        ReplRunButtonClicked ->
            case model.parsedTerm of
                Just (Ok term) ->
                    { model | evaledTerm = Just (L.eval1 model.env term) }
                        |> Return.singleton

                _ ->
                    model
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
                                            -- "Parsing Error"
                                            LambdaParser.termErrorToString err
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

                                    Err errors ->
                                        E.text (L.showEvaluationErrors errors)
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
        [ E.column [ E.width E.fill ]
            [ Input.button buttonStyle
                { onPress =
                    Just ModuleRunButtonClicked
                , label = E.text "Run"
                }
            , Input.multiline
                [ E.height (E.px 500)
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
                Just parsingModuleResult ->
                    case parsingModuleResult of
                        Ok module0 ->
                            -- TODO: shows the parsed output of the module
                            -- E.text (L.showModuleTerm module0)
                            E.column [ E.width E.fill ]
                                [ Input.button buttonStyle
                                    { onPress =
                                        Just ReplRunButtonClicked
                                    , label = E.text "Run"
                                    }
                                , Input.multiline
                                    [ E.height (E.px 45)
                                    , E.width E.fill
                                    ]
                                    { onChange = ReplInputChanged
                                    , text = moduleModel.replInput
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "what is this?"
                                    , spellcheck = False
                                    }
                                , case moduleModel.parsedTerm of
                                    Just parsingResult ->
                                        case parsingResult of
                                            Ok term ->
                                                case moduleModel.evaledTerm of
                                                    Just evaledResult ->
                                                        case evaledResult of
                                                            Ok ( thunkContext, val ) ->
                                                                E.column []
                                                                    [ E.text (L.showValue val)

                                                                    -- , E.text ("env := " ++ L.showEnvironment moduleModel.env)
                                                                    ]

                                                            Err errors ->
                                                                E.text (L.showEvaluationErrors errors)

                                                    Nothing ->
                                                        E.text ""

                                            Err err ->
                                                E.text (LambdaParser.termErrorToString err)

                                    Nothing ->
                                        E.text ""
                                ]

                        Err err ->
                            E.text (LambdaParser.moduleTermErrorToString err)

                Nothing ->
                    E.text ""
            )
        ]


viewHelp : Element Msg
viewHelp =
    E.column [ E.width E.fill ]
        [ E.text ""
        , E.text "example: `\\{ p . match-pair $p { pair(x y) . pair($y $x) } }`"
        , E.text "which in more standard lambda notation would be something like: `\\p. case p of (x, y) -> (y, x)`"
        , E.text ""
        , E.text "SYNTAX"
        , E.text ""
        , E.text "Constants"
        , E.text "  true, false"
        , E.text "  zero"
        , E.text "  00, 01, 02, 03, ... // nat literals have to start with 0 digit (`zero` is the same as `00`)"
        , E.text "  empty // empty list"
        , E.text ""
        , E.text ""
        , E.text "Variable Use"
        , E.text "  $foo // when using a variable, you have to prepend a dollar sign to it"
        , E.text ""
        , E.text ""
        , E.text "Simple Operators"
        , E.text "  pair($e1 $e2)"
        , E.text "  [$f $x] // function application"
        , E.text "  [$f $x $y]"
        , E.text "  left($e1)"
        , E.text "  right($e2)"
        , E.text "  succ($n)      // natural numbers successor function"
        , E.text "  cons($x $xs) // consing of an element to a list"
        , E.text ""
        , E.text ""
        , E.text "Bindings Operators"
        , E.text "  \\{ x . $body }"
        , E.text "  \\{ x y . $body }"
        , E.text ""
        , E.text "  match-pair $pair-exp { pair(x y) . $body }"
        , E.text "  match-bool $test-exp { true . $e1 } { false . $e2 } // if expression"
        , E.text "  match-sum $sum-exp { left(x) . $e1 } { right(y) . $e2 }"
        , E.text ""
        , E.text "  fold-nat $n { zero . $init-state } { succ(prev-state) . $next-state } // for-loop over natural numbers"
        , E.text "  fold-list $xs { empty . $init-state } { cons(x prev-state) . $next-state } // for-loop over lists"
        , E.text ""
        , E.text "  let-be $exp { x . $body }) // standard syntax `let x = exp in body`"
        , E.text "  let x = $exp; $body // syntactic sugar for `let-be`"
        , E.text ""
        , E.text "  delay {. $body } // freezes computation"
        , E.text "  force($thunk) // forces computation"
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
