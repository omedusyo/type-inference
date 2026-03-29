module Ui.Tab.Program exposing (Model, Msg, init, update, view)

import Calculus.Base as L exposing (ModuleTerm, Term, Type)
import Calculus.Evaluation.Evaluation as L exposing (ThunkContext)
import Calculus.Evaluation.Value as Value exposing (Value)
import Calculus.Parser as LambdaParser
import Calculus.Show as L
import Calculus.Type.Inference as L
import Calculus.Type.TypeVarContext as L
import Element as E exposing (Element)
import Element.Input as Input
import Lib.Parser.Error as PError
import Lib.State.StatefulWithErr as State
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.Style.Button as Button


type alias Binding =
    { input : String
    , -- Nothing means haven't parsed anything yet
      parsedTerm : Maybe (Result (PError.Error LambdaParser.ExpectedTerm) Term)
    , -- Nothing means haven't evaled the term yet
      evaledTerm : Maybe (Result (List L.EvalError) ( ThunkContext, Value ))
    , inferedType : Maybe (Result (List L.TypeError) ( L.TermVarContext, L.TypeVarContext, Type ))
    }


initBinding : Binding
initBinding =
    { input = ""
    , parsedTerm = Nothing
    , evaledTerm = Nothing
    , inferedType = Nothing
    }


isParsedSuccesfully : Binding -> Bool
isParsedSuccesfully model =
    case model.parsedTerm of
        Just (Ok term) ->
            True

        _ ->
            False


type alias Model =
    Binding


init : Effect rootMsg Msg Model
init =
    Effect.pure
        exampleBinding


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
    { input = input
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


type Msg
    = InputChanged String
    | InferButtonClicked
    | RunButtonClicked


update : Msg -> Action rootMsg Msg Model
update msg =
    case msg of
        InputChanged input ->
            Context.from
                (\model ->
                    { model
                        | input = input
                        , parsedTerm = Just (LambdaParser.runTerm input)
                        , evaledTerm = Nothing
                        , inferedType = Nothing
                    }
                )

        RunButtonClicked ->
            Context.from
                (\model ->
                    case model.parsedTerm of
                        Just (Ok term) ->
                            { model | evaledTerm = Just (L.eval0 term) }

                        _ ->
                            model
                )

        InferButtonClicked ->
            Context.from
                (\model ->
                    case model.parsedTerm of
                        Just (Ok term) ->
                            { model | inferedType = Just (L.infer0 term) }

                        _ ->
                            model
                )


view : Model -> Element Msg
view model =
    let
        heightPx =
            300
    in
    E.column [ E.width E.fill ]
        [ E.row []
            [ Input.button Button.buttonStyle
                { onPress =
                    if isParsedSuccesfully model then
                        Just RunButtonClicked

                    else
                        Nothing
                , label = E.text "Run"
                }
            , Input.button Button.buttonStyle
                { onPress =
                    if isParsedSuccesfully model then
                        Just InferButtonClicked

                    else
                        Nothing
                , label = E.text "Infer"
                }
            ]
        , E.row [ E.width E.fill, E.paddingEach { top = 5, right = 0, bottom = 0, left = 0 } ]
            [ Input.multiline
                [ E.height (E.px heightPx)
                , E.width E.fill
                ]
                { onChange = InputChanged
                , text = model.input
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
                            , case model.parsedTerm of
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
                        (case model.evaledTerm of
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
                        (case model.inferedType of
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
