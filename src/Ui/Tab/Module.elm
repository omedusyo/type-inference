module Ui.Tab.Module exposing (Model, Msg, init, update, view)

import Calculus.Base as L exposing (ModuleTerm, Term, Type)
import Calculus.Evaluation.Evaluation as L exposing (ThunkContext)
import Calculus.Evaluation.Value as Value exposing (Value)
import Calculus.Parser as LambdaParser
import Calculus.Show as L
import Element as E exposing (Element)
import Element.Input as Input
import Lib.Parser.Error as PError
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Config exposing (Config)
import Ui.Control.Effect as Effect exposing (Effect)
import Ui.Style.Button as Button


parseModule : Model -> Model
parseModule model =
    { model
        | parsedModule = Just (LambdaParser.runModuleTerm model.moduleInput)
    }


evalModule : Model -> Model
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


openModule : Model -> Model
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


type alias Model =
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


init : Effect rootMsg Msg Model
init =
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
    Effect.pure
        ({ moduleInput = input
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
        )


type Msg
    = ModuleInputChanged String
    | ModuleRunButtonClicked
    | ReplInputChanged String
    | ReplRunButtonClicked


update : Msg -> Action rootMsg Msg Model
update msg =
    case msg of
        ModuleInputChanged input ->
            Context.from
                (\model ->
                    { model
                        | moduleInput = input
                        , parsedModule = Just (LambdaParser.runModuleTerm input)
                        , evaledTerm = Nothing
                        , env = Value.emptyEnvironment
                    }
                )

        ModuleRunButtonClicked ->
            Context.from
                (\model ->
                    { model
                        | evaledTerm = Nothing
                        , env = Value.emptyEnvironment
                    }
                        |> evalModule
                        |> openModule
                )

        ReplInputChanged input ->
            Context.from
                (\model ->
                    let
                        parsedTerm =
                            Just (LambdaParser.runTerm input)
                    in
                    { model
                        | replInput = input
                        , parsedTerm = parsedTerm
                        , evaledTerm = Nothing
                    }
                )

        ReplRunButtonClicked ->
            Context.from
                (\model ->
                    case model.parsedTerm of
                        Just (Ok term) ->
                            { model | evaledTerm = Just (L.eval1 model.env term) }

                        _ ->
                            model
                )


view : Model -> Element Msg
view model =
    E.column [ E.width E.fill ]
        [ E.column [ E.width E.fill ]
            [ Input.button Button.buttonStyle
                { onPress =
                    Just ModuleRunButtonClicked
                , label = E.text "Run"
                }
            , Input.multiline
                [ E.height (E.px 500)
                , E.width E.fill
                ]
                { onChange = ModuleInputChanged
                , text = model.moduleInput
                , placeholder = Nothing
                , label = Input.labelHidden "what is this?"
                , spellcheck = False
                }
            ]
        , E.el [ E.width E.fill ]
            (case model.parsedModule of
                Just parsingModuleResult ->
                    case parsingModuleResult of
                        Ok module0 ->
                            -- TODO: shows the parsed output of the module
                            -- E.text (L.showModuleTerm module0)
                            E.column [ E.width E.fill ]
                                [ Input.button Button.buttonStyle
                                    { onPress =
                                        Just ReplRunButtonClicked
                                    , label = E.text "Run"
                                    }
                                , Input.multiline
                                    [ E.height (E.px 45)
                                    , E.width E.fill
                                    ]
                                    { onChange = ReplInputChanged
                                    , text = model.replInput
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "what is this?"
                                    , spellcheck = False
                                    }
                                , case model.parsedTerm of
                                    Just parsingResult ->
                                        case parsingResult of
                                            Ok term ->
                                                case model.evaledTerm of
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
