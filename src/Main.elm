module Main exposing (..)

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Evaluation as L exposing (Value)
import Html as H exposing (Html)
import Inference as L
import LambdaBasics as L exposing (Term, Type)
import Return exposing (Return)
import Show as L
import TermParser as L
import TypeVarContext as L


blue =
    E.rgb255 142 207 245



-- ===MODEL===


type alias Model =
    { input : String
    , -- Nothing means haven't parsed anything yet
      parsedTerm : Maybe (Result L.TermParsingError Term)
    , -- Nothing means haven't evaled the term yet
      evaledTerm : Maybe (Result (List L.EvalError) Value)
    , inferedType : Maybe (Result (List L.TypeError) ( L.TermVarContext, L.TypeVarContext, Type ))
    }


initModel : Model
initModel =
    let
        input =
            "(fn { p . (match-pair $p { (pair x y) . $p }) })"

        termResult =
            L.parseTerm input
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



-- ===MSG===


type Msg
    = InputChanged String
    | InferButtonClicked
    | RunButtonClicked


isParsedSuccesfully : Model -> Bool
isParsedSuccesfully model =
    case model.parsedTerm of
        Just (Ok term) ->
            True

        _ ->
            False


update : Msg -> Model -> Return Msg Model
update msg model =
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
                    { model | evaledTerm = Just (L.eval L.emptyTermEnvironment term) }
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



-- ===VIEW===


view : Model -> Element Msg
view model =
    let
        heightPx =
            450

        buttonStyle =
            [ Background.color blue
            , E.paddingXY 9 4
            , Border.rounded 2
            ]
    in
    E.column [ E.width E.fill, E.padding 10 ]
        [ E.text "example: `(fn { p . (match-pair $p { (pair x y) . $p }) })`"
        , E.text "which in more standard lambda notation would be something like: `\\p. case p of (x, y) -> (y, x)`"
        , E.row []
            [ Input.button buttonStyle
                { onPress =
                    if isParsedSuccesfully model then
                        Just RunButtonClicked

                    else
                        Nothing
                , label = E.text "Run"
                }
            , Input.button buttonStyle
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
                    [ E.text
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
                                            "Parsing Error"
                            ]
                        )
                    , E.text
                        (String.concat
                            [ "value = "
                            , case model.evaledTerm of
                                Nothing ->
                                    ""

                                Just result ->
                                    case result of
                                        Ok val ->
                                            L.showValue val

                                        Err err ->
                                            "Evaluation Error"
                            ]
                        )
                    , E.text
                        (String.concat
                            [ "type = "
                            , case model.inferedType of
                                Nothing ->
                                    ""

                                Just result ->
                                    case result of
                                        Ok ( context, equations, type0 ) ->
                                            -- TODO: remove the dependence on expandType
                                            -- TODO: you need to loose the dependence on `expandType`
                                            -- case L.expandType type0 equations of
                                            --     Ok type1 ->
                                            --         L.showType type1
                                            --     Err err ->
                                            --         "Type Error"
                                            L.showType type0

                                        Err err ->
                                            "Type Error"
                            ]
                        )
                    ]
                )
            ]
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
