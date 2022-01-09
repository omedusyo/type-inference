module Calculus.Ui.Help exposing (Model, Msg, init, update, view)

import Calculus.Ui.Control.Context as Context exposing (Config, Context)
import Calculus.Ui.Control.InitContext as InitContext exposing (InitContext)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Model =
    {}


init : InitContext Model Msg
init =
    InitContext.setModelTo
        {}


type Msg
    = Wat


update : Msg -> Context Model msg
update msg =
    case msg of
        Wat ->
            Context.none


view : Config -> Model -> Element Msg
view config model =
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
