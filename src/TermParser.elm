module TermParser exposing (..)

import Main exposing (..)
import Parser exposing ((|.), (|=), Parser)


true : Parser Term
true =
    Parser.succeed BoolTrue
        |. Parser.keyword "True"


false : Parser Term
false =
    Parser.succeed BoolFalse
        |. Parser.keyword "False"


bool : Parser Term
bool =
    Parser.oneOf [ true, false ]
