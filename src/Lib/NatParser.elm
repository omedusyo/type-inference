module Lib.NatParser exposing (..)

import Parser exposing ((|.), (|=), DeadEnd, Parser)



-- Why can't we just use `Parser.int`?
-- Because it consumes spaces and dots in `123  .`. WTF?


nat : Parser Int
nat =
    -- WARNING: When parsing the string "000123", this will succesfully consume "0", and that's it.
    -- Either "0" or a non-zero digit followed by a sequence (possibly empty) of digits
    Parser.oneOf
        [ Parser.succeed 0
            |. Parser.symbol "0"
        , Parser.succeed (\startDigit restDigits -> String.toInt (String.cons startDigit restDigits))
            |= nonZeroDigit
            |= digits
            |> Parser.andThen
                (\maybeNat ->
                    case maybeNat of
                        Just n ->
                            Parser.succeed n

                        Nothing ->
                            Parser.problem "Failed to parse a nat"
                )
        ]


char : Parser Char
char =
    Parser.getChompedString (Parser.chompIf (\_ -> True))
        |> Parser.andThen
            (\str ->
                case String.uncons str of
                    Just ( c, _ ) ->
                        Parser.succeed c

                    Nothing ->
                        Parser.problem "Encounted empty string while parsing a character"
            )


nonZeroDigit : Parser Char
nonZeroDigit =
    char
        |> Parser.andThen
            (\c ->
                if '0' < c && c <= '9' then
                    Parser.succeed c

                else
                    Parser.problem (String.concat [ "the character '", String.fromChar c, "' is not a non-zero digit" ])
            )


digits : Parser String
digits =
    Parser.getChompedString (Parser.chompWhile Char.isDigit)
