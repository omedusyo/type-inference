module Parser.Sequence exposing (..)

import Parser exposing ((|.), (|=), Parser)


type alias NonemptyDelimitedSequence d a =
    ( a, List ( d, a ) )


sequenceToList : NonemptyDelimitedSequence d a -> List a
sequenceToList ( first, rest ) =
    first :: List.map Tuple.second rest


alternate : Parser a -> Parser b -> Parser (List ( a, b ))
alternate p q =
    -- empty
    -- p q
    -- p q p q
    -- p q p q p q
    -- ...
    Parser.loop []
        (\state ->
            Parser.oneOf
                [ Parser.succeed (\a b -> Parser.Loop (( a, b ) :: state))
                    |= p
                    |= q
                , Parser.succeed (Parser.Done state)
                ]
        )
        -- TODO: I don't like the reverse here
        |> Parser.map List.reverse


delimitedSequenceWithDelimiter : Parser d -> Parser a -> Parser (NonemptyDelimitedSequence d a)
delimitedSequenceWithDelimiter delim p =
    Parser.succeed (\head tail -> ( head, tail ))
        |= p
        |= alternate delim p


delimitedSequence : Parser d -> Parser a -> Parser (List a)
delimitedSequence delim p =
    delimitedSequenceWithDelimiter delim p
        |> Parser.map sequenceToList
