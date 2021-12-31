module Lib.Show exposing
    ( Show
    , char
    , commaList
    , implement
    , int
    , show
    , string
    )

-- Note that I don't expose the record


type Show a
    = Show { show : a -> String }


implement : (a -> String) -> Show a
implement toString =
    Show { show = toString }


show : Show a -> a -> String
show implementation =
    case implementation of
        Show impl0 ->
            impl0.show


commaList : Show a -> Show (List a)
commaList implementation =
    implement
        (\xs ->
            xs
                |> List.map (show implementation)
                |> String.join ", "
        )


string : Show String
string =
    implement identity


char : Show Char
char =
    implement (\c -> String.fromList [ c ])


int : Show Int
int =
    implement String.fromInt
