module Field exposing
    ( Field
    , fromString
    , getError
    , getInput
    , getValue
    , init
    , isValid
    )


type Field err a
    = Initialized String
    | Valid String a
    | Invlaid String err


init : String -> Field err a
init =
    Initialized


fromString : (String -> Result err a) -> String -> Field err a
fromString validator input =
    case validator input of
        Err err ->
            Invlaid input err

        Ok a ->
            Valid input a


getInput : Field err a -> String
getInput field =
    case field of
        Initialized input ->
            input

        Valid input _ ->
            input

        Invlaid input _ ->
            input


getValue : Field err a -> Maybe a
getValue field =
    case field of
        Valid _ val ->
            Just val

        _ ->
            Nothing


getError : Field err a -> Maybe err
getError field =
    case field of
        Initialized _ ->
            Nothing

        Valid _ _ ->
            Nothing

        Invlaid _ err ->
            Just err


isValid : Field err a -> Bool
isValid field =
    case field of
        Valid _ _ ->
            True

        _ ->
            False
