module Equation exposing (parse, Equation, SumExpr, IExpr(..), VarName(..))


import Parser exposing(..)
import Set


-- Probably change the underlying type later
type VarName
    = VarName Char


type alias SumExpr =
    { var : VarName
    , start : IExpr
    , end : IExpr
    , expr : IExpr
    }


type alias Equation =
    (IExpr, IExpr)


type IExpr
    = Add IExpr IExpr
    | Mul IExpr IExpr
    | Sub IExpr IExpr
    | Div IExpr IExpr
    | Pow IExpr IExpr
    | Var VarName
    | Literal Int
    | Sum SumExpr
    | Fib IExpr
    | Tri IExpr


parse : String -> Result (List DeadEnd) Equation
parse str =
    run (equation |. end) str


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. whitespace


equation : Parser Equation
equation =
    succeed Tuple.pair
        |= lexeme (intExpr True)
        |. (symbol >> lexeme) "="
        |= lexeme (intExpr True)


var : Parser VarName
var =
    succeed VarName
        |= (
                variable
                { start = Char.isLower
                , inner = always False
                , reserved = Set.empty
                }
                |> andThen
                    (String.uncons
                        >> Maybe.map (Tuple.first >> succeed)
                        >> Maybe.withDefault
                            (problem
                                "This should never happen (There's a variable with the empty string as its name??)"
                            )
                    )
            )


fixSubAssoc : IExpr -> IExpr
fixSubAssoc subExpr =
    let
        helper e acc =
            case e of
                Sub intA intB ->
                    helper intB (Sub (acc intA))

                _ ->
                    acc e

    in
    helper subExpr identity


fixDivAssoc : IExpr -> IExpr
fixDivAssoc divExpr =
    let
        helper e acc =
            case e of
                Div intA intB ->
                    helper intB (Div (acc intA))

                _ ->
                    acc e

    in
    helper divExpr identity


intExpr : Bool -> Parser IExpr
intExpr reverse =
    lexeme (mulExpr True)
        |> andThen (\t -> oneOf [ add t, sub t, succeed t ])
        |> map (if reverse then fixSubAssoc else identity)


mulExpr : Bool -> Parser IExpr
mulExpr reverse =
    lexeme powTerm
        |> andThen (\t -> oneOf [ mul t, div t, succeed t ])
        |> map (if reverse then fixDivAssoc else identity)


var_ : Parser IExpr
var_ =
    succeed Var
        |= var


powTerm : Parser IExpr
powTerm =
    oneOf [ lexeme term |> andThen pow ]


term : Parser IExpr
term =
    oneOf [ paren, literal, var_, tri, fib, sum ]


add : IExpr -> Parser IExpr
add t =
    succeed (Add t)
        |. (symbol >> lexeme) "+"
        |= lazy (\_ -> intExpr True)


sub : IExpr -> Parser IExpr
sub t =
    succeed (Sub t)
        |. (symbol >> lexeme) "-"
        |= lazy (\_ -> intExpr False)


mul : IExpr -> Parser IExpr
mul t =
    succeed (Mul t)
        |. (symbol >> lexeme) "*"
        |= lazy (\_ -> mulExpr True)


div : IExpr -> Parser IExpr
div t =
    succeed (Div t)
        |. (symbol >> lexeme) "/"
        |= lazy (\_ -> mulExpr False)


paren : Parser IExpr
paren =
    succeed identity
        |. (symbol >> lexeme) "("
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ")"


pow : IExpr -> Parser IExpr
pow t =
    oneOf [ succeed (Pow t) |. (symbol >> lexeme) "^" |= lexeme powTerm, succeed t ]


literal : Parser IExpr
literal =
    succeed Literal
        |= lexeme int


tri : Parser IExpr
tri =
    succeed Tri
        |. (keyword >> lexeme) "Tri"
        |. (symbol >> lexeme) "("
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ")"


fib : Parser IExpr
fib =
    succeed Fib
        |. (keyword >> lexeme) "Fib"
        |. (symbol >> lexeme) "("
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ")"


sum : Parser IExpr
sum =
    succeed Sum
    |= (succeed SumExpr
        |. (keyword >> lexeme) "Sum"
        |. (symbol >> lexeme) "("
        |= lexeme var
        |. (symbol >> lexeme) ","
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ","
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ","
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ")")
