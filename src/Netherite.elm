module Netherite exposing (Shape(..), Representation, representations)


import Equation exposing (..)
import List.Extra
import Maybe.Extra


type Shape
    = DotR
    | LineR IExpr
    | SquareR IExpr
    | TriR IExpr
    | RectR IExpr IExpr
    | SumR { var : VarName, start : IExpr, end : IExpr, repr : Representation }
    | RepeatRp IExpr Representation


evalRepr : Representation -> IExpr
evalRepr re =
    let
        toExpr r =
            case r of
                DotR ->
                    Literal 1

                LineR e ->
                    e

                SquareR e ->
                    Pow e (Literal 2)

                TriR e ->
                    Tri e

                RectR e1 e2 ->
                    Mul e1 e2

                SumR { var, start, end, repr } ->
                    Sum { var = var, start = start, end = end, expr = evalRepr repr }

                RepeatRp e repr ->
                    Mul e (evalRepr repr)
    in
    List.map toExpr re
        |> List.Extra.foldl1 Add
        |> Maybe.withDefault (Literal 0)


isLineR : Shape -> Bool
isLineR s =
    case s of
        LineR _ ->
            True

        SumR _ ->
            True

        RepeatRp _ _ ->
            True

        _ ->
            False


doSub : IExpr -> (Representation, Representation) -> Maybe Representation
doSub subE (r1, r2) =
    case r1 of
        ((LineR e)::xs) ->
            Just ((LineR (Sub e subE))::(xs++r2))

        ((SumR sr)::xs) ->
            Maybe.map (\r -> (SumR { sr | end = Sub sr.end (Literal 1) })::(r++xs++r2)) (doSub subE (List.partition isLineR sr.repr))

        ((RepeatRp e rr)::xs) ->
            Maybe.map (\r -> (RepeatRp (Sub e (Literal 1)) rr)::(r++xs++r2)) (doSub subE (List.partition isLineR rr))

        (x::xs) ->
            doSub subE (xs, x::r2)

        [] ->
            Nothing


subtractList l1 l2 =
    case l2 of
        (x::xs) ->
            if List.member x l1 then
                subtractList (List.Extra.remove x l1) xs
            else
                Nothing

        [] ->
            Just l1


type alias Representation =
    List Shape


representations : IExpr -> List Representation
representations expr =
    case expr of
        Literal 1 ->
            [ [ DotR ] ]

        Literal i ->
            [ [ LineR (Literal i) ] ]

        Var name ->
            [ [ LineR (Var name) ] ]

        Add e1 e2 ->
            List.concatMap (\i -> List.concatMap (addRepr i) (representations e1)) (representations e2)

        Mul e1 e2 ->
            List.map (\r -> [ RepeatRp e1 r ]) (representations e2)
                ++ List.map (\r -> [ RepeatRp e2 r ]) (representations e1)
                ++ List.concatMap (\i -> List.concatMap (mulRepr i) (representations e1)) (representations e2)

        Sub e1 e2 ->
            List.concatMap (\i -> List.concatMap (subRepr i) (representations e2)) (representations e1)

        Div e (Literal 1) ->
            representations e

        Div e (Literal 2) ->
            List.filterMap div2Repr (representations e)

        Pow e (Literal 1) ->
            representations e

        Pow e (Literal 2) ->
            [ [ SquareR e ] ]

        Sum ({ var, start, end } as sumExpr) ->
            List.map (\r -> [ SumR { var = var, start = start, end = end, repr = r } ] ) (representations sumExpr.expr)

        Fib e ->
            [ [ LineR (Fib e) ] ]

        Tri e ->
            [ [ TriR e ] ]

        _ ->
            []

addRepr : Representation -> Representation -> List Representation
addRepr r1 r2 =
    case (r1, r2) of
        ( [ DotR ], [ DotR ]) ->
            [ [ LineR (Literal 2) ], r1 ++ r2 ]

        ( [ DotR ], [ LineR e ]) ->
            [ [ LineR (Add (Literal 1) e) ], r1 ++ r2 ]

        ([ LineR e ], [ DotR ]) ->
            [ [ LineR (Add e (Literal 1)) ], r1 ++ r2 ]

        ([ LineR e1 ], [ LineR e2 ]) ->
            [ [ LineR (Add e1 e2) ], r1 ++ r2 ]

        _ ->
            [ r1 ++ r2 ]


subRepr : Representation -> Representation -> List Representation
subRepr r1 r2 =
    let
        default =
            subtractList r1 r2
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    case (r1, r2) of
        (_, [ DotR ]) ->
            Maybe.Extra.toList (doSub (Literal 1) (List.partition isLineR r1)) ++ default

        (_, [ LineR e ]) ->
            Maybe.Extra.toList (doSub e (List.partition isLineR r1)) ++ default

        _ ->
            default


div2Repr : Representation -> Maybe Representation
div2Repr repr =
    case repr of
        [ RectR (Add e1 (Literal 1)) e2 ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ RectR (Add (Literal 1) e1) e2 ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ RectR e1 (Add e2 (Literal 1)) ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ RectR e1 (Add (Literal 1) e2) ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ RectR (Sub e1 (Literal 1)) e2 ] ->
            if e1 == e2 then
                Just [ TriR (Sub e1 (Literal 1)) ]
            else
                Nothing

        [ RectR e1 (Sub e2 (Literal 1)) ] ->
            if e1 == e2 then
                Just [ TriR (Sub e2 (Literal 1)) ]
            else
                Nothing

        _ ->
            Nothing


simpleExpr e =
    case e of
        Var _ ->
            True

        Literal _ ->
            True

        _ ->
            False


mulRepr : Representation -> Representation -> List Representation
mulRepr r1 r2 =
    case (r1, r2) of
        ( [ DotR ], _) ->
            [ r2 ]

        (_, [ DotR ]) ->
            [ r1 ]

        ( [ LineR e1 ], [ LineR e2 ] ) ->
            [ [ RectR e1 e2 ], [ LineR (Mul e1 e2) ] ]

        (_, _) ->
            []
