module Netherite exposing (Shape(..), Representation, representations)


import Equation exposing (..)
import List.Extra


type Shape
    = Dot
    | Line IExpr
    | Square IExpr
    | TriR IExpr
    | Rect IExpr IExpr
    | SumR { var : VarName, start : IExpr, end : IExpr, repr : Representation }
    | Repeat IExpr Representation


evalRepr : Representation -> IExpr
evalRepr re =
    let
        toExpr r =
            case r of
                Dot ->
                    Literal 1

                Line e ->
                    e

                Square e ->
                    Pow e (Literal 2)

                TriR e ->
                    Tri e

                Rect e1 e2 ->
                    Mul e1 e2

                SumR { var, start, end, repr } ->
                    Sum { var = var, start = start, end = end, expr = evalRepr repr }

                Repeat e repr ->
                    Mul e (evalRepr repr)
    in
    List.map toExpr re
        |> List.Extra.foldl1 Add
        |> Maybe.withDefault (Literal 0)


isLineR : Shape -> Bool
isLineR s =
    case s of
        Line _ ->
            True

        SumR _ ->
            True

        Repeat _ _ ->
            True

        _ ->
            False


doSub : IExpr -> (Representation, Representation) -> List Representation
doSub subE (r1, r2) =
    case r1 of
        ((Line e)::xs) ->
            ((Line (Sub e subE))::(xs++r2))::(doSub subE (xs, (Line e)::r2))

        ((SumR sr)::xs) ->
            List.map (\r -> (SumR { sr | end = Sub sr.end (Literal 1) })::(r++xs++r2)) (doSub subE (List.partition isLineR sr.repr))
                ++ doSub subE (xs, (SumR sr)::r2)

        ((Repeat e rr)::xs) ->
            List.map (\r -> (Repeat (Sub e (Literal 1)) rr)::(r++xs++r2)) (doSub subE (List.partition isLineR rr))
                ++ doSub subE (xs, (Repeat e rr)::r2)

        (x::xs) ->
            doSub subE (xs, x::r2)

        [] ->
            []


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
            [ [ Dot ] ]

        Literal i ->
            [ [ Line (Literal i) ] ]

        Var name ->
            [ [ Line (Var name) ] ]

        Add e1 e2 ->
            List.concatMap (\i -> List.concatMap (addRepr i) (representations e1)) (representations e2)

        Mul e1 e2 ->
            List.map (\r -> [ Repeat e1 r ]) (representations e2)
                ++ List.map (\r -> [ Repeat e2 r ]) (representations e1)
                ++ List.concatMap (\i -> List.concatMap (mulRepr i) (representations e1)) (representations e2)

        Sub e1 e2 ->
            List.concatMap (\i -> List.concatMap (subRepr i) (representations e1)) (representations e2)

        Div e (Literal 1) ->
            representations e

        Div e (Literal 2) ->
            List.filterMap div2Repr (representations e)

        Pow e (Literal 1) ->
            representations e

        Pow e (Literal 2) ->
            [ [ Square e ] ]

        Sum ({ var, start, end } as sumExpr) ->
            List.map (\r -> [ SumR { var = var, start = start, end = end, repr = r } ] ) (representations sumExpr.expr)

        Fib e ->
            [ [ Line (Fib e) ] ]

        Tri e ->
            [ [ TriR e ] ]

        _ ->
            []

addRepr : Representation -> Representation -> List Representation
addRepr r1 r2 =
    case (r1, r2) of
        ( [ Dot ], [ Dot ]) ->
            [ [ Line (Literal 2) ], r1 ++ r2 ]

        ( [ Dot ], [ Line e ]) ->
            [ [ Line (Add (Literal 1) e) ], r1 ++ r2 ]

        ([ Line e ], [ Dot ]) ->
            [ [ Line (Add e (Literal 1)) ], r1 ++ r2 ]

        ([ Line e1 ], [ Line e2 ]) ->
            [ [ Line (Add e1 e2) ], r1 ++ r2 ]

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
        (_, [ Dot ]) ->
            doSub (Literal 1) (List.partition isLineR r1) ++ default

        (_, [ Line e ]) ->
            doSub e (List.partition isLineR r1) ++ default

        _ ->
            default


div2Repr : Representation -> Maybe Representation
div2Repr repr =
    case repr of
        [ Rect (Add e1 (Literal 1)) e2 ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ Rect (Add (Literal 1) e1) e2 ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ Rect e1 (Add e2 (Literal 1)) ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ Rect e1 (Add (Literal 1) e2) ] ->
            if e1 == e2 then
                Just [ TriR e1 ]
            else
                Nothing

        [ Rect (Sub e1 (Literal 1)) e2 ] ->
            if e1 == e2 then
                Just [ TriR (Sub e1 (Literal 1)) ]
            else
                Nothing

        [ Rect e1 (Sub e2 (Literal 1)) ] ->
            if e1 == e2 then
                Just [ TriR (Sub e2 (Literal 1)) ]
            else
                Nothing

        _ ->
            Nothing


mulRepr : Representation -> Representation -> List Representation
mulRepr r1 r2 =
    case (r1, r2) of
        ( [ Dot ], _) ->
            [ r2 ]

        (_, [ Dot ]) ->
            [ r1 ]

        ( [ Line e1 ], [ Line e2 ] ) ->
            [ [ Rect e1 e2 ] ]

        (_, _) ->
            []
