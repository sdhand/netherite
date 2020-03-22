module Netherite exposing (..)


import Generalise exposing (..)
import Equation exposing (..)


type Shape
    = Dot
    | Line IExpr
    | Square IExpr
    | Tri IExpr
    | Rect IExpr IExpr
    | Frame IExpr IExpr


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
            mulRepr e1 e2 ++ mulRepr e2 e1

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
            r1 ++ r2


mulRepr : IExpr -> IExpr -> List (Int -> List ProofTree)
mulRepr expr1 expr2 =
    [ \n -> List.repeat (eval expr1) 
    case (expr1, expr2) of
        (Literal i1, Literal i2) ->
            [ \n -> [ ROp <| Rect i1 i2 ], \n -> List.repeat i1 (ROp <| Rect 1 i2) ]

        (Literal i, Var (VarName 'n')) ->
            [ \n -> [ ROp <| Rect i n ], \n -> List.repeat i (ROp <| Rect 1 n) ]

        (Var (VarName 'n'), Literal i) ->
            [ \n -> [ ROp <| Rect n i ], \n -> List.repeat n (ROp <| Rect 1 i) ]

        (Var (VarName 'n'), Var (VarName 'n')) ->
            [ \n -> [ ROp <| Rect n n ], \n -> List.repeat n (ROp <| Rect 1 n) ]

        (Literal i, e) ->
            List.map (\r n -> r n |> List.repeat i |> List.concat) (representations e)

        (Var (VarName 'n'), e) ->
            List.map (\r n -> r n |> List.repeat n |> List.concat) (representations e)

        _ ->
            []
