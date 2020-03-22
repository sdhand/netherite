module Netherite exposing (..)


import Generalise exposing (..)
import Equation exposing (..)


representations : IExpr -> List (Int -> List ProofTree)
representations expr =
    case expr of
        Literal i ->
            [ \n -> ROp <| Rect 1 i ]

        Var (VarName 'n') ->
            [ \n -> ROp <| Rect 1 n ]
        Add e1 e2 ->
            addRepr e1 e2 ++ addRepr e2 e1

        Mul e1 e2 ->
            mulRepr e1 e2 ++ mulRepr e2 e1

        _ ->
            []

addRepr : IExpr -> IExpr -> List (Int -> List ProofTree)
addRepr expr1 expr2 =
    List.concatMap (\i -> List.map (\j n -> i n ++ j n) (representations expr1)) (representations expr2)


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
