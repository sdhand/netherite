module Generalise exposing (..)


import Browser exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, r)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type SquareOp
    = LCutS LOp SquareOp
    | SplitInnerSquare FrameOp SquareOp
    | SplitOuterFrame FrameOp SquareOp
    | Split4 SquareOp SquareOp SquareOp SquareOp
    | SplitDiaS TriOp TriOp
    | Square Int
    | RecurseS
    | RepeatS { a : Int, b : Int, repeat : SquareOp, tail : SquareOp }
    | NextS


type RectOp
    = SplitSquare SquareOp RectOp
    | SplitDiaR TriOp TriOp
    | ToSquare SquareOp
    | Rotate RectOp
    | Rect Int Int
    | RecurseR
    | RepeatR { a : Int, b : Int, repeat : RectOp, tail : RectOp }
    | NextR


type FrameOp
    = SplitFrame RectOp RectOp RectOp RectOp
    | Frame Int Int
    | RecurseF
    | RepeatF { a : Int, b : Int, repeat : FrameOp, tail : FrameOp }
    | NextF


type TriOp
    = SplitTST TriOp SquareOp TriOp
    | LCutT LOp TriOp
    | SplitSide RectOp TriOp
    | Tri Int
    | RecurseT
    | RepeatT { a : Int, b : Int, repeat : TriOp, tail : TriOp }
    | NextT


type LOp
    = SplitEnds RectOp LOp
    | L Int
    | RecurseL
    | RepeatL { a : Int, b : Int, repeat : LOp, tail : LOp }
    | NextL


type ProofTree
    = SOp SquareOp
    | ROp RectOp
    | FOp FrameOp
    | TOp TriOp
    | LOp LOp


type alias SchematicProof =
    { step : ProofTree
    , base : ProofTree
    }


type Repeater
    = SRepeater { a : Int, b : Int, repeat : SquareOp, tail : SquareOp, i : Int }
    | RRepeater { a : Int, b : Int, repeat : RectOp, tail : RectOp, i : Int }
    | FRepeater { a : Int, b : Int, repeat : FrameOp, tail : FrameOp, i : Int }
    | TRepeater { a : Int, b : Int, repeat : TriOp, tail : TriOp, i : Int }
    | LRepeater { a : Int, b : Int, repeat : LOp, tail : LOp, i : Int }


type Stage
    = Proving { operation : String }
    | Initialising
        { n : String
        , shape : String
        , p1 : String
        , p2 : String
        }
    | Done


type alias Model =
    { proofs : List ( Int, ProofTree )
    , stage : Stage
    }

type Msg
    = SetN String
    | SetShape String
    | SetP1 String
    | SetP2 String
    | Next
    | SetOp String
    | UpdateProof ProofTree


radius : Int
radius =
    5


distance : Int
distance =
    20


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    (
        { stage =
            Initialising
                { n = ""
                , shape = "square"
                , p1 = ""
                , p2 = ""
                }
        , proofs = []
        }
    , Cmd.none
    )


draw x y =
    circle
        [ cx <| String.fromInt <| radius + distance * x
        , cy <| String.fromInt <| radius + distance * y
        , r <| String.fromInt radius
        ]
        []


row : Int -> Int -> Int -> List (Svg Msg)
row start end y =
    List.map (\x -> draw x y) (List.range start (end-1))


column : Int -> Int -> Int -> List (Svg Msg)
column start end x =
    List.map (draw x) (List.range start (end-1))


drawSOp : String -> (SquareOp -> ProofTree) -> SquareOp -> List (Html Msg)
drawSOp op rest tree =
    case tree of
        RecurseS ->
            []

        RepeatS _ ->
            []

        NextS ->
            []

        LCutS l s ->
            drawLOp op (rest << \x -> LCutS x s) l ++ drawSOp op (rest << LCutS l) s

        SplitInnerSquare f s ->
            drawFOp op (rest << \x -> SplitInnerSquare x s) f ++ drawSOp op (rest << SplitInnerSquare f) s

        SplitOuterFrame f s ->
            drawFOp op (rest << \x -> SplitOuterFrame x s) f ++ drawSOp op (rest << SplitOuterFrame f) s

        Split4 s1 s2 s3 s4 ->
            drawSOp op (rest << \x -> Split4 x s2 s3 s4) s1
                ++ drawSOp op (rest << \x -> Split4 s1 x s3 s4) s2
                ++ drawSOp op (rest << \x -> Split4 s1 s2 x s4) s3
                ++ drawSOp op (rest << Split4 s1 s2 s3) s4

        SplitDiaS t1 t2 ->
            drawTOp op (rest << \x -> SplitDiaS x t2) t1 ++ drawTOp op (rest << SplitDiaS t1) t2

        Square n ->
            let
                picture event =
                    [ svg
                        ([ Html.Attributes.style "padding" "30px", width <| n*distance-2*radius, height <| n*distance-2*radius ] ++ event)
                        (List.concatMap (column 0 n) (List.range 0 (n-1)))
                    ]
            in
            case op of
                "lcut" ->
                    if n > 1 then
                        picture [ onClick (UpdateProof (LCutS (L n) (Square (n-1)) |> rest)) ]
                    else
                        picture []

                "splitDia" ->
                    if n > 1 then
                        picture [ onClick (UpdateProof (SplitDiaS (Tri n) (Tri (n-1)) |> rest)) ]
                    else
                        picture []

                "splitOuterFrame" ->
                    if n >= 3 then
                        picture [ onClick (UpdateProof (SplitOuterFrame (Frame n 1) (Square (n-2)) |> rest)) ]
                    else
                        picture []

                "splitInnerSquare" ->
                    if n >= 3 then
                        picture [ onClick (UpdateProof (SplitInnerSquare (Frame n ((n-1)//2)) (Square (n-2*((n-1)//2))) |> rest)) ]
                    else
                        picture []

                "split4" ->
                    if n >= 2 && modBy 2 n == 0 then
                        picture [ onClick (UpdateProof (Split4 (Square (n//2)) (Square (n//2)) (Square (n//2)) (Square (n//2)) |> rest)) ]
                    else
                        picture []

                _ ->
                    picture []


drawROp : String -> (RectOp -> ProofTree) -> RectOp -> List (Html Msg)
drawROp op rest tree =
    case tree of
        RecurseR ->
            []

        RepeatR _ ->
            []

        NextR ->
            []

        SplitSquare s r ->
            drawSOp op (rest << \x -> SplitSquare x r) s ++ drawROp op (rest << SplitSquare s) r

        Rotate r ->
            drawROp op (rest << Rotate) r

        ToSquare s ->
            drawSOp op (rest << ToSquare) s

        SplitDiaR t1 t2 ->
            drawTOp op (rest << \x -> SplitDiaR x t2) t1 ++ drawTOp op (rest << SplitDiaR t1) t2

        Rect n m ->
            let
                picture event =
                    [ svg
                        ([ Html.Attributes.style "padding" "30px", width <| n*distance-2*radius, height <| m*distance-2*radius ] ++ event)
                        (List.concatMap (column 0 m) (List.range 0 (n-1)))
                    ]
            in
            case op of
                "rotate" ->
                    picture [ onClick (UpdateProof (Rotate (Rect m n) |> rest)) ]

                "splitDia" ->
                    if n-m == 1 then
                        picture [ onClick (UpdateProof (SplitDiaR (Tri m) (Tri m) |> rest)) ]
                    else if m-n == 1 then
                        picture [ onClick (UpdateProof (SplitDiaR (Tri n) (Tri n) |> rest)) ]
                    else
                        picture []

                "splitSquare" ->
                    if n > m then
                        picture [ onClick (UpdateProof (SplitSquare (Square m) (Rect (n-m) m) |> rest)) ]
                    else if m > n then
                        picture [ onClick (UpdateProof (SplitSquare (Square n) (Rect n (m-n)) |> rest)) ]
                    else
                        picture []

                "toSquare" ->
                    if n == m then
                        picture [ onClick (UpdateProof (ToSquare (Square n) |> rest)) ]
                    else
                        picture []

                _ ->
                    picture []


drawTOp : String -> (TriOp -> ProofTree) -> TriOp -> List (Html Msg)
drawTOp op rest tree =
    case tree of
        RecurseT ->
            []

        RepeatT _ ->
            []

        NextT ->
            []

        SplitTST t1 s t2 ->
            drawTOp op (rest << \x -> SplitTST x s t2) t1
                ++ drawSOp op (rest << \x -> SplitTST t1 x t2) s
                ++ drawTOp op (rest << SplitTST t1 s) t2

        LCutT l t ->
            drawLOp op (rest << \x -> LCutT x t) l ++ drawTOp op (rest << LCutT l) t

        SplitSide r t ->
            drawROp op (rest << \x -> SplitSide x t) r ++ drawTOp op (rest << SplitSide r) t

        Tri n ->
            let
                picture event =
                    [ svg
                        ([ Html.Attributes.style "padding" "30px", width <| n*distance-2*radius, height <| n*distance-2*radius ] ++ event)
                        (List.concatMap (\x -> row 0 (x+1) x) (List.range 0 (n-1)))
                    ]
            in
            case op of
                "splitTST" ->
                    if n > 1 then
                        picture [ onClick (UpdateProof (SplitTST (Tri (n//2)) (Square (n-n//2)) (Tri (n//2)) |> rest)) ]
                    else
                        picture []

                "lcut" ->
                    if n > 2 then
                        picture [ onClick (UpdateProof (LCutT (L n) (Tri (n-2)) |> rest)) ]
                    else
                        picture []

                "splitSide" ->
                    if n > 1 then
                        picture [ onClick (UpdateProof (SplitSide (Rect 1 n) (Tri (n-1)) |> rest)) ]
                    else
                        picture []

                _ ->
                    picture []



drawFOp : String -> (FrameOp -> ProofTree) -> FrameOp -> List (Html Msg)
drawFOp op rest tree =
    case tree of
        RecurseF ->
            []

        RepeatF _ ->
            []

        NextF ->
            []

        SplitFrame r1 r2 r3 r4 ->
            drawROp op (rest << \x -> SplitFrame x r2 r3 r4) r1
                ++ drawROp op (rest << \x -> SplitFrame r1 x r3 r4) r2
                ++ drawROp op (rest << \x -> SplitFrame r1 r2 x r4) r3
                ++ drawROp op (rest << SplitFrame r1 r2 r3) r4

        Frame n m ->
            let
                picture event =
                    [ svg
                        ([ Html.Attributes.style "padding" "30px", width <| n*distance-2*radius, height <| n*distance-2*radius ] ++ event)
                        (List.concatMap (column 0 n) (List.range 0 (m-1))
                            ++ List.concatMap (row m (n-m)) (List.range 0 (m-1))
                            ++ List.concatMap (row m (n-m)) (List.range (n-m) (n-1))
                            ++ List.concatMap (column 0 n) (List.range (n-m) (n-1)))
                    ]
            in
            case op of
                "splitFrame" ->
                    picture [ onClick (UpdateProof (SplitFrame (Rect (n-m) m) (Rect (n-m) m) (Rect m (n-m)) (Rect m (n-m)) |> rest)) ]

                _ ->
                    picture []



drawLOp : String -> (LOp -> ProofTree) -> LOp -> List (Html Msg)
drawLOp op rest tree =
    case tree of
        RecurseL ->
            []

        RepeatL _ ->
            []

        NextL ->
            []

        SplitEnds r l ->
            drawROp op (rest << \x -> SplitEnds x l) r ++ drawLOp op (rest << SplitEnds r) l

        L n ->
            let
                picture event =
                    [ svg
                        ([ Html.Attributes.style "padding" "30px", width <| n*distance-2*radius, height <| n*distance-2*radius ] ++ event)
                        ((row 1 n (n-1))++(column 0 n 0))
                    ]
            in
            case op of
                "splitEnds" ->
                    if n > 1 then
                        picture [ onClick (UpdateProof (SplitEnds (Rect 1 2) (L (n-1)) |> rest)) ]
                    else
                        picture []

                _ ->
                    picture []


ifFailed : Maybe a -> Maybe a -> Maybe a
ifFailed next prev =
    case prev of
        Nothing ->
            next

        Just _ ->
            prev


drawShapes : String -> ProofTree -> List (Html Msg)
drawShapes op tree =
    case tree of
        SOp s ->
            drawSOp op SOp s

        ROp r ->
            drawROp op ROp r

        TOp t ->
            drawTOp op TOp t

        FOp f ->
            drawFOp op FOp f

        LOp l ->
            drawLOp op LOp l


sEq : SquareOp -> SquareOp -> Bool
sEq s1 s2 =
    case (s1, s2) of
        (RecurseS, _) ->
            True

        (_, RecurseS) ->
            True

        (Square _, Square _) ->
            True

        (LCutS l11 s11, LCutS l21 s21) ->
            lEq l11 l21 && sEq s11 s21

        (Split4 s11 s12 s13 s14, Split4 s21 s22 s23 s24) ->
            sEq s11 s21 && sEq s12 s22 && sEq s13 s23 && sEq s14 s24

        (SplitDiaS t11 t12, SplitDiaS t21 t22) ->
            tEq t11 t21 && tEq t12 t22

        (SplitOuterFrame f11 s11, SplitOuterFrame f21 s21) ->
            fEq f11 f21 && sEq s11 s21

        (SplitInnerSquare f11 s11, SplitInnerSquare f21 s21) ->
            fEq f11 f21 && sEq s11 s21

        _ ->
            False


rEq : RectOp -> RectOp -> Bool
rEq r1 r2 =
    case (r1, r2) of
        (RecurseR, _) ->
            True

        (_, RecurseR) ->
            True

        (Rect _ _, Rect _ _) ->
            True

        (SplitSquare s11 r11, SplitSquare s21 r21) ->
            sEq s11 s21 && rEq r11 r21

        (SplitDiaR t11 t12, SplitDiaR t21 t22) ->
            tEq t11 t21 && tEq t12 t22

        _ ->
            False


tEq : TriOp -> TriOp -> Bool
tEq t1 t2 =
    case (t1, t2) of
        (RecurseT, _) ->
            True

        (_, RecurseT) ->
            True

        (Tri _, Tri _) ->
            True

        (LCutT l11 t11, LCutT l21 t21) ->
            lEq l11 l21 && tEq t11 t21

        (SplitTST t11 s11 t12, SplitTST t21 s21 t22) ->
            tEq t11 t21 && sEq s11 s21 && tEq t12 t22

        (SplitSide r11 t11, SplitSide r21 t21) ->
            rEq r11 r21 && tEq t11 t21

        _ ->
            False


fEq : FrameOp -> FrameOp -> Bool
fEq f1 f2 =
    case (f1, f2) of
        (RecurseF, _) ->
            True

        (_, RecurseF) ->
            True

        (Frame _ _, Frame _ _) ->
            True

        (SplitFrame r11 r12 r13 r14, SplitFrame r21 r22 r23 r24) ->
            rEq r11 r21 && rEq r12 r22 && rEq r13 r23 && rEq r14 r24

        _ ->
            False


lEq : LOp -> LOp -> Bool
lEq l1 l2 =
    case (l1, l2) of
        (RecurseL, _) ->
            True

        (_, RecurseL) ->
            True

        (L _, L _) ->
            True

        (SplitEnds r11 l11, SplitEnds r21 l21) ->
            rEq r11 r21 && lEq l11 l21

        _ ->
            False


ptEq : ProofTree -> ProofTree -> Bool
ptEq p1 p2 =
    case (p1, p2) of
        (SOp s1, SOp s2) ->
            sEq s1 s2

        (ROp r1, ROp r2) ->
            rEq r1 r2

        (TOp t1, TOp t2) ->
            tEq t1 t2

        (FOp f1, FOp f2) ->
            fEq f1 f2

        (LOp l1, LOp l2) ->
            lEq l1 l2

        _ ->
            False


findDifferenceS : (SquareOp -> ProofTree) -> ProofTree -> SquareOp -> Maybe ProofTree
findDifferenceS diff small large =
    if ptEq small (SOp large) then
        Just (diff RecurseS)
    else
        case large of
            Square _ ->
                Nothing

            RecurseS ->
                Nothing

            RepeatS _ ->
                Nothing

            NextS ->
                Nothing

            Split4 s1 s2 s3 s4 ->
                findDifferenceS (diff << \x -> Split4 x s2 s3 s4) small s1
                    |> ifFailed (findDifferenceS (diff << \x -> Split4 s1 x s3 s4) small s2)
                    |> ifFailed (findDifferenceS (diff << \x -> Split4 s1 s2 x s4) small s3)
                    |> ifFailed (findDifferenceS (diff << Split4 s1 s2 s3) small s4)

            LCutS l s ->
                findDifferenceL (diff << \x -> LCutS x s) small l
                    |> ifFailed ((findDifferenceS (diff << LCutS l)) small s)

            SplitOuterFrame f s ->
                findDifferenceF (diff << \x -> SplitOuterFrame x s) small f
                    |> ifFailed (findDifferenceS (diff << SplitOuterFrame f) small s)

            SplitInnerSquare f s ->
                findDifferenceF (diff << \x -> SplitInnerSquare x s) small f
                    |> ifFailed (findDifferenceS (diff << SplitInnerSquare f) small s)

            SplitDiaS t1 t2 ->
                findDifferenceT (diff << \x -> SplitDiaS x t2) small t1
                    |> ifFailed (findDifferenceT (diff << SplitDiaS t1) small t2)



findDifferenceR : (RectOp -> ProofTree) -> ProofTree -> RectOp -> Maybe ProofTree
findDifferenceR diff small large =
    if ptEq small (ROp large) then
        Just (diff RecurseR)
    else
        case large of
            Rect _ _ ->
                Nothing

            RecurseR ->
                Nothing

            RepeatR _ ->
                Nothing

            NextR ->
                Nothing

            SplitSquare s r ->
                findDifferenceS (diff << \x -> SplitSquare x r) small s
                    |> ifFailed (findDifferenceR (diff << SplitSquare s) small r)

            SplitDiaR t1 t2 ->
                findDifferenceT (diff << \x -> SplitDiaR x t2) small t1
                    |> ifFailed (findDifferenceT (diff << SplitDiaR t1) small t2)

            ToSquare s ->
                findDifferenceS (diff << ToSquare) small s

            Rotate r ->
                findDifferenceR (diff << Rotate) small r


findDifferenceT : (TriOp -> ProofTree) -> ProofTree -> TriOp -> Maybe ProofTree
findDifferenceT diff small large =
    if ptEq small (TOp large) then
        Just (diff RecurseT)
    else
        case large of
            Tri _ ->
                Nothing

            RecurseT ->
                Nothing

            RepeatT _ ->
                Nothing

            NextT ->
                Nothing

            SplitTST t1 s t2 ->
                findDifferenceT (diff << \x -> SplitTST x s t2) small t1
                    |> ifFailed (findDifferenceS (diff << \x -> SplitTST t1 x t2) small s)
                    |> ifFailed (findDifferenceT (diff << SplitTST t1 s) small t2)

            LCutT l t ->
                findDifferenceL (diff << \x -> LCutT x t) small l
                    |> ifFailed (findDifferenceT (diff << LCutT l) small t)

            SplitSide r t ->
                findDifferenceR (diff << \x -> SplitSide x t) small r
                    |> ifFailed (findDifferenceT (diff << SplitSide r) small t)


findDifferenceF : (FrameOp -> ProofTree) -> ProofTree -> FrameOp -> Maybe ProofTree
findDifferenceF diff small large =
    if ptEq small (FOp large) then
        Just (diff RecurseF)
    else
        case large of
            Frame _ _ ->
                Nothing

            RecurseF ->
                Nothing

            RepeatF _ ->
                Nothing

            NextF ->
                Nothing

            SplitFrame r1 r2 r3 r4 ->
                findDifferenceR (diff << \x -> SplitFrame x r2 r3 r4) small r1
                    |> ifFailed (findDifferenceR (diff << \x -> SplitFrame r1 x r3 r4) small r2)
                    |> ifFailed (findDifferenceR (diff << \x -> SplitFrame r1 r2 x r4) small r3)
                    |> ifFailed (findDifferenceR (diff << SplitFrame r1 r2 r3) small r4)

findDifferenceL : (LOp -> ProofTree) -> ProofTree -> LOp -> Maybe ProofTree
findDifferenceL diff small large =
    if ptEq small (LOp large) then
        Just (diff RecurseL)
    else
        case large of
            L _ ->
                Nothing

            RecurseL ->
                Nothing

            RepeatL _ ->
                Nothing

            NextL ->
                Nothing

            SplitEnds r l ->
                findDifferenceR (diff << \x -> SplitEnds x l) small r
                    |> ifFailed (findDifferenceL (diff << SplitEnds r) small l)


findDifference : ProofTree -> ProofTree -> Maybe ProofTree
findDifference small large =
        case large of
            SOp s ->
                findDifferenceS SOp small s

            ROp r ->
                findDifferenceR ROp small r

            TOp t ->
                findDifferenceT TOp small t

            FOp f ->
                findDifferenceF FOp small f

            LOp l ->
                findDifferenceL LOp small l


buildRepeatS : Maybe Repeater -> Int -> SquareOp -> Maybe SquareOp
buildRepeatS repeater n sq =
    case sq of
        RepeatS { a, b, repeat, tail } ->
           buildRepeatS (Just (SRepeater { a = a, b = b, repeat = repeat, tail = tail, i = a*n+b })) n repeat

        NextS ->
            case repeater of
                Just (SRepeater { a, b, repeat, tail, i }) ->
                    if i == 0 then
                        buildRepeatS Nothing n tail
                    else
                        buildRepeatS (Just (SRepeater { a = a, b = b, repeat = repeat, tail = tail, i = i-1 })) n repeat

                _ ->
                    Nothing

        LCutS l s ->
            Maybe.map2 LCutS (buildRepeatL repeater n l) (buildRepeatS repeater n s)

        SplitOuterFrame f s ->
            Maybe.map2 SplitOuterFrame (buildRepeatF repeater n f) (buildRepeatS repeater n s)

        SplitInnerSquare f s ->
            Maybe.map2 SplitInnerSquare (buildRepeatF repeater n f) (buildRepeatS repeater n s)

        SplitDiaS t1 t2 ->
            Maybe.map2 SplitDiaS (buildRepeatT repeater n t1) (buildRepeatT repeater n t2)

        Split4 s1 s2 s3 s4 ->
            Maybe.map4 Split4 (buildRepeatS repeater n s1) (buildRepeatS repeater n s2) (buildRepeatS repeater n s3) (buildRepeatS repeater n s4)

        Square x ->
            Just (Square x)

        RecurseS ->
            Just RecurseS


buildRepeatR : Maybe Repeater -> Int -> RectOp -> Maybe RectOp
buildRepeatR repeater n re =
    case re of
        RepeatR { a, b, repeat, tail } ->
            buildRepeatR (Just (RRepeater { a = a, b = b, repeat = repeat, tail = tail, i = a*n+b })) n repeat

        NextR ->
            case repeater of
                Just (RRepeater { a, b, repeat, tail, i }) ->
                    if i == 0 then
                        buildRepeatR Nothing n tail
                    else
                        buildRepeatR (Just (RRepeater { a = a, b = b, repeat = repeat, tail = tail, i = i-1 })) n repeat

                _ ->
                    Nothing

        SplitDiaR t1 t2 ->
            Maybe.map2 SplitDiaR (buildRepeatT repeater n t1) (buildRepeatT repeater n t2)

        SplitSquare s r ->
            Maybe.map2 SplitSquare (buildRepeatS repeater n s) (buildRepeatR repeater n r)

        ToSquare s ->
            Maybe.map ToSquare (buildRepeatS repeater n s)

        Rotate r ->
            Maybe.map Rotate (buildRepeatR repeater n r)

        Rect x y ->
            Just (Rect x y)

        RecurseR ->
            Just RecurseR


buildRepeatT : Maybe Repeater -> Int -> TriOp -> Maybe TriOp
buildRepeatT repeater n tr =
    case tr of
        RepeatT { a, b, repeat, tail } ->
            buildRepeatT (Just (TRepeater { a = a, b = b, repeat = repeat, tail = tail, i = a*n+b })) n repeat

        NextT ->
            case repeater of
                Just (TRepeater { a, b, repeat, tail, i }) ->
                    if i == 0 then
                        buildRepeatT Nothing n tail
                    else
                        buildRepeatT (Just (TRepeater { a = a, b = b, repeat = repeat, tail = tail, i = i-1 })) n repeat

                _ ->
                    Nothing

        SplitTST t1 s t2 ->
            Maybe.map3 SplitTST (buildRepeatT repeater n t1) (buildRepeatS repeater n s) (buildRepeatT repeater n t2)

        LCutT l t ->
            Maybe.map2 LCutT (buildRepeatL repeater n l) (buildRepeatT repeater n t)

        SplitSide r t ->
            Maybe.map2 SplitSide (buildRepeatR repeater n r) (buildRepeatT repeater n t)

        Tri x ->
            Just (Tri x)

        RecurseT ->
            Just RecurseT


buildRepeatF : Maybe Repeater -> Int -> FrameOp -> Maybe FrameOp
buildRepeatF repeater n fr =
    case fr of
        RepeatF { a, b, repeat, tail } ->
            buildRepeatF (Just (FRepeater { a = a, b = b, repeat = repeat, tail = tail, i = a*n+b })) n repeat

        NextF ->
            case repeater of
                Just (FRepeater { a, b, repeat, tail, i }) ->
                    if i == 0 then
                        buildRepeatF Nothing n tail
                    else
                        buildRepeatF (Just (FRepeater { a = a, b = b, repeat = repeat, tail = tail, i = i-1 })) n repeat

                _ ->
                    Nothing

        SplitFrame r1 r2 r3 r4 ->
            Maybe.map4 SplitFrame (buildRepeatR repeater n r1) (buildRepeatR repeater n r2) (buildRepeatR repeater n r3) (buildRepeatR repeater n r4)

        Frame x y ->
            Just (Frame x y)

        RecurseF ->
            Just RecurseF


buildRepeatL : Maybe Repeater -> Int -> LOp -> Maybe LOp
buildRepeatL repeater n ll =
    case ll of
        RepeatL { a, b, repeat, tail } ->
            buildRepeatL (Just (LRepeater { a = a, b = b, repeat = repeat, tail = tail, i = a*n+b })) n repeat

        NextL ->
            case repeater of
                Just (LRepeater { a, b, repeat, tail, i }) ->
                    if i == 0 then
                        buildRepeatL Nothing n tail
                    else
                        buildRepeatL (Just (LRepeater { a = a, b = b, repeat = repeat, tail = tail, i = i-1 })) n repeat

                _ ->
                    Nothing

        SplitEnds r l ->
            Maybe.map2 SplitEnds (buildRepeatR repeater n r) (buildRepeatL repeater n l)

        L x ->
            Just (L x)

        RecurseL ->
            Just RecurseL



buildRepeat : Int -> ProofTree -> Maybe ProofTree
buildRepeat n tree =
    case tree of
        SOp s ->
            buildRepeatS Nothing n s
                |> Maybe.map SOp

        ROp r ->
            buildRepeatR Nothing n r
                |> Maybe.map ROp

        TOp t ->
            buildRepeatT Nothing n t
                |> Maybe.map TOp

        FOp f ->
            buildRepeatF Nothing n f
                |> Maybe.map FOp

        LOp l ->
            buildRepeatL Nothing n l
                |> Maybe.map LOp


infer : (Int, ProofTree) -> (Int, ProofTree) -> Maybe SchematicProof
infer (n1, p1) (n2, p2) =
    let
        (largeN, largeP) =
            if n1 > n2 then
                (n1, p1)
            else
                (n2, p2)

        (smallN, smallP) =
            if n1 < n2 then
                (n1, p1)
            else
                (n2, p2)

        diff =
            findDifference smallP largeP
    in
    if large == small || diff == Nothing then
        Nothing
    else
       Nothing



view : Model -> Document Msg
view model =
    { title = "Netherite"
    , body =
        case model.stage of
            Initialising i ->
                input
                    [ type_ "number", Html.Attributes.value i.n, onInput SetN ]
                    []
                ::select
                    [ onInput SetShape ]
                    [ option [ Html.Attributes.value "square", Html.Attributes.selected True ] [ Html.text "Square" ]
                    , option [ Html.Attributes.value "rect" ] [ Html.text "Rectangle" ]
                    , option [ Html.Attributes.value "tri" ] [ Html.text "Triangle" ]
                    , option [ Html.Attributes.value "l" ] [ Html.text "L" ]
                    , option [ Html.Attributes.value "frame" ] [ Html.text "Frame" ]
                    ]
                :: input [ type_ "number", Html.Attributes.value i.p1, onInput SetP1 ] []
                :: (if i.shape == "rect" || i.shape == "frame" then [ input [ type_ "number", Html.Attributes.value i.p2, onInput SetP2 ] [] ] else [])
                ++ [ button [ onClick Next ] [ Html.text "Prove..." ] ]

            Proving p ->
                case List.head model.proofs of
                    Just ( n, tree ) ->
                        select
                            [ onInput SetOp ]
                            [ option [ Html.Attributes.value "none", Html.Attributes.selected True ] [ Html.text "None" ]
                            , option [ Html.Attributes.value "splitInnerSquare" ] [ Html.text "Split Inner Square" ]
                            , option [ Html.Attributes.value "split4" ] [ Html.text "Split Four" ]
                            , option [ Html.Attributes.value "splitEnds" ] [ Html.text "Split Ends" ]
                            , option [ Html.Attributes.value "splitOuterFrame" ] [ Html.text "Split Outer Frame" ]
                            , option [ Html.Attributes.value "splitFrame" ] [ Html.text "Split Frame" ]
                            , option [ Html.Attributes.value "splitSquare" ] [ Html.text "Split Square" ]
                            , option [ Html.Attributes.value "splitSide" ] [ Html.text "Split Side" ]
                            , option [ Html.Attributes.value "splitTST" ] [ Html.text "Split TST" ]
                            , option [ Html.Attributes.value "splitDia" ] [ Html.text "Split Diagonal" ]
                            , option [ Html.Attributes.value "lcut" ] [ Html.text "L-Cut" ]
                            , option [ Html.Attributes.value "rotate" ] [ Html.text "Rotate" ]
                            , option [ Html.Attributes.value "toSquare" ] [ Html.text "To Square" ]
                            ]
                        ::button [ onClick Next ] [Html.text "Next Proof" ]
                        ::drawShapes p.operation tree

                    Nothing ->
                        [ Html.text "Something has gone terribly wrong" ]

            Done ->
                []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.stage of
        Initialising i ->
            case msg of
                SetN n ->
                    ({ model | stage = Initialising { i | n = n } }, Cmd.none)

                SetShape shape ->
                    ({ model | stage = Initialising { i | shape = shape } }, Cmd.none)

                SetP1 p1 ->
                    ({ model | stage = Initialising { i | p1 = p1 } }, Cmd.none)

                SetP2 p2 ->
                    ({ model | stage = Initialising { i | p2 = p2 } }, Cmd.none)

                Next ->
                    case (String.toInt i.n, (i.shape, String.toInt i.p1, String.toInt i.p2)) of
                        ( Just n, ("square", Just p1, _)) ->
                            ( { proofs = (n, (SOp (Square p1)))::model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                        ( Just n, ("rect", Just p1, Just p2)) ->
                            ( { proofs = (n, ROp (Rect p1 p2))::model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                        ( Just n, ("tri", Just p1, _ )) ->
                            ( { proofs = (n, TOp (Tri p1))::model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                        ( Just n, ("l", Just p1, _ )) ->
                            ( { proofs = (n, LOp (L p1))::model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                        ( Just n, ("frame", Just p1, Just p2)) ->
                            ( { proofs = (n, FOp (Frame p1 p2))::model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                        _ ->
                            (model, Cmd.none)
                _ ->
                    (model, Cmd.none)

        Proving p ->
            case msg of
                SetOp operation ->
                    ( { model | stage = Proving { p | operation = operation } }, Cmd.none )

                UpdateProof proof ->
                    case model.proofs of
                        ((n, x)::xs) ->
                            ( { model | proofs = (n, proof)::xs }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Next ->
                    if List.length model.proofs >= 2 then
                        ( {model | stage = Done }, Cmd.none )
                    else
                        ( { model | stage = Initialising { n = "", shape = "square", p1 = "", p2 = "" } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Done ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
