module Generalise exposing (..)


import Browser exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, r)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Arithmetic exposing (gcd, divisors, divides)
import Maybe.Extra
import List.Extra


type alias Occurances a =
    { s : (SquareOp -> a) -> SquareOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
    , r : (RectOp -> a) -> RectOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
    , t : (TriOp -> a) -> TriOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
    , f : (FrameOp -> a) -> FrameOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
    , l : (LOp -> a) -> LOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
    }


type LinEq
    = Solved { a : Float, b : Float }
    | Unsolved Int


type Diff a
    = NoDiff
    | Diff a


type alias Repeat a =
    { ab : LinEq, repeat : a, tail : a }


type alias RepeatConv a =
    { s : Repeat SquareOp -> Maybe (Repeat a)
    , r : Repeat RectOp -> Maybe (Repeat a)
    , t : Repeat TriOp -> Maybe (Repeat a)
    , f : Repeat FrameOp -> Maybe (Repeat a)
    , l : Repeat LOp -> Maybe (Repeat a)
    }


type alias ConvFunc a =
    { s : (SquareOp -> a) -> Maybe (a -> a)
    , r : (RectOp -> a) -> Maybe (a -> a)
    , t : (TriOp -> a) -> Maybe (a -> a)
    , f : (FrameOp -> a) -> Maybe (a -> a)
    , l : (LOp -> a) -> Maybe (a -> a)
    }


type SquareOp
    = LCutS LOp SquareOp
    | SplitInnerSquare FrameOp SquareOp
    | SplitOuterFrame FrameOp SquareOp
    | Split4 SquareOp SquareOp SquareOp SquareOp
    | SplitDiaS TriOp TriOp
    | Square Int
    | RecurseS
    | RepeatS { ab : LinEq, repeat : SquareOp, tail : SquareOp }
    | NextS


type RectOp
    = SplitSquare SquareOp RectOp
    | SplitDiaR TriOp TriOp
    | ToSquare SquareOp
    | Rotate RectOp
    | Rect Int Int
    | RecurseR
    | RepeatR { ab : LinEq, repeat : RectOp, tail : RectOp }
    | NextR


type FrameOp
    = SplitFrame RectOp RectOp RectOp RectOp
    | Frame Int Int
    | RecurseF
    | RepeatF { ab : LinEq, repeat : FrameOp, tail : FrameOp }
    | NextF


type TriOp
    = SplitTST TriOp SquareOp TriOp
    | LCutT LOp TriOp
    | SplitSide RectOp TriOp
    | Tri Int
    | RecurseT
    | RepeatT { ab : LinEq, repeat : TriOp, tail : TriOp }
    | NextT


type LOp
    = SplitEnds RectOp LOp
    | L Int
    | RecurseL
    | RepeatL { ab : LinEq, repeat : LOp, tail : LOp }
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
    = SRepeater { a : Float, b : Float, repeat : SquareOp, tail : SquareOp, i : Int }
    | RRepeater { a : Float, b : Float, repeat : RectOp, tail : RectOp, i : Int }
    | FRepeater { a : Float, b : Float, repeat : FrameOp, tail : FrameOp, i : Int }
    | TRepeater { a : Float, b : Float, repeat : TriOp, tail : TriOp, i : Int }
    | LRepeater { a : Float, b : Float, repeat : LOp, tail : LOp, i : Int }


type Stage
    = Proving { operation : String }
    | Initialising
        { n : String
        , shape : String
        , p1 : String
        , p2 : String
        }
        (List { n : String, shape : String, p1 : String, p2 : String })
    | DoneI
        { n : String
        , shape : String
        , p1 : String
        , p2 : String
        }
    | Done (Maybe ProofTree)


type alias Model =
    { proofs : List ( Int, ProofTree )
    , stage : Stage
    }

type Msg
    = SetN (Maybe Int) String
    | SetShape (Maybe Int) String
    | SetP1 (Maybe Int) String
    | SetP2 (Maybe Int) String
    | Next
    | SetOp String
    | UpdateProof ProofTree
    | AddShape
    | Delete Int
    | SearchProof


radius : Int
radius =
    5


distance : Int
distance =
    20


mapDiff f d =
    case d of
        Diff p ->
            Diff (f p)

        NoDiff ->
            NoDiff


unwrapS p =
    case p of
        SOp s ->
            Just s

        _ ->
            Nothing


unwrapR p =
    case p of
        ROp r ->
            Just r

        _ ->
            Nothing


unwrapT p =
    case p of
        TOp t ->
            Just t

        _ ->
            Nothing

unwrapF p =
    case p of
        FOp f ->
            Just f

        _ ->
            Nothing


unwrapL p =
    case p of
        LOp l ->
            Just l

        _ ->
            Nothing


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
                []
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
        RepeatS { ab, repeat, tail } ->
            case ab of
                Solved {a, b} ->
                    buildRepeatS (Just (SRepeater { a = a, b = b, repeat = repeat, tail = tail, i = round (a*toFloat(n)+b) })) n repeat

                Unsolved _ ->
                    Just sq

        NextS ->
            case repeater of
                Just (SRepeater { a, b, repeat, tail, i }) ->
                    if i <= 0 then
                        Nothing
                    else
                        if i == 1 then
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
        RepeatR { ab, repeat, tail } ->
            case ab of
                Solved {a, b} ->
                    buildRepeatR (Just (RRepeater { a = a, b = b, repeat = repeat, tail = tail, i = round (a*toFloat(n)+b) })) n repeat

                Unsolved _ ->
                    Just re

        NextR ->
            case repeater of
                Just (RRepeater { a, b, repeat, tail, i }) ->
                        if i <= 0 then
                            Nothing

                        else
                            if i == 1 then
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
        RepeatT { ab, repeat, tail } ->
            case ab of
                Solved {a, b} ->
                    buildRepeatT (Just (TRepeater { a = a, b = b, repeat = repeat, tail = tail, i = round (a*toFloat(n)+b) })) n repeat

                Unsolved _ ->
                    Just tr

        NextT ->
            case repeater of
                Just (TRepeater { a, b, repeat, tail, i }) ->
                    if i <= 0 then
                        Nothing
                    else
                        if i == 1 then
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
        RepeatF { ab, repeat, tail } ->
            case ab of
                Solved {a, b} ->
                    buildRepeatF (Just (FRepeater { a = a, b = b, repeat = repeat, tail = tail, i = round(a*toFloat(n)+b) })) n repeat

                Unsolved _ ->
                    Just fr

        NextF ->
            case repeater of
                Just (FRepeater { a, b, repeat, tail, i }) ->
                    if i <= 0 then
                        Nothing
                    else
                        if i == 1 then
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
        RepeatL { ab, repeat, tail } ->
            case ab of
                Solved {a, b} ->
                    buildRepeatL (Just (LRepeater { a = a, b = b, repeat = repeat, tail = tail, i = round(a*toFloat(n)+b) })) n repeat

                Unsolved _ ->
                    Just ll

        NextL ->
            case repeater of
                Just (LRepeater { a, b, repeat, tail, i }) ->
                    if i <= 0 then
                        Nothing
                    else
                        if i == 1 then
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


findPossibleRepeatPointsS : ProofTree -> SquareOp -> List (Maybe (ProofTree, SquareOp -> ProofTree)) -> ProofTree -> (SquareOp -> ProofTree) -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPointsS pt sq ps base rest =
    case sq of
        LCutS l s ->
            let
                matches =
                    case pt of
                        SOp (LCutS _ _) ->
                            Just (rest RecurseS, SOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    LCutS x s

                branch2 =
                    LCutS l
            in
            findPossibleRepeatPointsL pt l (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsS pt s (branchps branch2) base (rest << branch2))

        SplitInnerSquare f s ->
            let
                matches =
                    case pt of
                        SOp (SplitInnerSquare _ _) ->
                            Just (rest RecurseS, SOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitInnerSquare x s

                branch2 =
                    SplitInnerSquare f
            in
            findPossibleRepeatPointsF pt f (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsS pt s (branchps branch2) base (rest << branch2))

        SplitOuterFrame f s ->
            let
                matches =
                    case pt of
                        SOp (SplitOuterFrame _ _) ->
                            Just (rest RecurseS, SOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitOuterFrame x s

                branch2 =
                    SplitOuterFrame f
            in
            findPossibleRepeatPointsF pt f (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsS pt s (branchps branch2) base (rest << branch2))

        SplitDiaS t1 t2 ->
            let
                matches =
                    case pt of
                        SOp (SplitDiaS _ _) ->
                            Just (rest RecurseS, SOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitDiaS x t2

                branch2 =
                    SplitDiaS t1
            in
            findPossibleRepeatPointsT pt t1 (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsT pt t2 (branchps branch2) base (rest << branch2))

        Split4 s1 s2 s3 s4 ->
            let
                matches =
                    case pt of
                        SOp (Split4 _ _ _ _) ->
                            Just (rest RecurseS, SOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    Split4 x s2 s3 s4

                branch2 x =
                    Split4 s1 x s3 s4

                branch3 x =
                    Split4 s1 s2 x s4

                branch4 =
                    Split4 s1 s2 s3
            in
            findPossibleRepeatPointsS pt s1 (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsS pt s2 (branchps branch2) base (rest << branch2))
                |> ifFailed (findPossibleRepeatPointsS pt s3 (branchps branch3) base (rest << branch3))
                |> ifFailed (findPossibleRepeatPointsS pt s4 (branchps branch4) base (rest << branch4))

        Square _ ->
            Nothing

        NextS ->
            Nothing

        RepeatS _ ->
            Nothing

        RecurseS ->
            case base of
                SOp s ->
                    Just (List.map (Maybe.map (Tuple.mapSecond ((|>) s))) ((Just (rest RecurseS, SOp))::ps))

                _ ->
                    Nothing


findPossibleRepeatPointsR : ProofTree -> RectOp -> List (Maybe (ProofTree, RectOp -> ProofTree)) -> ProofTree -> (RectOp -> ProofTree) -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPointsR pt re ps base rest =
    case re of
        SplitDiaR t1 t2 ->
            let
                matches =
                    case pt of
                        ROp (SplitDiaR _ _) ->
                            Just (rest RecurseR, ROp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitDiaR x t2

                branch2 =
                    SplitDiaR t1
            in
            findPossibleRepeatPointsT pt t1 (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsT pt t2 (branchps branch2) base (rest << branch2))

        SplitSquare s r ->
            let
                matches =
                    case pt of
                        ROp (SplitSquare _ _) ->
                            Just (rest RecurseR, ROp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitSquare x r

                branch2 =
                    SplitSquare s
            in
            findPossibleRepeatPointsS pt s (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsR pt r (branchps branch2) base (rest << branch2))

        ToSquare s ->
            let
                matches =
                    case pt of
                        ROp (ToSquare _) ->
                            Just (rest RecurseR, ROp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))
            in
            findPossibleRepeatPointsS pt s (branchps ToSquare) base (rest << ToSquare)

        Rotate r ->
            let
                matches =
                    case pt of
                        ROp (Rotate _) ->
                            Just (rest RecurseR, ROp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))
            in
            findPossibleRepeatPointsR pt r (branchps Rotate) base (rest << Rotate)

        Rect _ _ ->
            Nothing

        NextR ->
            Nothing

        RepeatR _ ->
            Nothing

        RecurseR ->
            case base of
                ROp r ->
                    Just (List.map (Maybe.map (Tuple.mapSecond ((|>) r))) ((Just (rest RecurseR, ROp))::ps))

                _ ->
                    Nothing


findPossibleRepeatPointsT : ProofTree -> TriOp -> List (Maybe (ProofTree, TriOp -> ProofTree)) -> ProofTree -> (TriOp -> ProofTree) -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPointsT pt tr ps base rest =
    case tr of
        LCutT l t ->
            let
                matches =
                    case pt of
                        TOp (LCutT _ _) ->
                            Just (rest RecurseT, TOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    LCutT x t

                branch2 =
                    LCutT l
            in
            findPossibleRepeatPointsL pt l (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsT pt t (branchps branch2) base (rest << branch2))

        SplitSide r t ->
            let
                matches =
                    case pt of
                        TOp (SplitSide _ _) ->
                            Just (rest RecurseT, TOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitSide x t

                branch2 =
                    SplitSide r
            in
            findPossibleRepeatPointsR pt r (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsT pt t (branchps branch2) base (rest << branch2))

        SplitTST t1 s t2 ->
            let
                matches =
                    case pt of
                        TOp (SplitTST _ _ _) ->
                            Just (rest RecurseT, TOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitTST x s t2

                branch2 x =
                    SplitTST t1 x t2

                branch3 =
                    SplitTST t1 s
            in
            findPossibleRepeatPointsT pt t1 (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsS pt s (branchps branch2) base (rest << branch2))
                |> ifFailed (findPossibleRepeatPointsT pt t2 (branchps branch3) base (rest << branch3))


        Tri _ ->
            Nothing

        NextT ->
            Nothing

        RepeatT _ ->
            Nothing

        RecurseT ->
            case base of
                TOp t ->
                    Just (List.map (Maybe.map (Tuple.mapSecond ((|>) t))) ((Just (rest RecurseT, TOp))::ps))

                _ ->
                    Nothing


findPossibleRepeatPointsF : ProofTree -> FrameOp -> List (Maybe (ProofTree, FrameOp -> ProofTree)) -> ProofTree -> (FrameOp -> ProofTree) -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPointsF pt fr ps base rest =
    case fr of
        SplitFrame r1 r2 r3 r4 ->
            let
                matches =
                    case pt of
                        FOp (SplitFrame _ _ _ _) ->
                            Just (rest RecurseF, FOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitFrame x r2 r3 r4

                branch2 x =
                    SplitFrame r1 x r3 r4

                branch3 x =
                    SplitFrame r1 r2 x r4

                branch4 =
                    SplitFrame r1 r2 r3
            in
            findPossibleRepeatPointsR pt r1 (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsR pt r2 (branchps branch2) base (rest << branch2))
                |> ifFailed (findPossibleRepeatPointsR pt r3 (branchps branch3) base (rest << branch3))
                |> ifFailed (findPossibleRepeatPointsR pt r4 (branchps branch4) base (rest << branch4))

        Frame _ _ ->
            Nothing

        NextF ->
            Nothing

        RepeatF _ ->
            Nothing

        RecurseF ->
            case base of
                FOp f ->
                    Just (List.map (Maybe.map (Tuple.mapSecond ((|>) f))) ((Just (rest RecurseF, FOp))::ps))

                _ ->
                    Nothing


findPossibleRepeatPointsL : ProofTree -> LOp -> List (Maybe (ProofTree, LOp -> ProofTree)) -> ProofTree -> (LOp -> ProofTree) -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPointsL pt ll ps base rest =
    case ll of
        SplitEnds r l ->
            let
                matches =
                    case pt of
                        LOp (SplitEnds _ _) ->
                            Just (rest RecurseL, LOp)

                        _ ->
                            Nothing

                branchps x =
                    matches::ps
                        |> List.map (Maybe.map (Tuple.mapSecond ((>>) x)))

                branch1 x =
                    SplitEnds x l

                branch2 =
                    SplitEnds r
            in
            findPossibleRepeatPointsR pt r (branchps branch1) base (rest << branch1)
                |> ifFailed (findPossibleRepeatPointsL pt l (branchps branch2) base (rest << branch2))

        L _ ->
            Nothing

        NextL ->
            Nothing

        RepeatL _ ->
            Nothing

        RecurseL ->
            case base of
                LOp l ->
                    Just (List.map (Maybe.map (Tuple.mapSecond ((|>) l))) ((Just (rest RecurseL, LOp))::ps))

                _ ->
                    Nothing


findPossibleRepeatPoints : ProofTree -> ProofTree -> Maybe (List (Maybe (ProofTree, ProofTree)))
findPossibleRepeatPoints base pt =
    case pt of
        SOp s ->
            findPossibleRepeatPointsS pt s [] base SOp
                |> Maybe.map List.reverse

        ROp r ->
            findPossibleRepeatPointsR pt r [] base ROp
                |> Maybe.map List.reverse

        TOp t ->
            findPossibleRepeatPointsT pt t [] base TOp
                |> Maybe.map List.reverse

        FOp f ->
            findPossibleRepeatPointsF pt f [] base FOp
                |> Maybe.map List.reverse

        LOp l ->
            findPossibleRepeatPointsL pt l [] base LOp
                |> Maybe.map List.reverse


applyN n f b =
    if n == 0 then
        b
    else
        f (applyN (n-1) f b)


repeatFuncS : (SquareOp -> a) -> SquareOp -> ConvFunc a -> Maybe (a -> a)
repeatFuncS rest ss conv =
    case ss of
        LCutS l s ->
            repeatFuncL (rest << \x -> LCutS x s) l conv
                |> ifFailed (repeatFuncS (rest << LCutS l) s conv)

        SplitInnerSquare f s ->
            repeatFuncF (rest << \x -> SplitInnerSquare x s) f conv
                |> ifFailed (repeatFuncS (rest << SplitInnerSquare f) s conv)

        SplitOuterFrame f s ->
            repeatFuncF (rest << \x -> SplitOuterFrame x s) f conv
                |> ifFailed (repeatFuncS (rest << SplitOuterFrame f) s conv)

        Split4 s1 s2 s3 s4 ->
            repeatFuncS (rest << \x -> Split4 x s2 s3 s4) s1 conv
                |> ifFailed (repeatFuncS (rest << \x -> Split4 s1 x s3 s4) s2 conv)
                |> ifFailed (repeatFuncS (rest << \x -> Split4 s1 s2 x s4) s3 conv)
                |> ifFailed (repeatFuncS (rest << Split4 s1 s2 s3) s4 conv)

        SplitDiaS t1 t2 ->
            repeatFuncT (rest << \x -> SplitDiaS x t2) t1 conv
                |> ifFailed (repeatFuncT (rest << SplitDiaS t1) t2 conv)

        NextS ->
            conv.s rest

        _ ->
            Nothing


repeatFuncR : (RectOp -> a) -> RectOp -> ConvFunc a -> Maybe (a -> a)
repeatFuncR rest re conv =
    case re of
        SplitSquare s r ->
            repeatFuncS (rest << \x -> SplitSquare x r) s conv
                |> ifFailed (repeatFuncR (rest << SplitSquare s) r conv)

        SplitDiaR t1 t2 ->
            repeatFuncT (rest << \x -> SplitDiaR x t2) t1 conv
                |> ifFailed (repeatFuncT (rest << SplitDiaR t1) t2 conv)

        ToSquare s ->
            repeatFuncS (rest << ToSquare) s conv

        Rotate r ->
            repeatFuncR (rest << Rotate) r conv

        NextR ->
            conv.r rest

        _ ->
            Nothing


repeatFuncF : (FrameOp -> a) -> FrameOp -> ConvFunc a -> Maybe (a -> a)
repeatFuncF rest fr conv =
    case fr of
        SplitFrame r1 r2 r3 r4 ->
            repeatFuncR (rest << \x -> SplitFrame x r2 r3 r4) r1 conv
                |> ifFailed (repeatFuncR (rest << \x -> SplitFrame r1 x r3 r4) r2 conv)
                |> ifFailed (repeatFuncR (rest << \x -> SplitFrame r1 r2 x r4) r3 conv)
                |> ifFailed (repeatFuncR (rest << SplitFrame r1 r2 r3) r4 conv)

        NextF ->
            conv.f rest

        _ ->
            Nothing


repeatFuncT : (TriOp -> a) -> TriOp -> ConvFunc a -> Maybe (a -> a)
repeatFuncT rest tr conv =
    case tr of
        SplitTST t1 s t2 ->
            repeatFuncT (rest << \x -> SplitTST x s t2) t1 conv
                |> ifFailed (repeatFuncS (rest << \x -> SplitTST t1 x t2) s conv)
                |> ifFailed(repeatFuncT (rest << SplitTST t1 s) t2 conv)

        LCutT l t ->
            repeatFuncL (rest << \x -> LCutT x t) l conv
                |> ifFailed (repeatFuncT (rest << LCutT l) t conv)

        SplitSide r t ->
            repeatFuncR (rest << \x -> SplitSide x t) r conv
                |> ifFailed (repeatFuncT (rest << SplitSide r) t conv)

        NextT ->
            conv.t rest

        _ ->
            Nothing


repeatFuncL : (LOp -> a) -> LOp -> ConvFunc a -> Maybe (a -> a)
repeatFuncL rest ll conv =
    case ll of
        SplitEnds r l ->
            repeatFuncR (rest << \x -> SplitEnds x l) r conv
                |> ifFailed (repeatFuncL (rest << SplitEnds r) l conv)

        NextL ->
            conv.l rest

        _ ->
            Nothing


inferFunction : ProofTree -> Int -> ProofTree -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunction large largeN small smallN conv =
    case (large, small) of
        (SOp s1, SOp s2) ->
            inferFunctionS s1 largeN s2 smallN conv

        (ROp r1, ROp r2) ->
            inferFunctionR r1 largeN r2 smallN conv

        (TOp t1, TOp t2) ->
            inferFunctionT t1 largeN t2 smallN conv

        (FOp f1, FOp f2) ->
            inferFunctionF f1 largeN f2 smallN conv

        (LOp l1, LOp l2) ->
            inferFunctionL l1 largeN l2 smallN conv

        _ ->
            Nothing


inferFunctionS : SquareOp -> Int -> SquareOp -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunctionS sq1 largeN sq2 smallN conv =
    case (sq1, sq2) of
        (LCutS l1 s1, LCutS l2 s2) ->
            inferFunctionL l1 largeN l2 smallN conv
                |> ifFailed (inferFunctionS s1 largeN s2 smallN conv)

        (SplitInnerSquare f1 s1, SplitInnerSquare f2 s2) ->
            inferFunctionF f1 largeN f2 smallN conv
                |> ifFailed (inferFunctionS s1 largeN s2 smallN conv)

        (SplitOuterFrame f1 s1, SplitOuterFrame f2 s2) ->
            inferFunctionF f1 largeN f2 smallN conv
                |> ifFailed (inferFunctionS s1 largeN s2 smallN conv)

        (Split4 s11 s12 s13 s14, Split4 s21 s22 s23 s24) ->
            inferFunctionS s11 largeN s21 smallN conv
                |> ifFailed (inferFunctionS s12 largeN s22 smallN conv)
                |> ifFailed (inferFunctionS s13 largeN s23 smallN conv)
                |> ifFailed (inferFunctionS s14 largeN s24 smallN conv)

        (RepeatS { ab, repeat, tail }, ss) ->
            case ab of
                Solved { a, b } ->
                    buildRepeatS Nothing smallN (RepeatS {ab = ab, repeat = repeat, tail = tail })
                        |> Maybe.andThen (\x -> inferFunctionS x largeN ss smallN conv)

                Unsolved n ->
                    case repeatFuncS (\x -> x) repeat { s = Just, r = \_ -> Nothing, t = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing } of
                        Just func ->
                            let
                                maxApplys m =
                                    case findLowerDiffS (applyN m func NextS) sq2 of
                                        Just _ ->
                                            maxApplys (m+1)

                                        Nothing ->
                                            m - 1

                                repeatNum =
                                    maxApplys 1

                                a =
                                    toFloat(n-repeatNum)/toFloat(largeN-smallN)
                            in
                            conv.s ({ ab = Solved { a = a, b = toFloat(n) - toFloat(largeN)*a }, repeat = repeat, tail = tail })

                        Nothing ->
                            Nothing

        _ ->
            Nothing


inferFunctionR : RectOp -> Int -> RectOp -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunctionR re1 largeN re2 smallN conv =
    case (re1, re2) of
        (SplitSquare s1 r1, SplitSquare s2 r2) ->
            inferFunctionS s1 largeN s2 smallN conv
                |> ifFailed (inferFunctionR r1 largeN r2 smallN conv)

        (SplitDiaR t11 t12, SplitDiaR t21 t22) ->
            inferFunctionT t11 largeN t21 smallN conv
                |> ifFailed (inferFunctionT t12 largeN t22 smallN conv)

        (ToSquare s1, ToSquare s2) ->
            inferFunctionS s1 largeN s2 smallN conv

        (Rotate r1, Rotate r2) ->
            inferFunctionR r1 largeN r2 smallN conv

        (RepeatR { ab, repeat, tail } , re) ->
            case ab of
                Solved { a, b } ->
                    buildRepeatR Nothing smallN (RepeatR { ab = ab, repeat = repeat, tail = tail })
                        |> Maybe.andThen (\x -> inferFunctionR x largeN re smallN conv)

                Unsolved n ->
                    case repeatFuncR (\x -> x) repeat { s = \_ -> Nothing, r = Just, t = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing } of
                        Just func ->
                            let
                                maxApplys m =
                                    case findLowerDiffR (applyN m func NextR) re2 of
                                        Just _ ->
                                            maxApplys (m+1)

                                        Nothing ->
                                            m - 1

                                repeatNum =
                                    maxApplys 1

                                a =
                                    toFloat(n-repeatNum)/toFloat(largeN-smallN)
                            in
                            conv.r { ab = Solved { a = a, b = toFloat(n) - toFloat(largeN)*a }, repeat = repeat, tail = tail }

                        Nothing ->
                            Nothing

        _ ->
            Nothing


inferFunctionF : FrameOp -> Int -> FrameOp -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunctionF fr1 largeN fr2 smallN conv =
    case (fr1, fr2) of
        (SplitFrame r11 r12 r13 r14, SplitFrame r21 r22 r23 r24) ->
            inferFunctionR r11 largeN r21 smallN conv
                |> ifFailed (inferFunctionR r12 largeN r22 smallN conv)
                |> ifFailed (inferFunctionR r13 largeN r23 smallN conv)
                |> ifFailed (inferFunctionR r14 largeN r24 smallN conv)

        (RepeatF { ab, repeat, tail }, fr) ->
            case ab of
                Solved { a, b } ->
                    buildRepeatF Nothing smallN (RepeatF { ab = ab, repeat = repeat, tail = tail })
                        |> Maybe.andThen (\x -> inferFunctionF x largeN fr smallN conv)

                Unsolved n ->
                    case repeatFuncF (\x -> x) repeat { s = \_ -> Nothing, r = \_ -> Nothing, f = Just, l = \_ -> Nothing, t = \_ -> Nothing } of
                        Just func ->
                            let
                                maxApplys m =
                                    case findLowerDiffF (applyN m func NextF) fr2 of
                                        Just _ ->
                                            maxApplys (m+1)

                                        Nothing ->
                                            m - 1

                                repeatNum =
                                    maxApplys 1

                                a =
                                    toFloat(n-repeatNum)/toFloat(largeN-smallN)
                            in
                            conv.f { ab = Solved { a = a, b = toFloat(n) - toFloat(largeN)*a }, repeat = repeat, tail = tail }

                        Nothing ->
                            Nothing

        _ ->
            Nothing


inferFunctionT : TriOp -> Int -> TriOp -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunctionT tr1 largeN tr2 smallN conv =
    case (tr1, tr2) of
        (SplitTST t11 s1 t12, SplitTST t21 s2 t22) ->
            inferFunctionT t11 largeN t21 smallN conv
                |> ifFailed (inferFunctionS s1 largeN s2 smallN conv)
                |> ifFailed (inferFunctionT t12 largeN t22 smallN conv)

        (LCutT l1 t1, LCutT l2 t2) ->
            inferFunctionL l1 largeN l2 smallN conv
                |> ifFailed (inferFunctionT t1 largeN t2 smallN conv)

        (SplitSide r1 t1, SplitSide r2 t2) ->
            inferFunctionR r1 largeN r2 smallN conv
                |> ifFailed (inferFunctionT t1 largeN t2 smallN conv)

        (RepeatT { ab, repeat, tail }, tr) ->
            case ab of
                Solved { a, b } ->
                    buildRepeatT Nothing smallN (RepeatT { ab = ab, repeat = repeat, tail = tail })
                        |> Maybe.andThen (\x -> inferFunctionT x largeN tr smallN conv)

                Unsolved n ->
                    case repeatFuncT (\x -> x) repeat { s = \_ -> Nothing, r = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing, t = Just } of
                        Just func ->
                            let
                                maxApplys m =
                                    case findLowerDiffT (applyN m func NextT) tr2 of
                                        Just _ ->
                                            maxApplys (m+1)

                                        Nothing ->
                                            m - 1

                                repeatNum =
                                    maxApplys 1

                                a =
                                    toFloat(n-repeatNum)/toFloat(largeN-smallN)
                            in
                            conv.t { ab = Solved { a = a, b = toFloat(n) - toFloat(largeN)*a }, repeat = repeat, tail = tail }

                        Nothing ->
                            Nothing

        _ ->
            Nothing


inferFunctionL : LOp -> Int -> LOp -> Int -> RepeatConv a -> Maybe (Repeat a)
inferFunctionL ll1 largeN ll2 smallN conv =
    case (ll1, ll2) of
        (SplitEnds r1 l1, SplitEnds r2 l2) ->
            inferFunctionR r1 largeN r2 smallN conv
                |> ifFailed (inferFunctionL l1 largeN l2 smallN conv)

        (RepeatL { ab, repeat, tail }, ll) ->
            case ab of
                Solved { a, b } ->
                    buildRepeatL Nothing smallN (RepeatL { ab = ab, repeat = repeat, tail = tail })
                        |> Maybe.andThen (\x -> inferFunctionL x largeN ll smallN conv)

                Unsolved n ->
                    case repeatFuncL (\x -> x) repeat { s = \_ -> Nothing, r = \_ -> Nothing, f = \_ -> Nothing, l = Just, t = \_ -> Nothing } of
                        Just func ->
                            let
                                maxApplys m =
                                    case findLowerDiffL (applyN m func NextL) ll2 of
                                        Just _ ->
                                            maxApplys (m+1)

                                        Nothing ->
                                            m - 1

                                repeatNum =
                                    maxApplys 1

                                a =
                                    toFloat(n-repeatNum)/toFloat(largeN-smallN)
                            in
                            conv.l { ab = Solved { a = a, b = toFloat(n) - toFloat(largeN)*a }, repeat = repeat, tail = tail }

                        Nothing ->
                            Nothing

        _ ->
            Nothing



countOccurances : RepeatConv a -> (a -> a -> Maybe (Diff a)) -> a -> a -> (Repeat a -> a) -> (a -> a) -> a -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Maybe (Repeat a)
countOccurances conv lowerDiffFunc defaultVal next repeat trial ss rest largeN small smallN =
    if trial next == next then
        Nothing
    else
        let
            maxApplys n =
                case lowerDiffFunc (applyN n trial next) ss of
                    Just (Diff diff) ->
                        Just (Maybe.withDefault (n, diff) (maxApplys (n+1)))

                    Just NoDiff ->
                        Just (n, defaultVal)

                    Nothing ->
                        Nothing
        in
        case maxApplys 1 of
            Just (n, tail) ->
                inferFunction (rest (repeat { ab = Unsolved (n+1), repeat = trial next, tail = tail })) largeN small smallN conv

            Nothing ->
                Nothing


findRepeatS : (SquareOp -> a) -> SquareOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Occurances a -> Maybe (Repeat a)
findRepeatS trial ss rest largeN small smallN occ =
    case occ.s trial ss rest largeN small smallN of
        Just s ->
            Just s

        Nothing ->
            case ss of
                LCutS l s ->
                    findRepeatL (trial << \x -> LCutS x s) l rest largeN small smallN occ
                        |> ifFailed (findRepeatS (trial << LCutS l) s rest largeN small smallN occ)

                SplitInnerSquare f s ->
                    findRepeatF (trial << \x -> SplitInnerSquare x s) f rest largeN small smallN occ
                        |> ifFailed (findRepeatS (trial << SplitInnerSquare f) s rest largeN small smallN occ)

                SplitOuterFrame f s ->
                    findRepeatF (trial << \x -> SplitOuterFrame x s) f rest largeN small smallN occ
                        |> ifFailed (findRepeatS (trial << SplitOuterFrame f) s rest largeN small smallN occ)

                Split4 s1 s2 s3 s4 ->
                    findRepeatS (trial << \x -> Split4 x s2 s3 s4) s1 rest largeN small smallN occ
                        |> ifFailed (findRepeatS (trial << \x -> Split4 s1 x s3 s4) s2 rest largeN small smallN occ)
                        |> ifFailed (findRepeatS (trial << \x -> Split4 s1 s2 x s4) s3 rest largeN small smallN occ)
                        |> ifFailed (findRepeatS (trial << Split4 s1 s2 s3) s4 rest largeN small smallN occ)

                SplitDiaS t1 t2 ->
                    findRepeatT (trial << \x -> SplitDiaS x t2) t1 rest largeN small smallN occ
                        |> ifFailed (findRepeatT (trial << SplitDiaS t1) t2 rest largeN small smallN occ)

                _ ->
                    Nothing


findRepeatR : (RectOp -> a) -> RectOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Occurances a -> Maybe (Repeat a)
findRepeatR trial re rest largeN small smallN occ =
    case occ.r trial re rest largeN small smallN of
        Just r ->
            Just r

        Nothing ->
            case re of
                SplitSquare s r ->
                    findRepeatS (trial << \x -> SplitSquare x r) s rest largeN small smallN occ
                        |> ifFailed (findRepeatR (trial << SplitSquare s) r rest largeN small smallN occ)

                SplitDiaR t1 t2 ->
                    findRepeatT (trial << \x -> SplitDiaR x t2) t1 rest largeN small smallN occ
                        |> ifFailed (findRepeatT (trial << SplitDiaR t1) t2 rest largeN small smallN occ)

                ToSquare s ->
                    findRepeatS (trial << ToSquare) s rest largeN small smallN occ

                Rotate r ->
                    findRepeatR (trial << Rotate) r rest largeN small smallN occ

                _ ->
                    Nothing


findRepeatF : (FrameOp -> a) -> FrameOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Occurances a -> Maybe (Repeat a)
findRepeatF trial fr rest largeN small smallN occ =
    case occ.f trial fr rest largeN small smallN of
        Just f ->
            Just f

        Nothing ->
            case fr of
                SplitFrame r1 r2 r3 r4 ->
                    findRepeatR (trial << \x -> SplitFrame x r2 r3 r4) r1 rest largeN small smallN occ
                        |> ifFailed (findRepeatR (trial << \x -> SplitFrame r1 x r3 r4) r2 rest largeN small smallN occ)
                        |> ifFailed (findRepeatR (trial << \x -> SplitFrame r1 r2 x r4) r3 rest largeN small smallN occ)
                        |> ifFailed (findRepeatR (trial << SplitFrame r1 r3 r3) r4 rest largeN small smallN occ)

                _ ->
                    Nothing


findRepeatT : (TriOp -> a) -> TriOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Occurances a -> Maybe (Repeat a)
findRepeatT trial tr rest largeN small smallN occ =
    case occ.t trial tr rest largeN small smallN of
        Just t ->
            Just t

        Nothing ->
            case tr of
                SplitTST t1 s t2 ->
                    findRepeatT (trial << \x -> SplitTST x s t2) t1 rest largeN small smallN occ
                        |> ifFailed (findRepeatS (trial << \x -> SplitTST t1 x t2) s rest largeN small smallN occ)
                        |> ifFailed (findRepeatT (trial << SplitTST t1 s) t2 rest largeN small smallN occ)

                LCutT l t ->
                    findRepeatL (trial << \x -> LCutT x t) l rest largeN small smallN occ
                        |> ifFailed (findRepeatT (trial << LCutT l) t rest largeN small smallN occ)

                SplitSide r t ->
                    findRepeatR (trial << \x -> SplitSide x t) r rest largeN small smallN occ
                        |> ifFailed (findRepeatT (trial << SplitSide r) t rest largeN small smallN occ)

                _ ->
                    Nothing


findRepeatL : (LOp -> a) -> LOp -> (a -> ProofTree) -> Int -> ProofTree -> Int -> Occurances a -> Maybe (Repeat a)
findRepeatL trial ll rest largeN small smallN occ =
    case occ.l trial ll rest largeN small smallN of
        Just l ->
            Just l

        Nothing ->
            case ll of
                SplitEnds r l ->
                    findRepeatR (trial << \x -> SplitEnds x l) r rest largeN small smallN occ
                        |> ifFailed (findRepeatL (trial << SplitEnds r) l rest largeN small smallN occ)

                _ ->
                    Nothing


aN _ _ _ _ _ _ =
    Nothing


du d f s1 s2 =
    case d s1 s2 of
        Just (Diff p) ->
            case f p of
                Just s ->
                    Just (Diff s)

                _ ->
                    Nothing

        Just (NoDiff) ->
            Just (NoDiff)

        Nothing ->
            Nothing


findLowerDiffS ss1 ss2 =
    case (ss1, ss2) of
        (LCutS l1 s1, LCutS l2 s2) ->
            case (findLowerDiffL l1 l2, findLowerDiffS s1 s2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (SplitInnerSquare f1 s1, SplitInnerSquare f2 s2) ->
            case (findLowerDiffF f1 f2, findLowerDiffS s1 s2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (SplitOuterFrame f1 s1, SplitOuterFrame f2 s2) ->
            case (findLowerDiffF f1 f2, findLowerDiffS s1 s2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (Split4 s11 s12 s13 s14, Split4 s21 s22 s23 s24) ->
            case (findLowerDiffS s11 s21, (findLowerDiffS s12 s22, findLowerDiffS s13 s23, findLowerDiffS s14 s24)) of
                (Just (Diff d), (Just NoDiff, Just NoDiff, Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just (Diff d), Just NoDiff, Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just (Diff d), Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just NoDiff, Just (Diff d))) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just NoDiff, Just NoDiff)) ->
                    Just NoDiff

                _ ->
                    Nothing

        (SplitDiaS t11 t12, SplitDiaS t21 t22) ->
            case (findLowerDiffT t11 t21, findLowerDiffT t12 t22) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (Square _, Square _) ->
            Just NoDiff


        (NextS, s) ->
            Just (Diff (SOp s))

        (RecurseS, s) ->
            Just (Diff (SOp s))

        _ ->
            Nothing


findLowerDiffR re1 re2 =
    case (re1, re2) of
        (SplitSquare s1 r1, SplitSquare s2 r2) ->
            case (findLowerDiffS s1 s2, findLowerDiffR r1 r2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (SplitDiaR t11 t12, SplitDiaR t21 t22) ->
            case (findLowerDiffT t11 t21, findLowerDiffT t12 t22) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (ToSquare s1, ToSquare s2) ->
            findLowerDiffS s1 s2

        (Rotate r1, Rotate r2) ->
            findLowerDiffR r1 r2

        (Rect _ _, Rect _ _) ->
            Just NoDiff

        (NextR, r) ->
            Just (Diff (ROp r))

        (RecurseR, r) ->
            Just (Diff (ROp r))

        _ ->
            Nothing


findLowerDiffF fr1 fr2 =
    case (fr1, fr2) of
        (SplitFrame r11 r12 r13 r14, SplitFrame r21 r22 r23 r24) ->
            case (findLowerDiffR r11 r21, (findLowerDiffR r12 r22, findLowerDiffR r13 r23, findLowerDiffR r14 r24)) of
                (Just (Diff d), (Just NoDiff, Just NoDiff, Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just (Diff d), Just NoDiff, Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just (Diff d), Just NoDiff)) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just NoDiff, Just (Diff d))) ->
                    Just (Diff d)

                (Just NoDiff, (Just NoDiff, Just NoDiff, Just NoDiff)) ->
                    Just NoDiff

                _ ->
                    Nothing

        (Frame _ _, Frame _ _) ->
            Just NoDiff

        (NextF, f) ->
            Just (Diff (FOp f))

        _ ->
            Nothing


findLowerDiffT tr1 tr2 =
    case (tr1, tr2) of
        (SplitTST t11 s1 t12, SplitTST t21 s2 t22) ->
            case (findLowerDiffT t11 t21, findLowerDiffS s1 s2, findLowerDiffT t12 t22) of
                (Just (Diff d), Just NoDiff, Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (LCutT l1 t1, LCutT l2 t2) ->
            case (findLowerDiffL l1 l2, findLowerDiffT t1 t2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (SplitSide r1 t1, SplitSide r2 t2) ->
            case (findLowerDiffR r1 r2, findLowerDiffT t1 t2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (Tri _, Tri _) ->
            Just NoDiff

        (NextT, t) ->
            Just (Diff (TOp t))

        (RecurseT, t) ->
            Just (Diff (TOp t))

        _ ->
            Nothing


findLowerDiffL ll1 ll2 =
    case (ll1, ll2) of
        (SplitEnds r1 l1, SplitEnds r2 l2) ->
            case (findLowerDiffR r1 r2, findLowerDiffL l1 l2) of
                (Just (Diff d), Just NoDiff) ->
                    Just (Diff d)

                (Just NoDiff, Just (Diff d)) ->
                    Just (Diff d)

                (Just NoDiff, Just NoDiff) ->
                    Just NoDiff

                _ ->
                    Nothing

        (L _, L _) ->
            Just NoDiff

        (NextL, l) ->
            Just (Diff (LOp l))

        (RecurseL, l) ->
            Just (Diff (LOp l))

        _ ->
            Nothing


repeatSearchS : SquareOp -> Int -> ProofTree -> Int -> (SquareOp -> ProofTree) -> SquareOp
repeatSearchS ss largeN small smallN rest =
    case findRepeatS (\x -> x) ss rest largeN small smallN { s = countOccurances { s = Just, r = \_ -> Nothing, t = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing } (du findLowerDiffS unwrapS) (Square 0) NextS RepeatS, r = aN, t = aN, f = aN, l = aN } of
        Just repeat ->
            RepeatS { repeat | tail = repeatSearchS repeat.tail largeN small smallN (rest << \x -> RepeatS { repeat | tail = x }) }

        Nothing ->
            case ss of
                LCutS l s ->
                    let
                        lRepeat =
                            repeatSearchL l largeN small smallN (rest << \x -> LCutS x s)

                        sRepeat =
                            repeatSearchS s largeN small smallN (rest << LCutS lRepeat)
                    in
                    LCutS lRepeat sRepeat

                SplitInnerSquare f s ->
                    let
                        fRepeat =
                            repeatSearchF f largeN small smallN (rest << \x -> SplitInnerSquare x s)

                        sRepeat =
                            repeatSearchS s largeN small smallN (rest << SplitInnerSquare fRepeat)
                    in
                    SplitInnerSquare fRepeat sRepeat

                SplitOuterFrame f s ->
                    let
                        fRepeat =
                            repeatSearchF f largeN small smallN (rest << \x -> SplitOuterFrame x s)

                        sRepeat =
                            repeatSearchS s largeN small smallN (rest << SplitOuterFrame fRepeat)
                    in
                    SplitOuterFrame fRepeat sRepeat

                Split4 s1 s2 s3 s4 ->
                    let
                        s1Repeat =
                            repeatSearchS s1 largeN small smallN (rest << \x -> Split4 x s2 s3 s4)

                        s2Repeat =
                            repeatSearchS s2 largeN small smallN (rest << \x -> Split4 s1Repeat x s3 s4)

                        s3Repeat =
                            repeatSearchS s3 largeN small smallN (rest << \x -> Split4 s1Repeat s2Repeat x s4)

                        s4Repeat =
                            repeatSearchS s4 largeN small smallN (rest << Split4 s1Repeat s2Repeat s3Repeat)
                    in
                    Split4 s1Repeat s2Repeat s3Repeat s4Repeat

                SplitDiaS t1 t2 ->
                    let
                        t1Repeat =
                            repeatSearchT t1 largeN small smallN (rest << \x -> SplitDiaS x t2)

                        t2Repeat =
                            repeatSearchT t2 largeN small smallN (rest << SplitDiaS t1Repeat)
                    in
                    SplitDiaS t1Repeat t2Repeat

                x ->
                    x


repeatSearchR : RectOp -> Int -> ProofTree -> Int -> (RectOp -> ProofTree) -> RectOp
repeatSearchR re largeN small smallN rest =
    case findRepeatR (\x -> x) re rest largeN small smallN { s = aN, r = countOccurances { r = Just, s = \_ -> Nothing, t = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing } (du findLowerDiffR unwrapR) (Rect 0 0) NextR RepeatR, t = aN, f = aN, l = aN } of
        Just repeat ->
            RepeatR { repeat | tail = repeatSearchR repeat.tail largeN small smallN (rest << \x -> RepeatR { repeat | tail = x }) }

        Nothing ->
            case re of
                SplitSquare s r ->
                    let
                        sRepeat =
                            repeatSearchS s largeN small smallN (rest << \x -> SplitSquare x r)

                        rRepeat =
                            repeatSearchR r largeN small smallN (rest << SplitSquare sRepeat)
                    in
                    SplitSquare sRepeat rRepeat

                SplitDiaR t1 t2 ->
                    let
                        t1Repeat =
                            repeatSearchT t1 largeN small smallN (rest << \x -> SplitDiaR x t2)

                        t2Repeat =
                            repeatSearchT t2 largeN small smallN (rest << SplitDiaR t1Repeat)
                    in
                    SplitDiaR t1Repeat t2Repeat

                ToSquare s ->
                    ToSquare (repeatSearchS s largeN small smallN (rest << ToSquare))

                Rotate r ->
                    Rotate (repeatSearchR r largeN small smallN (rest << Rotate))

                x ->
                    x


repeatSearchF : FrameOp -> Int -> ProofTree -> Int -> (FrameOp -> ProofTree) -> FrameOp
repeatSearchF fr largeN small smallN rest  =
    case findRepeatF (\x -> x) fr rest largeN small smallN { s = aN, r = aN, t = aN, f = countOccurances { f = Just, s = \_ -> Nothing, r = \_ -> Nothing, t = \_ -> Nothing, l = \_ -> Nothing } (du findLowerDiffF unwrapF) (Frame 0 0) NextF RepeatF, l = aN } of
        Just repeat ->
            RepeatF { repeat | tail = repeatSearchF repeat.tail largeN small smallN (rest << \x -> RepeatF { repeat | tail = x }) }

        Nothing ->
            case fr of
                SplitFrame r1 r2 r3 r4 ->
                    let
                        r1Repeat =
                            repeatSearchR r1 largeN small smallN (rest << \x -> SplitFrame x r2 r3 r4)

                        r2Repeat =
                            repeatSearchR r2 largeN small smallN (rest << \x -> SplitFrame r1Repeat x r3 r4)

                        r3Repeat =
                            repeatSearchR r3 largeN small smallN (rest << \x -> SplitFrame r1Repeat r2Repeat x r4)

                        r4Repeat =
                            repeatSearchR r4 largeN small smallN (rest << SplitFrame r1Repeat r2Repeat r3Repeat)
                    in
                    SplitFrame r1Repeat r2Repeat r3Repeat r4Repeat

                x ->
                    x


repeatSearchT : TriOp -> Int -> ProofTree -> Int -> (TriOp -> ProofTree) -> TriOp
repeatSearchT tr largeN small smallN rest =
    case findRepeatT (\x -> x) tr rest largeN small smallN { s = aN, r = aN, t = countOccurances { s = \_ -> Nothing, r = \_ -> Nothing, f = \_ -> Nothing, l = \_ -> Nothing, t = Just } (du findLowerDiffT unwrapT) (Tri 0) NextT RepeatT, f = aN, l = aN } of
        Just repeat ->
            RepeatT { repeat | tail = repeatSearchT repeat.tail largeN small smallN (rest << \x -> RepeatT { repeat | tail = x }) }

        Nothing ->
            case tr of
                SplitTST t1 s t2 ->
                    let
                        t1Repeat =
                            repeatSearchT t1 largeN small smallN (rest << \x -> SplitTST x s t2)

                        sRepeat =
                            repeatSearchS s largeN small smallN (rest << \x -> SplitTST t1Repeat x t2)

                        t2Repeat =
                            repeatSearchT t2 largeN small smallN (rest << SplitTST t1Repeat sRepeat)
                    in
                    SplitTST t1Repeat sRepeat t2Repeat

                LCutT l t ->
                    let
                        lRepeat =
                            repeatSearchL l largeN small smallN (rest << \x -> LCutT x t)

                        tRepeat =
                            repeatSearchT t largeN small smallN (rest << LCutT lRepeat)
                    in
                    LCutT lRepeat tRepeat

                SplitSide r t ->
                    let
                        rRepeat =
                            repeatSearchR r largeN small smallN (rest << \x -> SplitSide x t)

                        tRepeat =
                            repeatSearchT t largeN small smallN (rest << SplitSide rRepeat)
                    in
                    SplitSide rRepeat tRepeat

                x ->
                    x


repeatSearchL : LOp -> Int -> ProofTree -> Int -> (LOp -> ProofTree) -> LOp
repeatSearchL ll largeN small smallN rest =
    case findRepeatL (\x -> x) ll rest largeN small smallN { s = aN, r = aN, t = aN, f = aN, l = countOccurances { l = Just, s = \_ -> Nothing, t = \_ -> Nothing, f = \_ -> Nothing, r = \_ -> Nothing } (du findLowerDiffL unwrapL) (L 0) NextL RepeatL } of
        Just repeat ->
            RepeatL { repeat | tail = repeatSearchL repeat.tail largeN small smallN (rest << \x -> RepeatL { repeat | tail = x}) }

        Nothing ->
            case ll of
                SplitEnds r l ->
                    let
                        rRepeat =
                            repeatSearchR r largeN small smallN (rest << \x -> SplitEnds x l)

                        lRepeat =
                            repeatSearchL l largeN small smallN (rest << SplitEnds rRepeat)
                    in
                    SplitEnds rRepeat lRepeat

                x ->
                    x


repeatSearch : Int -> ProofTree -> Int -> ProofTree -> ProofTree
repeatSearch smallN small largeN large =
    case large of
        SOp s ->
            SOp (repeatSearchS s largeN small smallN SOp)

        ROp r->
            ROp (repeatSearchR r largeN small smallN ROp)

        FOp f ->
            FOp (repeatSearchF f largeN small smallN FOp)

        TOp t ->
            TOp (repeatSearchT t largeN small smallN TOp)

        LOp l ->
            LOp (repeatSearchL l largeN small smallN LOp)


findBase : ProofTree -> Int -> ProofTree -> Maybe ProofTree
findBase step n base =
    case (buildRepeat n step, base) of
        (Just (SOp s1), SOp s2) ->
            case findLowerDiffS s1 s2 of
                Just (Diff p) ->
                    ifFailed (Just p) (findBase step (n-1) p)

                Just NoDiff ->
                    Just (SOp (Square 0))

                Nothing ->
                    Nothing

        (Just (ROp r1), ROp r2) ->
            case findLowerDiffR r1 r2 of
                Just (Diff p) ->
                    ifFailed (Just p) (findBase step (n-1) p)

                Just NoDiff ->
                    Just (ROp (Rect 0 0))

                Nothing ->
                    Nothing

        (Just (FOp f1), FOp f2) ->
            case findLowerDiffF f1 f2 of
                Just (Diff p) ->
                    ifFailed (Just p) (findBase step (n-1) p)

                Just NoDiff ->
                    Just (FOp (Frame 0 0))

                Nothing ->
                    Nothing

        (Just (TOp t1), TOp t2) ->
            case findLowerDiffT t1 t2 of
                Just (Diff p) ->
                    ifFailed (Just p) (findBase step (n-1) p)

                Just NoDiff ->
                    Just (TOp (Tri 0))

                Nothing ->
                    Nothing

        (Just (LOp l1), LOp l2) ->
            case findLowerDiffL l1 l2 of
                Just (Diff p) ->
                    ifFailed (Just p) (findBase step (n-1) p)

                Just NoDiff ->
                    Just (LOp (L 0))

                Nothing ->
                    Nothing

        _ ->
            Nothing


tryStepCase : (Int, Int) -> (ProofTree, ProofTree) -> Maybe SchematicProof
tryStepCase (stepN, baseN) (step, base) =
    let
        generalStep =
            repeatSearch baseN base stepN step
    in
    case findBase generalStep baseN base of
        Just diff ->
            Just { step = generalStep, base = diff }

        Nothing ->
            Nothing


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

        mDiff =
            findDifference smallP largeP

        testPossibleRepeats ps rNums =
            case rNums of
                (r::rs) ->
                    if List.all (\x -> x) (List.indexedMap (\i p -> if divides ((List.length ps - 1)//r) i then Maybe.Extra.isJust p else True) ps) then
                        List.Extra.getAt ((List.length ps - 1)//r) ps
                            |> Maybe.withDefault Nothing
                            |> Maybe.andThen (tryStepCase (largeN, largeN - (largeN-smallN)//r))
                            |> ifFailed (testPossibleRepeats ps rs)
                    else
                        testPossibleRepeats ps rs

                [] ->
                    Nothing
    in
    case (Maybe.andThen (findPossibleRepeatPoints smallP) mDiff, mDiff, largeN == smallN) of
        (Just ps, Just diff, False) ->
            gcd (largeN - smallN) (List.length ps - 1)
                |> divisors
                |> List.reverse
                |> testPossibleRepeats ps

        _ ->
            Nothing


makeShapeInputs : Int -> { n : String, shape : String, p1 : String, p2 : String } -> List (Html Msg)
makeShapeInputs i { n, shape, p1, p2 } =
    Html.text ","
    ::input [ type_ "number", Html.Attributes.value n, onInput (SetN (Just i)) ] []
    ::Html.text "✕"
    ::select
        [ onInput (SetShape (Just i)) ]
        [ option [ Html.Attributes.value "square", Html.Attributes.selected True ] [ Html.text "Square" ]
        , option [ Html.Attributes.value "rect" ] [ Html.text "Rectangle" ]
        , option [ Html.Attributes.value "tri" ] [ Html.text "Triangle" ]
        , option [ Html.Attributes.value "l" ] [ Html.text "L" ]
        , option [ Html.Attributes.value "frame" ] [ Html.text "Frame" ]
        ]
    ::input [ type_ "number", Html.Attributes.value p1, onInput (SetP1 (Just i)) ] []
    :: (if shape == "rect" || shape == "frame" then [ input [ type_ "number", Html.Attributes.value p2, onInput (SetP2 (Just i)) ] [] ] else [])
    ++ [ button [ onClick (Delete i) ] [ Html.text "Delete" ] ]


view : Model -> Document Msg
view model =
    { title = "Netherite"
    , body =
        case model.stage of
            Initialising i shapes ->
                label
                    []
                    [ Html.text "n:"
                    , input
                        [ type_ "number", Html.Attributes.value i.n, onInput (SetN Nothing) ]
                        []
                    ]
                :: br [] []
                :: label
                    []
                    ([ Html.text "Shape:"
                    , select
                        [ onInput (SetShape Nothing) ]
                        [ option [ Html.Attributes.value "square", Html.Attributes.selected True ] [ Html.text "Square" ]
                        , option [ Html.Attributes.value "rect" ] [ Html.text "Rectangle" ]
                        , option [ Html.Attributes.value "tri" ] [ Html.text "Triangle" ]
                        , option [ Html.Attributes.value "l" ] [ Html.text "L" ]
                        , option [ Html.Attributes.value "frame" ] [ Html.text "Frame" ]
                        ]
                    , input [ type_ "number", Html.Attributes.value i.p1, onInput (SetP1 Nothing) ] []
                    ]
                    ++ (if i.shape == "rect" || i.shape == "frame" then [ input [ type_ "number", Html.Attributes.value i.p2, onInput (SetP2 Nothing) ] [] ] else []))
                :: br [] []
                :: Html.text "Goal:"
                :: Maybe.withDefault [] (List.tail (List.concat (List.indexedMap makeShapeInputs shapes)))
                ++ [ button [ onClick AddShape ] [ Html.text "+" ], br [] [], button [ onClick Next ] [ Html.text "Prove..." ], button [ onClick SearchProof ] [ Html.text "Discover Proof" ] ]

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

            DoneI i ->
                input
                    [ type_ "number", Html.Attributes.value i.n, onInput (SetN Nothing) ]
                    []
                ::select
                    [ onInput (SetShape Nothing) ]
                    [ option [ Html.Attributes.value "square", Html.Attributes.selected True ] [ Html.text "Square" ]
                    , option [ Html.Attributes.value "rect" ] [ Html.text "Rectangle" ]
                    , option [ Html.Attributes.value "tri" ] [ Html.text "Triangle" ]
                    , option [ Html.Attributes.value "l" ] [ Html.text "L" ]
                    , option [ Html.Attributes.value "frame" ] [ Html.text "Frame" ]
                    ]
                :: input [ type_ "number", Html.Attributes.value i.p1, onInput (SetP1 Nothing) ] []
                :: (if i.shape == "rect" || i.shape == "frame" then [ input [ type_ "number", Html.Attributes.value i.p2, onInput (SetP2 Nothing) ] [] ] else [])
                ++ [ button [ onClick Next ] [ Html.text "Show proof" ] ]

            Done (Just pt) ->
                drawShapes "" pt

            Done Nothing ->
                [ Html.text "No Proof Found" ]
    }


evaluateProofS m shape step schematic =
    case (shape, step) of
        (Square n, LCutS l s) ->
            if n > 1 then
                Maybe.map2 LCutS (evaluateProofL m (L n) l schematic) (evaluateProofS m (Square (n-1)) s schematic)
            else
                Nothing

        (Square n, SplitDiaS t1 t2) ->
            if n > 1 then
                Maybe.map2 SplitDiaS (evaluateProofT m (Tri n) t1 schematic) (evaluateProofT m (Tri (n-1)) t2 schematic)
            else
                Nothing

        (Square n, SplitOuterFrame f s) ->
            if n >= 3 then
                Maybe.map2 SplitOuterFrame (evaluateProofF m (Frame n 1) f schematic) (evaluateProofS m (Square (n-2)) s schematic)
            else
                Nothing

        (Square n, SplitInnerSquare f s) ->
            if n >= 3 then
                Maybe.map2 SplitInnerSquare (evaluateProofF m (Frame n ((n-1)//2)) f schematic) (evaluateProofS m (Square (n-2*((n-1)//2))) s schematic)
            else
                Nothing

        (Square n, Split4 s1 s2 s3 s4) ->
            if n >= 2 && modBy 2 n == 0 then
                Maybe.map4 Split4 (evaluateProofS m (Square (n//2)) s1 schematic) (evaluateProofS m (Square (n//2)) s2 schematic) (evaluateProofS m (Square (n//2)) s3 schematic) (evaluateProofS m (Square (n//2)) s4 schematic)
            else
                Nothing

        (Square n, Square _) ->
            Just (Square n)

        (Square n, RecurseS) ->
            Maybe.andThen unwrapS (evaluateProof (m-1) (SOp (Square n)) schematic)

        _ ->
            Nothing


evaluateProofR m shape step schematic =
    case (shape, step) of
        (Rect n1 n2, SplitDiaR t1 t2) ->
            if n1-n2 == 1 then
                Maybe.map2 SplitDiaR (evaluateProofT m (Tri n2) t1 schematic) (evaluateProofT m (Tri n2) t2 schematic)
            else if n2-n1 == 1 then
                Maybe.map2 SplitDiaR (evaluateProofT m (Tri n1) t1 schematic) (evaluateProofT m (Tri n1) t2 schematic)
            else
                Nothing

        (Rect n1 n2, SplitSquare s r) ->
            if n1 > n2 then
                Maybe.map2 SplitSquare (evaluateProofS m (Square n2) s schematic) (evaluateProofR m (Rect (n1-n2) n2) r schematic)
            else if n2 > n1 then
                Maybe.map2 SplitSquare (evaluateProofS m (Square n1) s schematic) (evaluateProofR m (Rect n1 (n2-n1)) r schematic)
            else
                Nothing

        (Rect n1 n2, ToSquare s) ->
            if n1 == n2 then
                Maybe.map ToSquare (evaluateProofS m (Square n1) s schematic)

            else
                Nothing

        (Rect n1 n2, Rotate r) ->
            Maybe.map Rotate (evaluateProofR m (Rect n2 n1) r schematic)

        (Rect n1 n2, Rect _ _) ->
            Just (Rect n1 n2)

        (Rect n1 n2, RecurseR) ->
            Maybe.andThen unwrapR (evaluateProof (m-1) (ROp (Rect n1 n2)) schematic)

        _ ->
            Nothing


evaluateProofT m shape step schematic =
    case (shape, step) of
        (Tri n, SplitTST t1 s t2) ->
            if n > 1 then
                Maybe.map3 SplitTST (evaluateProofT m (Tri (n//2)) t1 schematic) (evaluateProofS m (Square (n-n//2)) s schematic) (evaluateProofT m (Tri (n//2)) t2 schematic)
            else
                Nothing

        (Tri n, LCutT l t) ->
            if n > 2 then
                Maybe.map2 LCutT (evaluateProofL m (L n) l schematic) (evaluateProofT m (Tri (n-2)) t schematic)
            else
                Nothing

        (Tri n, SplitSide r t) ->
            if n > 1 then
                Maybe.map2 SplitSide (evaluateProofR m (Rect 1 n) r schematic) (evaluateProofT m (Tri (n-1)) t schematic)
            else
                Nothing

        (Tri n, Tri _) ->
            Just (Tri n)

        (Tri n, RecurseT) ->
            Maybe.andThen unwrapT (evaluateProof (m-1) (TOp (Tri n)) schematic)

        _ ->
            Nothing


evaluateProofF m shape step schematic =
    case (shape, step) of
        (Frame n1 n2, SplitFrame r1 r2 r3 r4) ->
            Maybe.map4 SplitFrame (evaluateProofR m (Rect (n1-n2) n2) r1 schematic) (evaluateProofR m (Rect (n1-n2) n2) r2 schematic) (evaluateProofR m (Rect n2 (n1-n2)) r3 schematic) (evaluateProofR m (Rect n2 (n1-n2)) r4 schematic)

        (Frame n1 n2, Frame _ _) ->
            Just (Frame n1 n2)

        (Frame n1 n2, RecurseF) ->
            Maybe.andThen unwrapF (evaluateProof (m-1) (FOp (Frame n1 n2)) schematic)

        _ ->
            Nothing


evaluateProofL m shape step schematic =
    case (shape, step) of
        (L n, SplitEnds r l) ->
            if n > 1 then
                Maybe.map2 SplitEnds (evaluateProofR m (Rect 1 2) r schematic) (evaluateProofL m (L (n-1)) l schematic)

            else
                Nothing

        (L n, L _) ->
            Just (L n)

        (L n, RecurseL) ->
            Maybe.andThen unwrapL (evaluateProof (m-1) (LOp (L n)) schematic)

        _ ->
            Nothing


{- reachable : ProofTree -> ProofTree -> Bool
reachable shape1 shape2 =
    case (shape1, shape2) of
        (SOp (Square a), SOp (Square c)) ->
            a >= c

        (SOp (Square a), ROp (Rect c d)) ->
             (a - (a-1)//2 >= c && (a-1)//2 >= d) || ((a-1)//2 >= c && a - (a-1)//2 >= d)

        (SOp (Square a), TOp (Tri c)) ->
            a >= c

        (SOp (Square a), FOp (Frame c d)) ->
            a >= c

        (SOp (Square a), LOp (L c)) ->
            a >= c

        (ROp (Rect a b), SOp (Square c)) ->
            a >= c && b >= c

        (ROp (Rect a b), ROp (Rect c d)) ->
            (a >= c && b >= d) || (a >= d && b >= c)

        (ROp (Rect a b), TOp (Tri c)) ->
            a >= c || b >= c

        (ROp (Rect a b), FOp (Frame c d)) ->
            a >= c && b >= c

        (ROp (Rect a b), LOp (L c)) ->
            a >= c && b >= c

        (TOp (Tri a), SOp (Square c)) ->
            a-(a//2) >= c

        (TOp (Tri a_), ROp (Rect c d)) ->
            let
                a =
                    a_-(a_//2)
            in
            (a - (a-1)//2 >= c && (a-1)//2 >= d) || ((a-1)//2 >= c && a - (a-1)//2 >= d) || (c == 1 && a_ >= d) || (d == 1 && a_ >= c)

        (TOp-}


evaluateProof n start ({ step, base } as schematic) =
    let
        evalP p =
            case (start, p) of
                (SOp s1, SOp s2) ->
                    Maybe.map SOp (evaluateProofS n s1 s2 schematic)

                (ROp r1, ROp r2) ->
                    Maybe.map ROp (evaluateProofR n r1 r2 schematic)

                (FOp f1, FOp f2) ->
                    Maybe.map FOp (evaluateProofF n f1 f2 schematic)

                (TOp t1, TOp t2) ->
                    Maybe.map TOp (evaluateProofT n t1 t2 schematic)

                (LOp l1, LOp l2) ->
                    Maybe.map LOp (evaluateProofL n l1 l2 schematic)

                _ ->
                    Nothing
        in
        if n == 1 then
            evalP base

        else
            Maybe.andThen evalP (buildRepeat n step)


buildInitShape : { n : String, shape : String, p1 : String, p2 : String } -> Maybe (Int, ProofTree)
buildInitShape { n, shape, p1, p2 } =
    case (String.toInt n, (shape, String.toInt p1, String.toInt p2)) of
        ( Just jn, ("square", Just jp1, _)) ->
            Just (jn, SOp (Square jp1))

        ( Just jn, ("rect", Just jp1, Just jp2)) ->
            Just (jn, ROp (Rect jp1 jp2))

        ( Just jn, ("tri", Just jp1, _ )) ->
            Just (jn, TOp (Tri jp1))

        ( Just jn, ("l", Just jp1, _ )) ->
            Just (jn, LOp (L jp1))

        ( Just jn, ("frame", Just jp1, Just jp2)) ->
            Just (jn, FOp (Frame jp1 jp2))

        _ ->
            Nothing


sortPt : ProofTree -> ProofTree -> Order
sortPt p1 p2 =
    case (p1, p2) of
        (SOp (Square a), SOp (Square c)) ->
            compare a c

        (SOp (Square _), SOp _) ->
            LT

        (SOp _, SOp _) ->
            EQ

        (SOp _, _) ->
            LT

        (ROp (Rect a b), ROp (Rect c d)) ->
            case compare a c of
                EQ ->
                    compare b d

                x ->
                    x

        (ROp (Rect _ _), ROp _) ->
            LT

        (ROp _, SOp _) ->
            GT

        (ROp _, ROp _) ->
            EQ

        (ROp _, _) ->
            LT

        (TOp (Tri a), TOp (Tri c)) ->
            compare a c

        (TOp (Tri _), TOp _) ->
            LT

        (TOp _, SOp _) ->
            GT

        (TOp _, ROp _) ->
            GT

        (TOp _, TOp _) ->
            EQ

        (TOp _, _) ->
            LT

        (FOp (Frame a b), FOp (Frame c d)) ->
            case compare a c of
                EQ ->
                    compare b d

                x ->
                    x

        (FOp (Frame _ _), FOp _) ->
            LT

        (FOp _, FOp _) ->
            EQ

        (FOp _, LOp _) ->
            LT

        (FOp _, _) ->
            GT

        (LOp (L a), LOp (L c)) ->
            compare a c

        (LOp (L _), LOp _) ->
            LT

        (LOp _, LOp _) ->
            EQ

        (LOp _, _) ->
            GT


filterGoal : List ProofTree -> List (a, List ProofTree) -> List (a, List ProofTree)
filterGoal goal =
    List.filter (\(_, ends) -> List.Extra.isSubsequenceOf ends goal)


andSearch next =
    List.concatMap (\(x, y) -> List.map (Tuple.mapBoth x ((++) y >> List.sortWith sortPt)) next)


findProofS : SquareOp -> List ProofTree -> List (SquareOp, List ProofTree)
findProofS ss goal =
    case ss of
        Square n ->
            let
                lcutSearch =
                    if n > 1 then
                        List.map (Tuple.mapFirst LCutS) (findProofL (L n) goal)
                            |> andSearch (findProofS (Square (n-1)) goal)
                            |> filterGoal goal

                    else
                        []

                splitdiaSearch =
                    if n > 1 then
                        List.map (Tuple.mapFirst SplitDiaS) (findProofT (Tri n) goal)
                            |> andSearch (findProofT (Tri (n-1)) goal)
                            |> filterGoal goal

                    else
                        []

                splitouterSearch =
                    if n >= 3 then
                        List.map (Tuple.mapFirst SplitOuterFrame) (findProofF (Frame n 1) goal)
                            |> andSearch (findProofS (Square (n-2)) goal)
                            |> filterGoal goal

                    else
                        []

                splitinnerSearch =
                    if n >= 3 then
                        List.map (Tuple.mapFirst SplitInnerSquare) (findProofF (Frame n ((n-1)//2)) goal)
                            |> andSearch (findProofS (Square (n-2*((n-1)//2))) goal)
                            |> filterGoal goal

                    else
                        []

                split4Search =
                    if n>= 2 && modBy 2 n == 0 then
                        let
                            squareProofs =
                                findProofS (Square (n//2)) goal
                        in
                        List.map (Tuple.mapFirst Split4) squareProofs
                            |> andSearch squareProofs
                            |> andSearch squareProofs
                            |> andSearch squareProofs

                    else
                        []
            in
            (Square n, [SOp (Square n)])::(lcutSearch++splitdiaSearch++splitouterSearch++splitinnerSearch++split4Search)

        _ ->
            []

findProofR rotated rr goal =
    case rr of
        Rect n1 n2 ->
            let
                splitdiaSearch =
                    if n1-n2 == 1 then
                        let
                            triProofs =
                                findProofT (Tri n2) goal
                        in
                        List.map (Tuple.mapFirst SplitDiaR) triProofs
                            |> andSearch triProofs
                            |> filterGoal goal

                    else if n2-n1 == 1 then
                        let
                            triProofs =
                                findProofT (Tri n1) goal
                        in
                        List.map (Tuple.mapFirst SplitDiaR) triProofs
                            |> andSearch triProofs
                            |> filterGoal goal

                    else
                        []

                splitsquareSearch =
                    if n1 > n2 then
                        List.map (Tuple.mapFirst SplitSquare) (findProofS (Square n2) goal)
                            |> andSearch (findProofR False (Rect (n1-n2) n2) goal)
                            |> filterGoal goal

                    else if n2 > n1 then
                        List.map (Tuple.mapFirst SplitSquare) (findProofS (Square n1) goal)
                            |> andSearch (findProofR False (Rect n1 (n2-n1)) goal)
                            |> filterGoal goal

                    else
                        []

                tosquareSearch =
                    if n1 == n2 then
                        List.map (Tuple.mapFirst ToSquare) (findProofS (Square n1) goal)
                            |> filterGoal goal

                    else
                        []

                rotateSearch =
                    if not rotated then
                        List.map (Tuple.mapFirst Rotate) (findProofR True (Rect n2 n1) goal)
                            |> filterGoal goal
                    else
                        []
            in
            (Rect n1 n2, [ROp (Rect n1 n2)])::(splitdiaSearch++splitsquareSearch++rotateSearch)

        _ ->
            []


findProofT tt goal =
    case tt of
        Tri n ->
            let
                splittstSearch =
                    if n > 1 then
                        let
                            triProofs =
                                findProofT (Tri (n//2)) goal
                        in
                        List.map (Tuple.mapFirst SplitTST) triProofs
                            |> andSearch (findProofS (Square (n-n//2)) goal)
                            |> andSearch triProofs
                            |> filterGoal goal

                    else
                        []

                lcutSearch =
                    if n > 2 then
                        List.map (Tuple.mapFirst LCutT) (findProofL (L n) goal)
                            |> andSearch (findProofT (Tri (n-2)) goal)
                            |> filterGoal goal

                    else
                        []

                splitsideSearch =
                    if n > 1 then
                        List.map (Tuple.mapFirst SplitSide) (findProofR False (Rect 1 n) goal)
                            |> andSearch (findProofT (Tri (n-1)) goal)
                            |> filterGoal goal

                    else
                        []
            in
            (Tri n, [TOp (Tri n)])::(splittstSearch++lcutSearch++splitsideSearch)

        _ ->
            []


findProofF ff goal =
    case ff of
        Frame n1 n2 ->
            let
                splitframeSearch =
                    if n1 > n2 then
                        let
                            rect1Proofs =
                                findProofR False (Rect (n1-n2) n2) goal

                            rect2Proofs =
                                findProofR False (Rect n2 (n1-n2)) goal
                        in
                        List.map (Tuple.mapFirst SplitFrame) rect1Proofs
                            |> andSearch rect1Proofs
                            |> andSearch rect2Proofs
                            |> andSearch rect2Proofs
                    else
                        []
            in
            (Frame n1 n2, [FOp (Frame n1 n2)])::splitframeSearch

        _ ->
            []


findProofL ll goal =
    case ll of
        L n ->
            let
                splitendsSearch =
                    if n > 1 then
                        List.map (Tuple.mapFirst SplitEnds) (findProofR False (Rect 1 2) goal)
                            |> List.concatMap (\(x, y) -> List.map (Tuple.mapBoth x ((++) y >> List.sortWith sortPt)) (findProofL (L (n-1)) goal))
                            |> filterGoal goal

                    else
                        []
            in
            (L n, [LOp (L n)])::splitendsSearch

        _ ->
            []


findProof : ProofTree -> List ProofTree -> Maybe ProofTree
findProof start goal =
    let
        sGoal =
            List.sortWith sortPt goal
    in
    case start of
        SOp s ->
            findProofS s sGoal
                |> List.map (Tuple.mapFirst SOp)
                |> List.filter (Tuple.second >> (==) sGoal)
                |> List.head
                |> Maybe.map Tuple.first

        ROp r ->
            findProofR False r sGoal
                |> List.map (Tuple.mapFirst ROp)
                |> List.filter (Tuple.second >> (==) sGoal)
                |> List.head
                |> Maybe.map Tuple.first

        TOp t ->
            findProofT t sGoal
                |> List.map (Tuple.mapFirst TOp)
                |> List.filter (Tuple.second >> (==) sGoal)
                |> List.head
                |> Maybe.map Tuple.first

        FOp f ->
            findProofF f sGoal
                |> List.map (Tuple.mapFirst FOp)
                |> List.filter (Tuple.second >> (==) sGoal)
                |> List.head
                |> Maybe.map Tuple.first

        LOp l ->
            findProofL l sGoal
                |> List.map (Tuple.mapFirst LOp)
                |> List.filter (Tuple.second >> (==) sGoal)
                |> List.head
                |> Maybe.map Tuple.first


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.stage of
        Initialising i shapes ->
            case msg of
                SetN Nothing n ->
                    ({ model | stage = Initialising { i | n = n } shapes }, Cmd.none)

                SetN (Just j) n ->
                    ({ model | stage = Initialising i (List.Extra.updateAt j (\x -> { x | n = n }) shapes) }, Cmd.none)

                SetShape Nothing shape ->
                    ({ model | stage = Initialising { i | shape = shape } shapes }, Cmd.none)

                SetShape (Just j) shape ->
                    ({ model | stage = Initialising i (List.Extra.updateAt j (\x -> { x | shape = shape }) shapes) }, Cmd.none)

                SetP1 Nothing p1 ->
                    ({ model | stage = Initialising { i | p1 = p1 } shapes }, Cmd.none)

                SetP1 (Just j) p1 ->
                    ({ model | stage = Initialising i (List.Extra.updateAt j (\x -> { x | p1 = p1 }) shapes) }, Cmd.none)

                SetP2 Nothing p2 ->
                    ({ model | stage = Initialising { i | p2 = p2 } shapes }, Cmd.none)

                SetP2 (Just j) p2 ->
                    ({ model | stage = Initialising i (List.Extra.updateAt j (\x -> { x | p2 = p2 }) shapes) }, Cmd.none)

                Next ->
                    ( { proofs = (Maybe.withDefault [] (Maybe.map List.singleton (buildInitShape i)))++model.proofs, stage = Proving { operation = "none" } }, Cmd.none )

                AddShape ->
                    ( { model | stage = Initialising i <| shapes++[ { n = "", shape = "square", p1 = "", p2 = "" } ] }, Cmd.none )

                Delete j ->
                    ( { model | stage = Initialising i (List.Extra.removeAt j shapes) }, Cmd.none )

                SearchProof ->
                    let
                        builtShapes =
                            List.filterMap buildInitShape shapes

                        mgoal =
                            if List.length builtShapes == List.length shapes then
                                Just (List.concatMap (\(x, y) -> List.repeat x y) builtShapes)

                            else
                                Nothing
                    in
                    case (buildInitShape i, mgoal) of
                        (Just (n, start), Just goal) ->
                            case findProof start goal of
                                Just pt ->
                                    if List.length model.proofs >= 1 then
                                        ( { proofs = (n, pt)::model.proofs, stage = DoneI { n = "", shape = "square", p1 = "", p2 = "" } }, Cmd.none )

                                    else
                                        ( { proofs = (n, pt)::model.proofs, stage = Initialising { n = "", shape = "square", p1 = "", p2 = ""} [] }, Cmd.none )

                                Nothing ->
                                    (model, Cmd.none)

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
                        --( {model | stage = Done <| Debug.log "" (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing) }, Cmd.none )
                        ( { model | stage = DoneI { n = "", shape = "square", p1 = "", p2 = "" } }, Cmd.none )
                    else
                        ( { model | stage = Initialising { n = "", shape = "square", p1 = "", p2 = "" } [] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DoneI i ->
            case msg of
                SetN Nothing n ->
                    ({ model | stage = DoneI { i | n = n } }, Cmd.none)

                SetShape Nothing shape ->
                    ({ model | stage = DoneI { i | shape = shape } }, Cmd.none)

                SetP1 Nothing p1 ->
                    ({ model | stage = DoneI { i | p1 = p1 } }, Cmd.none)

                SetP2 Nothing p2 ->
                    ({ model | stage = DoneI { i | p2 = p2 } }, Cmd.none)

                Next ->
                    case (String.toInt i.n, (i.shape, String.toInt i.p1, String.toInt i.p2)) of
                        ( Just n, ("square", Just p1, _)) ->
                            ( { model | stage = Done (Maybe.andThen (evaluateProof n (SOp (Square p1))) (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing)) }, Cmd.none )

                        ( Just n, ("rect", Just p1, Just p2)) ->
                            ( { model | stage = Done (Maybe.andThen (evaluateProof n (ROp (Rect p1 p2))) (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing)) }, Cmd.none )

                        ( Just n, ("tri", Just p1, _ )) ->
                            ( { model | stage = Done (Maybe.andThen (evaluateProof n (TOp (Tri p1))) (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing)) }, Cmd.none )

                        ( Just n, ("l", Just p1, _ )) ->
                            ( { model | stage = Done (Maybe.andThen (evaluateProof n (LOp (L p1))) (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing)) }, Cmd.none )

                        ( Just n, ("frame", Just p1, Just p2)) ->
                            ( { model | stage = Done (Maybe.andThen (evaluateProof n (FOp (Frame p1 p2))) (Maybe.map2 infer (List.Extra.getAt 0 model.proofs) (List.Extra.getAt 1 model.proofs) |> Maybe.withDefault Nothing)) }, Cmd.none )

                        _ ->
                            (model, Cmd.none)
                _ ->
                    (model, Cmd.none)

        Done _ ->
            (model, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
