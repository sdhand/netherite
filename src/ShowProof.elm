module ShowProof exposing (findNextStep, countSteps, step)

import Generalise exposing (ProofTree(..), SquareOp(..), RectOp(..), TriOp(..), FrameOp(..), LOp(..))
import Svg exposing (..)


type EvalState
    = Done
    | Fail
    | Next (List (ProofTree, ProofTree))


findNextStep : List ProofTree -> Maybe (String, Int)
findNextStep pts =
    let
        helper n pts_ =
            case pts_ of
                x::xs ->
                    case x of
                        SOp (LCutS _ _) ->
                            Just ("L-Cut", n)

                        SOp (SplitDiaS _ _) ->
                            Just ("Split Diagonal", n)

                        SOp (SplitOuterFrame _ _) ->
                            Just ("Split Outer Frame", n)

                        SOp (SplitInnerSquare _ _) ->
                            Just ("Split Inner Square", n)

                        SOp (Split4 _ _ _ _) ->
                            Just ("Split Four", n)

                        ROp (SplitDiaR _ _) ->
                            Just ("Split Diagonal", n)

                        ROp (SplitSquare _ _) ->
                            Just ("Split Square", n)

                        ROp (ToSquare _) ->
                            Just ("To Square", n)

                        ROp (Rotate _) ->
                            Just ("Rotate", n)

                        TOp (SplitTST _ _ _) ->
                            Just ("SplitTST", n)

                        TOp (LCutT _ _) ->
                            Just ("L-Cut", n)

                        TOp (SplitSide _ _) ->
                            Just ("Split Side", n)

                        FOp (SplitFrame _ _ _ _) ->
                            Just ("Split Frame", n)

                        LOp (SplitEnds _ _) ->
                            Just ("Split Ends", n)

                        _ ->
                            helper (n+1) xs

                [] ->
                    Nothing
    in
    helper 0 pts


evalOneS n sop =
    case sop of
        LCutS l s ->
            if n > 1 then
                Next [ (LOp (L n), LOp l), (SOp (Square (n-1)), SOp s) ]

            else
                Fail

        SplitDiaS t1 t2 ->
            if n > 1 then
                Next [ (TOp (Tri n), TOp t1), (TOp (Tri (n-1)), TOp t2) ]

            else
                Fail

        SplitOuterFrame f s ->
            if n >= 3 then
                Next [ (FOp (Frame n 1), FOp f), (SOp (Square (n-2)), SOp s) ]

            else
                Fail

        SplitInnerSquare f s ->
            if n >= 3 then
                Next [ (FOp (Frame n ((n-1)//2)), FOp f), (SOp (Square (n-2*((n-1)//2))), SOp s) ]

            else
                Fail

        Split4 s1 s2 s3 s4 ->
            if n >= 2 && modBy 2 n == 0 then
                Next [ (SOp (Square (n//2)), SOp s1), (SOp (Square (n//2)), SOp s2), (SOp (Square (n//2)), SOp s3), (SOp (Square (n//2)), SOp s4) ]

            else
                Fail

        _ ->
            Done


evalOneR n1 n2 rop =
    case rop of
        SplitDiaR t1 t2 ->
            if n1-n2 == 1 then
                Next [ (TOp (Tri n2), TOp t1), (TOp (Tri n2), TOp t2) ]

            else if n2-n1 == 1 then
                Next [ (TOp (Tri n1), TOp t1), (TOp (Tri n1), TOp t2) ]

            else
                Fail

        SplitSquare s r ->
            if n1 > n2 then
                Next [ (SOp (Square n2), SOp s), (ROp (Rect (n1-n2) n2), ROp r) ]

            else if n2 > n1 then
                Next [ (SOp (Square n1), SOp s), (ROp (Rect n1 (n2-n1)), ROp r) ]

            else
                Fail

        ToSquare s ->
            if n1 == n2 then
                Next [ (SOp (Square n1), SOp s) ]

            else
                Fail

        Rotate r ->
            Next [ (ROp (Rect n2 n1), ROp r) ]

        _ ->
            Done


evalOneT n top =
    case top of
        SplitTST t1 s t2 ->
            if n > 1 then
                Next [ (TOp (Tri (n//2)), TOp t1), (SOp (Square (n-n//2)), SOp s), (TOp (Tri (n//2)), TOp t2) ]

            else
                Fail

        LCutT l t ->
            if n > 2 then
                Next [ (LOp (L n), LOp l), (TOp (Tri (n-2)), TOp t) ]

            else
                Fail

        SplitSide r t ->
            if n > 1 then
                Next [ (ROp (Rect 1 n), ROp r), (TOp (Tri (n-1)), TOp t) ]

            else
                Fail

        _ ->
            Done


evalOneF n1 n2 fop =
    case fop of
        SplitFrame r1 r2 r3 r4 ->
            Next [ (ROp (Rect (n1-n2) n2), ROp r1), (ROp (Rect (n1-n2) n2), ROp r2), (ROp (Rect n2 (n1-n2)), ROp r3), (ROp (Rect n2 (n1-n2)), ROp r4) ]

        _ ->
            Done


evalOneL n lop =
    case lop of
        SplitEnds r l ->
            if n > 1 then
                Next [ (ROp (Rect 1 2), ROp r), (LOp (L (n-1)), LOp l) ]

            else
                Fail

        _ ->
            Done


countSteps : ProofTree -> Int
countSteps pt =
    case pt of
        SOp (LCutS l s) ->
            1 + countSteps (LOp l) + countSteps (SOp s)

        SOp (SplitDiaS t1 t2) ->
            1 + countSteps (TOp t1) + countSteps (TOp t2)

        SOp (SplitOuterFrame f s) ->
            1 + countSteps (FOp f) + countSteps (SOp s)

        SOp (SplitInnerSquare f s) ->
            1 + countSteps (FOp f) + countSteps (SOp s)

        SOp (Split4 s1 s2 s3 s4) ->
            1 + countSteps (SOp s1) + countSteps (SOp s2) + countSteps (SOp s3) + countSteps (SOp s4)

        ROp (SplitDiaR t1 t2) ->
            1 + countSteps (TOp t1) + countSteps (TOp t2)

        ROp (SplitSquare s r) ->
            1 + countSteps (SOp s) + countSteps (ROp r)

        ROp (ToSquare s) ->
            1 + countSteps (SOp s)

        ROp (Rotate r) ->
            1 + countSteps (ROp r)

        TOp (SplitTST t1 s t2) ->
            1 + countSteps (TOp t1) + countSteps (SOp s) + countSteps (TOp t2)

        TOp (LCutT l t) ->
            1 + countSteps (LOp l) + countSteps (TOp t)

        TOp (SplitSide r t) ->
            1 + countSteps (ROp r) + countSteps (TOp t)

        FOp (SplitFrame r1 r2 r3 r4) ->
            1 + countSteps (ROp r1) + countSteps (ROp r2) + countSteps (ROp r3) + countSteps (ROp r4)

        LOp (SplitEnds r l) ->
            1 + countSteps (ROp r) + countSteps (LOp l)

        _ ->
            0


step : Int -> List (ProofTree, ProofTree) -> Maybe (List (ProofTree, ProofTree))
step count current =
    case (count, current) of
        (0, _) ->
            Just current

        (_, x::xs) ->
            let
                next st =
                    case st of
                        Done ->
                            Maybe.map ((::) x) (step count xs)

                        Fail ->
                            Nothing

                        Next l ->
                            step (count-1) (l++xs)
            in
            case x of
                (SOp (Square n), SOp sop) ->
                    next (evalOneS n sop)

                (ROp (Rect n1 n2), ROp rop) ->
                    next (evalOneR n1 n2 rop)

                (TOp (Tri n), TOp top) ->
                    next (evalOneT n top)

                (FOp (Frame n1 n2), FOp fop) ->
                    next (evalOneF n1 n2 fop)

                (LOp (L n), LOp lop) ->
                    next (evalOneL n lop)

                _ ->
                    Nothing

        (_, []) ->
            Just []
