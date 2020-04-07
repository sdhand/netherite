module Search exposing (..)

import Generalise exposing (ProofTree(..), SquareOp(..), RectOp(..), FrameOp(..), LOp (..), TriOp(..), normalizeG, sortPt, diffPt, reachable)


type alias Next a =
    { shape : a
    , found : List ProofTree
    , rGoal : List ProofTree
    , build : a -> List ProofTree -> AppendableProofTree
    }


type AppendableProofTree
    = NextS (Next SquareOp)
    | NextR { shape : RectOp, found : List ProofTree, rGoal : List ProofTree, build : RectOp -> List ProofTree -> AppendableProofTree, rotate : Bool }
    | NextT (Next TriOp)
    | NextF (Next FrameOp)
    | NextL (Next LOp)
    | Done ProofTree (List ProofTree)

findProofHelper : List ProofTree -> AppendableProofTree -> Maybe ProofTree
findProofHelper goal points =
    case points of
        NextS { shape, found, rGoal, build } ->
            if List.any (reachable (SOp shape) >> not) rGoal then
                Nothing
            else
                findProofS shape build found rGoal goal

        NextR { shape, found, rGoal, build, rotate } ->
            if List.any (reachable (ROp shape) >> not) rGoal then
                Nothing
            else
                findProofR rotate shape build found rGoal goal

        NextT { shape, found, rGoal, build } ->
            if List.any (reachable (TOp shape) >> not) rGoal then
                Nothing
            else
                findProofT shape build found rGoal goal

        NextF { shape, found, rGoal, build } ->
            if List.any (reachable (FOp shape) >> not) rGoal then
                Nothing
            else
                findProofF shape build found rGoal goal

        NextL { shape, found, rGoal, build } ->
            if List.any (reachable (LOp shape) >> not) rGoal then
                Nothing
            else
                findProofL shape build found rGoal goal

        Done pt found ->
            if found == goal then
                Just pt

            else
                Nothing


findProof : ProofTree -> List ProofTree -> Maybe ProofTree
findProof start goal =
    let
        sGoal =
            List.map normalizeG goal
                |> List.sortWith sortPt
    in
    case start of
        SOp s ->
            findProofHelper sGoal (NextS { shape = s, found = [], rGoal = sGoal, build = SOp >> Done })

        ROp r ->
            findProofHelper sGoal (NextR { shape = r, found = [], rGoal = sGoal, build = ROp >> Done, rotate = False })

        TOp t ->
            findProofHelper sGoal (NextT { shape = t, found = [], rGoal = sGoal, build = TOp >> Done })

        FOp f ->
            findProofHelper sGoal (NextF { shape = f, found = [], rGoal = sGoal, build = FOp >> Done })

        LOp l ->
            findProofHelper sGoal (NextL { shape = l, found = [], rGoal = sGoal, build = LOp >> Done })


ifFailed m1 m2 =
    case m2 of
        Just a ->
            Just a

        Nothing ->
            m1 ()


insert x l =
    case l of
        (y::ys) ->
            case sortPt x y of
                EQ ->
                    x::l

                LT ->
                    x::l

                GT ->
                    y::(insert x ys)

        [] ->
            [x]




findProofS : SquareOp -> (SquareOp -> List ProofTree -> AppendableProofTree) -> List ProofTree -> List ProofTree -> List ProofTree -> Maybe ProofTree
findProofS ss build found rGoal goal =
    case ss of
        Square n ->
            let
                lcutSearch () =
                    if n > 1 then
                        findProofHelper
                            goal
                            (NextL
                                { shape = L n
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \l fo1 ->
                                        NextS
                                            { shape = Square (n-1)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build = \s fo2 -> build (LCutS l s) fo2
                                            }
                                }
                            )

                    else
                        Nothing

                splitdiaSearch () =
                    if n > 1 then
                        findProofHelper
                            goal
                            (NextT
                                { shape = Tri n
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \t1 fo1 ->
                                        NextT
                                            { shape = Tri (n-1)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build = \t2 fo2 -> build (SplitDiaS t1 t2) fo2
                                            }
                                }
                            )

                    else
                        Nothing

                splitouterSearch () =
                    if n >= 3 then
                        findProofHelper
                            goal
                            (NextF
                                { shape = Frame n 1
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \f fo1 ->
                                        NextS
                                            { shape = Square (n-2)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build = \s fo2 -> build (SplitOuterFrame f s) fo2
                                            }
                                }
                            )

                    else
                        Nothing

                splitinnerSearch () =
                    if n >= 3 then
                        findProofHelper
                            goal
                            (NextF
                                { shape = Frame n ((n-1)//2)
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \f fo1 ->
                                        NextS
                                            { shape = Square (n-2*((n-1)//2))
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build = \s fo2 -> build (SplitInnerSquare f s) fo2
                                            }
                                }
                            )
                    else
                        Nothing

                split4Search () =
                    if n >= 2 && modBy 2 n == 0 then
                        findProofHelper
                            goal
                            (NextS
                                { shape = Square (n//2)
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \s1 fo1 ->
                                        NextS
                                            { shape = Square (n//2)
                                            , found =  fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \s2 fo2 ->
                                                    NextS
                                                        { shape = Square (n//2)
                                                        , found = fo2
                                                        , rGoal = diffPt goal fo2
                                                        , build =
                                                            \s3 fo3 ->
                                                                NextS
                                                                    { shape = Square (n//2)
                                                                    , found = fo3
                                                                    , rGoal = diffPt goal fo3
                                                                    , build = \s4 fo4 -> build (Split4 s1 s2 s3 s3) fo4
                                                                    }
                                                        }
                                            }
                                }
                            )
                    else
                        Nothing
            in
            if List.member (SOp ss) rGoal then
                findProofHelper goal (build ss (insert (SOp ss) found))
                    |> ifFailed lcutSearch
                    |> ifFailed splitdiaSearch
                    |> ifFailed splitouterSearch
                    |> ifFailed splitinnerSearch
                    |> ifFailed split4Search
            else
                lcutSearch ()
                    |> ifFailed splitdiaSearch
                    |> ifFailed splitouterSearch
                    |> ifFailed splitinnerSearch
                    |> ifFailed split4Search

        _ ->
            Nothing


findProofR : Bool -> RectOp -> (RectOp -> List ProofTree -> AppendableProofTree) -> List ProofTree -> List ProofTree -> List ProofTree -> Maybe ProofTree
findProofR rotate rr build found rGoal goal =
    case rr of
        Rect n1 n2 ->
            let
                splitdiaSearch () =
                    if n1-n2 == 1 then
                        findProofHelper
                            goal
                            (NextT
                                { shape = Tri n2
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \t1 fo1 ->
                                        NextT
                                            { shape = Tri n2
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \t2 fo2 -> build (SplitDiaR t1 t2) fo2
                                            }
                                }
                            )

                    else if n2-n1 == 1 then
                        findProofHelper
                            goal
                            (NextT
                                { shape = Tri n1
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \t1 fo1 ->
                                        NextT
                                            { shape = Tri n1
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \t2 fo2 -> build (SplitDiaR t1 t2) fo2
                                            }
                                }
                            )
                    else
                        Nothing

                splitsquareSearch () =
                    if n1 > n2 then
                        findProofHelper
                            goal
                            (NextS
                                { shape = Square n2
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \s fo1 ->
                                        NextR
                                            { shape = Rect (n1-n2) n2
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \r fo2 -> build (SplitSquare s r) fo2
                                            , rotate = False
                                            }
                                }
                            )

                    else if n2 > n1 then
                        findProofHelper
                            goal
                            (NextS
                                { shape = Square n1
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \s fo1 ->
                                        NextR
                                            { shape = Rect n1 (n2-n1)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \r fo2 -> build (SplitSquare s r) fo2
                                            , rotate = False
                                            }
                                }
                            )
                    else
                        Nothing

                tosquareSearch () =
                    if n1 == n2 then
                        findProofHelper
                            goal
                            (NextS
                                { shape = Square n1
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \s fo1 -> build (ToSquare s) fo1
                                }
                            )
                    else
                        Nothing

                rotateSearch () =
                    if not rotate then
                        findProofHelper
                            goal
                            (NextR
                                { shape = Rect n2 n1
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \r fo1 -> build (Rotate r) fo1
                                , rotate = True
                                }
                            )
                    else
                        Nothing

                shape =
                    case (n1, n2) of
                        (1, 1) ->
                            SOp (Square 1)

                        _ ->
                            ROp rr
            in
            if List.member shape rGoal then
                findProofHelper goal (build rr (insert shape found))
                    |> ifFailed splitdiaSearch
                    |> ifFailed splitsquareSearch
                    |> ifFailed tosquareSearch
                    |> ifFailed rotateSearch
            else
                splitdiaSearch ()
                    |> ifFailed splitsquareSearch
                    |> ifFailed tosquareSearch
                    |> ifFailed rotateSearch


        _ ->
            Nothing


findProofT : TriOp -> (TriOp -> List ProofTree -> AppendableProofTree) -> List ProofTree -> List ProofTree -> List ProofTree -> Maybe ProofTree
findProofT tt build found rGoal goal =
    case tt of
        Tri n ->
            let
                splittstSearch () =
                    if n > 1 then
                        findProofHelper
                            goal
                            (NextT
                                { shape = Tri (n//2)
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \t1 fo1 ->
                                        NextS
                                            { shape = Square (n-n//2)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \s fo2 ->
                                                    NextT
                                                        { shape = Tri (n//2)
                                                        , found = fo2
                                                        , rGoal = diffPt goal fo2
                                                        , build =
                                                            \t2 fo3 -> build (SplitTST t1 s t2) fo3
                                                        }
                                            }
                                }
                            )
                    else
                        Nothing

                lcutSearch () =
                    if n > 2 then
                        findProofHelper
                            goal
                            (NextL
                                { shape = L n
                                , found = found
                                , rGoal = diffPt goal found
                                , build =
                                    \l fo1 ->
                                        NextT
                                            { shape = Tri (n-2)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \t fo2 ->
                                                    build (LCutT l t) fo2
                                            }
                                }
                            )
                    else
                        Nothing

                splitsideSearch () =
                    if n > 1 then
                        findProofHelper
                            goal
                            (NextR
                                { shape = Rect 1 n
                                , found = found
                                , rGoal = diffPt goal found
                                , rotate = False
                                , build =
                                    \r fo1 ->
                                        NextT
                                            { shape = Tri (n-1)
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , build =
                                                \t fo2 ->
                                                    build (SplitSide r t) fo2
                                            }
                                }
                            )
                    else
                        Nothing

                shape =
                    case n of
                        1 ->
                            SOp (Square 1)
                        _ ->
                            TOp tt
            in
            if List.member shape rGoal then
                findProofHelper goal (build tt (insert shape found))
                    |> ifFailed splittstSearch
                    |> ifFailed lcutSearch
                    |> ifFailed splitsideSearch
            else
                splittstSearch ()
                    |> ifFailed lcutSearch
                    |> ifFailed splitsideSearch

        _ ->
            Nothing

findProofF : FrameOp -> (FrameOp -> List ProofTree -> AppendableProofTree) -> List ProofTree -> List ProofTree -> List ProofTree -> Maybe ProofTree
findProofF ff build found rGoal goal =
    case ff of
        Frame n1 n2 ->
            let
                splitframeSearch () =
                    if n1 > n2 then
                        findProofHelper
                            goal
                            (NextR
                                { shape = Rect (n1-n2) n2
                                , found = found
                                , rGoal = diffPt goal found
                                , rotate = False
                                , build =
                                    \r1 fo1 ->
                                        NextR
                                            { shape = Rect (n1-n2) n2
                                            , found = fo1
                                            , rGoal = diffPt goal fo1
                                            , rotate = False
                                            , build =
                                                \r2 fo2 ->
                                                    NextR
                                                        { shape = Rect n2 (n1-n2)
                                                        , found = fo2
                                                        , rGoal = diffPt goal fo2
                                                        , rotate = False
                                                        , build =
                                                            \r3 fo3 ->
                                                                NextR
                                                                    { shape = Rect n2 (n1-n2)
                                                                    , found = fo3
                                                                    , rGoal = diffPt goal fo3
                                                                    , rotate = False
                                                                    , build =
                                                                        \r4 fo4 ->
                                                                            build (SplitFrame r1 r2 r3 r4) fo4
                                                                    }
                                                        }
                                            }
                                }
                            )

                    else
                        Nothing

                shape =
                    case (n1, n2) of
                        (1, 1) ->
                            SOp (Square 1)

                        _ ->
                            FOp ff
            in
            if List.member shape rGoal then
                findProofHelper goal (build ff (insert shape found))
                    |> ifFailed splitframeSearch
            else
                splitframeSearch ()

        _ ->
            Nothing


findProofL : LOp -> (LOp -> List ProofTree -> AppendableProofTree) -> List ProofTree -> List ProofTree -> List ProofTree -> Maybe ProofTree
findProofL ll build found rGoal goal =
    case ll of
        L n ->
            let
                splitendsSearch () =
                    findProofHelper
                        goal
                        (NextR
                            { shape = Rect 1 2
                            , found = found
                            , rGoal = diffPt goal found
                            , rotate = False
                            , build =
                                \r fo1 ->
                                    NextL
                                        { shape = L (n-1)
                                        , found = fo1
                                        , rGoal = diffPt goal fo1
                                        , build =
                                            \l fo2 -> build (SplitEnds r l) fo2
                                        }
                            }
                        )
                shape =
                    case n of
                        1 ->
                            SOp (Square 1)
                        _ ->
                            LOp ll
            in
            if List.member shape rGoal then
                findProofHelper goal (build ll (insert shape found))
                    |> ifFailed splitendsSearch
            else
                splitendsSearch ()

        _ ->
            Nothing
