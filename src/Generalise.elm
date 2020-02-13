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
    | UnitS


type RectOp
    = SplitSquare SquareOp RectOp
    | SplitDiaR TriOp TriOp
    | ToSquare SquareOp
    | Rotate RectOp
    | Rect Int Int
    | UnitR


type FrameOp
    = SplitFrame RectOp RectOp RectOp RectOp
    | Frame Int Int
    | UnitF


type TriOp
    = SplitTST TriOp SquareOp TriOp
    | LCutT LOp TriOp
    | SplitSide RectOp TriOp
    | Tri Int
    | UnitT


type LOp
    = SplitEnds RectOp LOp
    | L Int
    | UnitL


type ProofTree
    = SOp SquareOp
    | ROp RectOp
    | FOp FrameOp
    | TOp TriOp
    | LOp LOp


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
        UnitS ->
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
        UnitR ->
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
        UnitT ->
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
        UnitF ->
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
        UnitL ->
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


findDifferenceS : (SquareOp -> ProofTree) -> ProofTree -> SquareOp -> Maybe ProofTree
findDifferenceS diff small large =
    if small == SOp large then
        Just (diff UnitS)
    else
        case large of
            Square _ ->
                Nothing

            Split4 s1 s2 s3 s4 ->
                findDifferenceS (diff << \x -> Split4 x s2 s3 s4) small s1
                    |> ifFailed ((findDifferenceS (diff << \x -> Split4 s1 x s3 s4)) small s2)
                    |> ifFailed ((findDifferenceS (diff << \x -> Split4 s1 s2 x s4)) small s3)
                    |> ifFailed ((findDifferenceS (diff << Split4 s1 s2 s3)) small s4)

            _ ->
                Nothing


findDifferenceR : (RectOp -> ProofTree) -> ProofTree -> RectOp -> Maybe ProofTree
findDifferenceR diff small large =
    if small == ROp large then
        Just (diff UnitR)
    else
        Nothing



findDifference : ProofTree -> ProofTree -> Maybe ProofTree
findDifference small large =
        case large of
            SOp s ->
                findDifferenceS SOp small s
            _ ->
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
