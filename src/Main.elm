module Main exposing (main)


import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics.Extra exposing (flip)


type alias Model =
    { diagram : List Diagram
    , operation : Maybe Operation
    }


type Diagram
    = Rect Int Int
    | L Int
    | Tri Int
    | Frame Int Int


type Operation
    = SplitInnerSquare
    | Split4
    | SplitEnds
    | SplitOuterFrame
    | SplitFrame
    | SplitSquare
    | SplitSide
    | SplitTST
    | SplitDia
    | LCut


splitInnerSquare : Diagram -> List Diagram
splitInnerSquare d =
    case d of
        Rect n m ->
            if n == m && n >= 3 then
                [ Frame n ((n-1)//2), Rect (n-2*((n-1)//2)) (n-2*((n-1)//2)) ]

            else
                [d]

        _ ->
            [d]


split4 : Diagram -> List Diagram
split4 d =
    case d of
        Rect n m ->
            if n == m && modBy 2 n == 0 then
                List.repeat 4 (Rect (n//2) (n//2))

            else
                [d]

        _ ->
            [d]


splitEnds : Diagram -> List Diagram
splitEnds d =
    case d of
        L n ->
            [L (n-1), Rect 1 2]

        _ ->
            [d]


splitOuterFrame : Diagram -> List Diagram
splitOuterFrame d =
    case d of
        Rect n m ->
            if n == m && n >= 3 then
                [ Rect (n-2) (n-2), Frame n 1 ]

            else
                [d]

        _ ->
            [d]


splitFrame : Diagram -> List Diagram
splitFrame d =
    case d of
        Frame n w ->
            [ Rect (n-1) w, Rect (n-1) w, Rect w (n-1), Rect w (n-1) ]

        _ ->
            [d]


splitSquare : Diagram -> List Diagram
splitSquare d =
    case d of
        Rect n m ->
            if n > m then
                [ Rect m m, Rect (n-m) m ]

            else if m > n then
                [Rect n n, Rect n (m-n) ]

            else
                [d]

        _ ->
            [d]


splitSide : Diagram -> List Diagram
splitSide d =
    case d of
        Tri n ->
            [ Tri (n-1) ]

        _ ->
            [d]


splitTST : Diagram -> List Diagram
splitTST d =
    case d of
        Tri n ->
            [ Tri (n//2), Rect (n-n//2) (n-n//2), Tri (n//2) ]

        _ ->
            [d]


splitDia : Diagram -> List Diagram
splitDia d =
    case d of
        Rect n m ->
            case (n-m+1) of
                1 ->
                    [ Tri n, Tri (n-1) ]

                2 ->
                    [ Tri m, Tri m ]

                0 ->
                    [ Tri n, Tri n ]

                _ ->
                    [d]

        {- Square n ->
            [ Tri n, Tri (n-1) ] -}

        _ ->
            [d]


lcut : Diagram -> List Diagram
lcut d =
    case d of
        Rect n m ->
            if n == m && n /= 1 then
                [L n, Rect (n-1) (n-1) ]

            else
                [d]

        Tri n ->
            if n <= 2 then
                [d]

            else
                [ L n, Tri (n-2) ]

        _ ->
            [d]


radius : Int
radius =
    5


distance : Int
distance =
    20


type Msg
    = SetOp String
    | DoOp Int


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { diagram = [Rect 5 5], operation = Nothing }, Cmd.none )


draw x y =
    circle
        [ cx <| String.fromInt <| radius + distance * x
        , cy <| String.fromInt <| radius + distance * y
        , r <| String.fromInt radius
        ]
        []


row : Int -> Int -> Int -> List (Svg Msg)
row start end y =
    List.map (flip draw <| y) (List.range start (end-1))


column : Int -> Int -> Int -> List (Svg Msg)
column start end x =
    List.map (draw x) (List.range start (end-1))


viewDiagram : Int -> Diagram -> Html Msg
viewDiagram i d =
    case d of
        {- Square n ->
            List.concatMap (column 0 n) (List.range 0 (n-1))
                |> svg [Html.Attributes.style "padding" "30px", Html.Events.onClick (Op LCut i), width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] -}

        L n ->
            svg [ Html.Events.onClick (DoOp i), Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] ((row 1 n (n-1))++(column 0 n 0))

        Rect w h ->
            List.concatMap (column 0 h) (List.range 0 (w-1))
                |> svg [ Html.Events.onClick (DoOp i), Html.Attributes.style "padding" "30px", width <| String.fromInt (w*distance-2*radius), height <| String.fromInt (h*distance-2*radius)]

        Tri n ->
            List.concatMap (\x -> row 0 (x+1) x) (List.range 0 (n-1))
                |> svg [ Html.Events.onClick (DoOp i), Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)]

        Frame n w ->
            (List.concatMap (column 0 n) (List.range 0 (w-1)) ++ List.concatMap (row w (n-w)) (List.range 0 (w-1)) ++ List.concatMap (row w (n-w)) (List.range (n-w) (n-1)) ++ List.concatMap (column 0 n) (List.range (n-w) (n-1)))
                |> svg [ Html.Events.onClick (DoOp i), Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)]

        {- Dia n ->
            List.map (\x -> draw x (n-1-x)) (List.range 0 (n-1))
                |> svg [ Html.Events.onClick (DoOp i), Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] -}


toOp : String -> Maybe Operation
toOp s =
    case s of
        "splitInnerSquare" ->
            Just SplitInnerSquare

        "split4" ->
            Just Split4

        "splitEnds" ->
            Just SplitEnds

        "splitOuterFrame" ->
            Just SplitOuterFrame

        "splitFrame" ->
            Just SplitFrame

        "splitSquare" ->
            Just SplitSquare

        "splitSide" ->
            Just SplitSide

        "splitTST" ->
            Just SplitTST

        "splitDia" ->
            Just SplitDia

        "lcut" ->
           Just LCut

        _ ->
            Nothing


view : Model -> Browser.Document Msg
view model =
    { title = "Netherite"
    , body =
        select
            [ onInput SetOp ]
            [ option [ Html.Attributes.value "none" ] [ Html.text "None" ]
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
            ]
            ::List.indexedMap viewDiagram model.diagram
    }


toOpFunc o =
    case o of
        Nothing ->
            \x -> [x]

        Just SplitInnerSquare ->
            splitInnerSquare

        Just Split4 ->
            split4

        Just SplitEnds ->
            splitEnds

        Just SplitOuterFrame ->
            splitOuterFrame

        Just SplitFrame ->
            splitFrame

        Just SplitSquare ->
            splitSquare

        Just SplitSide ->
            splitSide

        Just SplitTST ->
            splitTST

        Just SplitDia ->
            splitDia

        Just LCut ->
            lcut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOp s ->
            ({ model | operation = toOp s }, Cmd.none)
        {- Op LCut i ->
            ( List.concat <| List.indexedMap (\j dia -> if j == i then lcut dia else [dia]) model, Cmd.none ) -}

        DoOp i ->
            ( { model | diagram = List.concat <| List.indexedMap (\j dia -> if j == i then (toOpFunc model.operation) dia else [dia]) model.diagram }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
