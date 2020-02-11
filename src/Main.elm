module Main exposing (main)


import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics.Extra exposing (flip)


type alias Model =
    List Diagram


type Diagram
    = Rect Int Int
    | L Int
    | Tri Int
    | Frame Int Int


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
    = LCut Int


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( split4 (Rect 6 6), Cmd.none )


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
                |> svg [Html.Attributes.style "padding" "30px", Html.Events.onClick (LCut i), width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] -}

        L n ->
            svg [ Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] ((row 1 n (n-1))++(column 0 n 0))

        Rect w h ->
            List.concatMap (column 0 h) (List.range 0 (w-1))
                |> svg [Html.Attributes.style "padding" "30px", width <| String.fromInt (w*distance-2*radius), height <| String.fromInt (h*distance-2*radius)]

        Tri n ->
            List.concatMap (\x -> row 0 (x+1) x) (List.range 0 (n-1))
                |> svg [Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)]

        Frame n w ->
            (List.concatMap (column 0 n) (List.range 0 (w-1)) ++ List.concatMap (row w (n-w)) (List.range 0 (w-1)) ++ List.concatMap (row w (n-w)) (List.range (n-w) (n-1)) ++ List.concatMap (column 0 n) (List.range (n-w) (n-1)))
                |> svg [Html.Attributes.style "padding" "30px", Html.Events.onClick (LCut i), width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)]

        {- Dia n ->
            List.map (\x -> draw x (n-1-x)) (List.range 0 (n-1))
                |> svg [Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] -}


view : Model -> Browser.Document Msg
view model =
    { title = "Netherite"
    , body =
        List.indexedMap viewDiagram model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LCut i ->
            ( List.concat <| List.indexedMap (\j dia -> if j == i then lcut dia else [dia]) model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
