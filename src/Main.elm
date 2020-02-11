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
    = Square Int
    | Rect Int Int
    | L Int
    | Tri Int
    | Dia Int


splitDia : Diagram -> List Diagram
splitDia =
    Debug.crash "unimplemented"


lcut : Diagram -> List Diagram
lcut d =
    case d of
        Square n ->
            if n == 1 then
                [d]

            else
                [L n, Square (n-1)]

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
    ( [Rect 5 4], Cmd.none )


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
        Square n ->
            List.concatMap (column 0 n) (List.range 0 (n-1))
                |> svg [Html.Attributes.style "padding" "30px", Html.Events.onClick (LCut i), width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)]

        L n ->
            svg [ Html.Attributes.style "padding" "30px", width <| String.fromInt (n*distance-2*radius), height <| String.fromInt (n*distance-2*radius)] ((row 1 n (n-1))++(column 0 n 0))

        Rect w h ->
            List.concatMap (column 0 h) (List.range 0 (w-1))
                |> svg [Html.Attributes.style "padding" "30px", width <| String.fromInt (w*distance-2*radius), height <| String.fromInt (h*distance-2*radius)]


view : Model -> Browser.Document Msg
view model =
    { title = "Diamond"
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
