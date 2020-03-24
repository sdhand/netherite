module Discover exposing (..)


import Generalise exposing (..)
import Equation exposing (..)
import Netherite exposing (..)


import Dict


import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Press
    | Set String
    | GenM Generalise.Msg


type Model
     = Input String
     | ShowProof (Shape, SchematicProof) String (Maybe ProofTree)


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
    ( Input "", Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Netherite"
    , body =
        case model of
            Input s ->
                [ label
                    []
                    [ text "Equation:"
                    , input
                        [ value s, onInput Set ]
                        []
                    ]
                , button
                    [ onClick Press ]
                    [ text "Discover Proof" ]
                ]

            ShowProof sp s pt ->
                [ label
                    []
                    [ text "n:"
                    , input
                        [ value s, onInput Set ]
                        []
                    ]
                , button
                    [ onClick Press ]
                    [ text "Show Proof" ]
                , br [] []
                ]++(List.map (Html.map GenM) (case pt of
                    Just proof ->
                        drawShapes "" proof

                    Nothing ->
                        []
                    ))
    }


discoverProof : List (Representation, Representation) -> Maybe (Shape, SchematicProof)
discoverProof reprs =
    case reprs of
        (start, goal)::xs ->
            let
                startBig =
                    repr2Goal (Dict.fromList [ ('n', 5) ]) start

                goalBig =
                    repr2Goal (Dict.fromList [ ('n', 5) ]) goal

                startSmall =
                    repr2Goal (Dict.fromList [ ('n', 3) ]) start

                goalSmall =
                    repr2Goal (Dict.fromList [ ('n', 3) ]) goal

                bigProof =
                    case (startBig, goalBig) of
                        (Just [ sb ], Just gb) ->
                            findProof sb gb

                        _ ->
                            Nothing

                smallProof =
                    case (startSmall, goalSmall) of
                        (Just [ ss ], Just gs) ->
                            findProof ss gs

                        _ ->
                            Nothing

                proof =
                    case (bigProof, smallProof) of
                        (Just bp, Just sp) ->
                            infer (5, bp) (3, sp)

                        _ ->
                            Nothing
            in
            case (start, proof) of
                ([r], Just sp) ->
                    Just <| Debug.log "" (r, sp)

                _ ->
                    discoverProof xs

        [] ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (Set s, Input _) ->
            ( Input s, Cmd.none )

        (Press, Input s) ->
            case parse s of
                Ok (e1, e2) ->
                    let
                        rRep =
                            representations e1

                        lRep =
                            representations e2

                        possible =
                            List.concatMap (\a -> List.map (\b -> (a, b)) lRep) (List.filter (List.length >> (==) 1) rRep)
                                ++ List.concatMap (\a -> List.map (\b -> (a, b)) rRep) (List.filter (List.length >> (==) 1) lRep)
                    in
                    case discoverProof possible of
                        Just sp ->
                            ( ShowProof sp "" Nothing, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        (Set s, ShowProof sp _ pt) ->
            ( ShowProof sp s pt, Cmd.none )

        (Press, ShowProof sp s pt) ->
            case String.toInt s of
                Just n ->
                    case repr2Goal (Dict.fromList [ ('n', n) ]) [ (Tuple.first sp) ] of
                        Just [ start ] ->
                            (ShowProof sp s (evaluateProof n start (Tuple.second sp)), Cmd.none)

                        _ ->
                            ( model, Cmd.none)

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
