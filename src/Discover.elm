module Discover exposing (..)


import Generalise exposing (..)
import Equation exposing (..)
import Netherite exposing (..)
import ShowProof exposing (..)


import Dict


import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Press
    | Set String
    | GenM Generalise.Msg
    | SetStep Int
    | Hovering
    | NotHovering


type alias ProofStep =
    { proof : ProofTree
    , n : Int
    , start : ProofTree
    , current : List (ProofTree, ProofTree)
    , hovering : Bool
    , max : Int
    }


type Model
     = Input String
     | ShowProof (Shape, SchematicProof) String (Maybe ProofStep)


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


draw drawBorder pt =
    div
        (if drawBorder then [ style "border" "2px solid blue", style "display" "inline-block" ] else [style "display" "inline-block"])
        (drawShapes "" pt)


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
                {-, br [] []
                , text ("Proof: "++(Debug.toString sp))-}
                , br [] []
                ]
                ++(case pt of
                    Just proofStepState ->
                        drawProofStepState proofStepState

                    Nothing ->
                        [])
                {- (List.map (Html.map GenM) (case pt of
                    Just { current, hovering } ->
                        List.indexedMap (\n shape -> draw (findNextStep (List.map Tuple.second current) |> Maybe.map (Tuple.second >> ((==) n)) |> Maybe.withDefault False |> (&&) hovering) (Tuple.first shape)) current

                    Nothing ->
                        []
                    )) -}
    }


drawProofStepState { n, current, hovering, max } =
    let
        nextStep =
            findNextStep (List.map Tuple.second current)

        nextStepName =
            case nextStep of
                Just (name, _) ->
                    name++" "

                _ ->
                    ""
        nextStepN =
            Maybe.map Tuple.second nextStep
                |> Maybe.withDefault (-1)
    in
    [ button
        [ disabled (n <= 0)
        , onClick (SetStep (Basics.max (n-1) 0))
        ]
        [ text "<" ]
    , input
        [ type_ "range"
        , Html.Attributes.min "0"
        , Html.Attributes.max (String.fromInt max)
        , value (String.fromInt n)
        , onInput (String.toInt >> Maybe.withDefault n >> SetStep)
        ]
        []
    , button
        [ disabled (n >= max)
        , onClick (SetStep (Basics.min (n+1) max))
        , onMouseEnter Hovering
        , onMouseLeave NotHovering
        ]
        [ text (nextStepName++">") ]
    , br [] []
    ]
    ++ List.indexedMap (\stepN shape -> Html.map GenM (draw (stepN == nextStepN && hovering) (Tuple.first shape))) current


discoverProof : List (Representation, Representation) -> Maybe (Shape, SchematicProof)
discoverProof reprs =
    case reprs of
        (start, goal)::xs ->
            let
                startBig =
                    repr2Goal (Dict.fromList [ ('n', 30) ]) start

                goalBig =
                    repr2Goal (Dict.fromList [ ('n', 30) ]) goal

                startSmall =
                    repr2Goal (Dict.fromList [ ('n', 28) ]) start

                goalSmall =
                    repr2Goal (Dict.fromList [ ('n', 28) ]) goal

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
                    case (Debug.log "big" bigProof, Debug.log "small" smallProof) of
                        (Just bp, Just sp) ->
                            infer (30, bp) (28, sp)

                        _ ->
                            Nothing
            in
            case (start, proof) of
                ([r], Just sp) ->
                    Just (r, sp)

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
                            let
                                temp =
                                    { t1 = Debug.log "N: " n, t2 = Debug.log "start: " start, t3 = Debug.log "SP: " (Tuple.second sp) }
                            in
                            (ShowProof sp s (Maybe.map (\p -> {proof = p, start = start, n = 0, current = [(start,p) ], hovering = False, max = countSteps p}) (evaluateProof n start (Tuple.second sp))), Cmd.none)

                        _ ->
                            ( model, Cmd.none)

                _ ->
                    ( model, Cmd.none )

        (Hovering, ShowProof sp s (Just pt)) ->
            (ShowProof sp s (Just { pt | hovering = True }), Cmd.none)

        (NotHovering, ShowProof sp s (Just pt)) ->
            (ShowProof sp s (Just { pt | hovering = False }), Cmd.none)

        (SetStep n, ShowProof sp s (Just pt)) ->
            case ShowProof.step n [ (pt.start, pt.proof) ] of
                Just c ->
                    (ShowProof sp s (Just { pt | n = n, current = c }), Cmd.none)

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
