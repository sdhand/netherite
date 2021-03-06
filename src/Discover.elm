module Discover exposing (..)


import Generalise exposing (..)
import Equation exposing (..)
import Netherite exposing (..)
import ShowProof exposing (..)
import Search exposing (..)


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


discover : (IExpr, IExpr) -> Int -> Int -> Maybe (Shape, SchematicProof)
discover (e1, e2) n1 n2 =
    let
        bigN =
            Basics.max n1 n2

        smallN =
            Basics.min n1 n2

        rRep =
            representations e1

        lRep =
            representations e2

        possible =
            List.concatMap (\a -> List.map (\b -> (a, b)) lRep) (List.filter (List.length >> (==) 1) rRep)
                ++ List.concatMap (\a -> List.map (\b -> (a, b)) rRep) (List.filter (List.length >> (==) 1) lRep)
    in
    discoverProof (List.sortBy (\(_, (_, a), (_, b)) -> List.length a * List.length b) (List.filterMap (buildGoal bigN smallN) possible)) bigN smallN



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


buildGoal : Int -> Int -> (Representation, Representation) -> Maybe (Shape, (ProofTree, List ProofTree), (ProofTree, List ProofTree))
buildGoal n1 n2 (start, goal)=
    case start of
        [r] ->
            case ( (repr2Goal (Dict.fromList [ ('n', n1) ]) start, repr2Goal (Dict.fromList [ ('n', n1) ]) goal), (repr2Goal (Dict.fromList [ ('n', n2) ]) start, repr2Goal (Dict.fromList [ ('n', n2) ]) goal)) of
                ((Just [sb], Just gb), (Just [ss], Just gs)) ->
                    Just (r, (sb, gb), (ss, gs))
                _ ->
                    Nothing

        _ ->
            Nothing


discoverProof : List (Shape, (ProofTree, List ProofTree), (ProofTree, List ProofTree)) -> Int -> Int -> Maybe (Shape, SchematicProof)
discoverProof reprs n1 n2 =
    case reprs of
        (r, (sb, gb), (ss, gs))::xs ->
            let
                bigProof =
                    Search.findProof sb gb

                smallProof =
                    Search.findProof ss gs

                proof =
                    case (bigProof, smallProof) of
                        (Just bp, Just sp) ->
                            infer (n1, bp) (n2, sp)

                        _ ->
                            Nothing
            in
            case proof of
                Just sp ->
                    Just (r, sp)

                _ ->
                    discoverProof xs n1 n2

        [] ->
            Nothing

eval sp n =
    case repr2Goal (Dict.fromList [ ('n', n) ]) [ Tuple.first sp ] of
        Just [ start ] ->
            Maybe.map (Tuple.pair start) (evaluateProof n start (Tuple.second sp))

        _ ->
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
                    case discoverProof (List.sortBy (\(_, (_, a), (_, b)) -> List.length a * List.length b) (List.filterMap (buildGoal 50 48) possible)) 50 48 of
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
