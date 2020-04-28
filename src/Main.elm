module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Equation
import Discover
import Svg exposing (svg, circle, Svg, line)
import Svg.Attributes exposing (cx, cy, r, x1, x2, y1, y2, stroke, strokeWidth)
import Netherite exposing (Shape(..))
import Generalise exposing (ProofTree(..), SchematicProof, SquareOp(..), RectOp(..), FrameOp(..), TriOp(..), LOp(..))
import Json.Decode as Decode exposing (Decoder)
import ShowProof exposing (..)


type Modal
    = Formula { formula : String, n1 : String, n2 : String }
    | None


type alias ProofStep =
    { proof : ProofTree
    , n : Int
    , start : ProofTree
    , current : List (ProofTree, ProofTree)
    , hovering : Bool
    , max : Int
    }


type ProofState
    = New
    | Proven
        { formula : String
        , schematic : (Shape, SchematicProof)
        , n : String
        , pt : Maybe ProofStep
        }


type alias Model =
    { modal : Modal
    , state : ProofState
    }


type Msg
    = NewProof
    | CancelModal
    | SetFormula String
    | SetN1 String
    | SetN2 String
    | Discover
    | SetN String
    | ShowProof
    | SetStep Int
    | Hovering
    | NotHovering
    | GenM Generalise.Msg


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


equations =
    [ ("n^2=Sum(i,1,n,2*i-1)", True)
    , ("(n*(n+1))/2=Sum(i,1,n,i)", True)
    , ("Tri(2*n+1)=Tri(n+1)+3*Tri(n)", True)
    , ("Tri(2*n)=Sum(i,1,n,2*(2*i)-1)", True)
    , ("(2*n+1)^2=1+4*Sum(i,1,n,2*i)", True)
    , ("Fib(n)*Fib(n+1)=Sum(i,1,n,Fib(i)^2)", True)
    , ("Tri(2*n-1)=Sum(i,1,n,2*(2*i-1)-1)", True)
    , ("n*(n+1)=(n*(n+1))/2+(n*(n+1))/2", True)
    , ("Tri(2*n)=Tri(n-1)+3*Tri(n)", True)
    , ("(2*n+1)^2=8*Tri(n)+1", True)
    , ("(2*n)^2=8*Tri(n-1)+4*n", True)
    , ("n*(n+3)=(n+3)*n", True)
    , ("(n^2+n)/2=Sum(i,1,n,i)", False)
    , ("Sum(i,0,n,n^2+i)=(2*n+1)*Tri(n)", False)
    , ("Sum(i,1,n,i^2)=(n*(n+1)*(2*n+1))/6", False)
    , ("Sum(i,1,n,(2*i-1)^2=(n*(2*n-1)*(2*n+1))/3", False)
    , ("(2*n+1)^2=Tri(3*n+1)-Tri(n)", False)
    , ("Tri(n-1)^2+Tri(n)^2=Tri(n^2)", False)
    , ("Sum(i,1,n,i*(i+1))=(n*(n+1)*(n+2))/3", False)
    , ("Sum(i,1,n-1,3^i)=(3^n-1)/2", False)
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    (
        { modal = None
        , state = New
        }
        , Cmd.none
    )


enterDecoder : Msg -> Decoder Msg
enterDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "Non-enter key pressed"
            )


{- radius = 5
distance = 20


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


drawSOp (x,y) n op =
    let
        xOffset =
                x * distance

        yOffset =
                y * distance
    in
    case op of
        LCutS l s ->
            [ line
                [ x1 <| String.fromInt <| radius + distance//2 + xOffset
                , x2 <| String.fromInt <| radius + distance//2 + xOffset
                , y1 <| String.fromInt <| 0 + yOffset
                , y2 <| String.fromInt <| (n-2)*distance+radius+distance//2+1 + yOffset
                , strokeWidth "2"
                , stroke "#000000"
                ]
                []
            , line
                [ x1 <| String.fromInt <| 2*radius + distance//4-1 + xOffset
                , x2 <| String.fromInt <| (n-1)*(radius+distance) + xOffset
                , y1 <| String.fromInt <| (n-2)*distance+radius+distance//2 + yOffset
                , y2 <| String.fromInt <| (n-2)*distance+radius+distance//2 + yOffset
                , strokeWidth "2"
                , stroke "#000000"
                ]
                []
            ] ++ drawLOp False (x, y) n l ++ drawSOp (x+1, y) (n-1) s

        _ ->
            []


drawROp (x,y) (n, m) op =
    []


drawTOp flip (x, y) n op =
    []


drawFOp (x, y) (n, m) fop =
    []


drawLOp flip (x, y) n lop =
    []


drawPtOps start pt =
    case (start, pt) of
        (SOp (Square n), SOp sop) ->
            drawSOp (0, 0) n sop

        (ROp (Rect n m), ROp rop) ->
            drawROp (0, 0) (n, m) rop

        (TOp (Tri n), TOp top) ->
            drawTOp False (0, 0) n top

        (FOp (Frame n m), FOp fop) ->
            drawFOp (0, 0) (n, m) fop

        (LOp (L n), LOp lop) ->
            drawLOp False (0, 0) n lop

        _ ->
            []


drawPt start pt =
    let
        mainShape =
            case start of
                SOp (Square n) ->
                    { shape = (List.concatMap (column 0 n) (List.range 0 (n-1)))
                    , width = n
                    , height = n
                    }

                ROp (Rect n m) ->
                    { shape = (List.concatMap (column 0 m) (List.range 0 (n-1)))
                    , width = n
                    , height = m
                    }

                TOp (Tri n) ->
                    { shape = (List.concatMap (\x -> row 0 (x+1) x) (List.range 0 (n-1)))
                    , width = n
                    , height = n
                    }

                FOp (Frame n m) ->
                    { shape =
                        (List.concatMap (column 0 n) (List.range 0 (m-1))
                            ++ List.concatMap (row m (n-m)) (List.range 0 (m-1))
                            ++ List.concatMap (row m (n-m)) (List.range (n-m) (n-1))
                            ++ List.concatMap (column 0 n) (List.range (n-m) (n-1)))
                    , width = n
                    , height = n
                    }

                LOp (L n) ->
                    { shape = ((row 1 n (n-1))++(column 0 n 0))
                    , width = n
                    , height = n
                    }

                _ ->
                    { shape = [], width = 0, height = 0 }

    in
    svg [ Svg.Attributes.class "mx-auto mt-5", width <| mainShape.width*distance-2*radius, height <| mainShape.height*distance-2*radius ] (mainShape.shape++drawPtOps start pt) -}

draw drawBorder pt =
    div
        (if drawBorder then [ style "border" "2px solid blue", class "d-inline-block shape-container" ] else [class "d-inline-block shape-container"])
        (Generalise.drawShapes "" pt)


showModal : Modal -> List (Html Msg)
showModal modal =
    case modal of
        Formula { formula, n1, n2 } ->
            [ div
                [ class "modal"
                , tabindex -1
                , attribute "role" "dialog"
                , style "display" "block"
                ]
                [ div
                    [ class "modal-dialog", attribute "role" "document" ]
                    [ div
                        [ class "modal-content" ]
                        [ div
                            [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ text "New Proof" ]
                            , button [ type_ "button", class "close", onClick CancelModal ] [ text "×" ]
                            ]
                        , div
                            [ class "modal-body" ]
                            [ Html.form
                                [ class "m-0" ]
                                [ div
                                    [ class "form-group" ]
                                    [ div
                                        [ class "input-group" ]
                                        [ div
                                            [ class "input-group-prepend" ]
                                            [ div [ class "input-group-text" ] [ text "Equation" ] ]
                                        , input
                                            [ type_ "text", class "form-control", value formula, onInput SetFormula ]
                                            []
                                        , div
                                            [ class "input-group-append" ]
                                            [ button
                                                [ class "btn btn-outline-secondary dropdown-toggle", attribute "data-toggle" "dropdown" ]
                                                []
                                            , div
                                                [ class "dropdown-menu dropdown-menu-right" ]
                                                (List.map (\(eq, dev) -> a [ class "dropdown-item", href "#", onClick (SetFormula eq) ] [ text eq, span [ class "float-right ml-2"] [ text <| if dev then "✓" else "✕" ]]) equations)
                                            ]
                                        ]
                                    ]
                                , div
                                    [ class "form-row" ]
                                    [ div
                                        [ class "col-md-6" ]
                                        [ div
                                            [ class "input-group" ]
                                            [ div
                                                [ class "input-group-prepend" ]
                                                [ div [ class "input-group-text" ] [ text "n", sub [] [ text "1" ] ] ]
                                            , input
                                                [ type_ "text", class "form-control", value n1, onInput SetN1 ]
                                                []
                                            ]
                                        ]
                                    , div
                                        [ class "col-md-6" ]
                                        [ div
                                            [ class "input-group" ]
                                            [ div
                                                [ class "input-group-prepend" ]
                                                [ div [ class "input-group-text" ] [ text "n", sub [] [ text "2" ] ] ]
                                            , input
                                                [ type_ "text", class "form-control", value n2, onInput SetN2 ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div
                            [ class "modal-footer" ]
                            [ button [ type_ "button", class "mr-auto btn btn-outline-danger", onClick CancelModal ] [ text "Cancel" ]
                            , button [ type_ "button", class "btn btn-outline-primary", onClick Discover ] [ text "Discover Proof" ]
                            , button [ type_ "button", class "btn btn-primary" ] [ text "Manually Prove" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show", id "error-modal-back" ] []
            ]

        None ->
            []


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
    [ Html.form
        [ class "form-inline mt-1" ]
        [ button
            [ disabled (n <= 0)
            , preventDefaultOn "click" (Decode.succeed (SetStep (Basics.max (n-1) 0), True))
            , class "btn btn-outline-secondary"
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
            , preventDefaultOn "click" (Decode.succeed (SetStep (Basics.min (n+1) max), True))
            , onMouseEnter Hovering
            , onMouseLeave NotHovering
            , class "btn btn-outline-secondary"
            ]
            [ text (nextStepName++">") ]
        , br [] []
        ]
    ]
    ++ List.indexedMap (\stepN shape -> Html.map GenM (draw (stepN == nextStepN && hovering) (Tuple.first shape))) current


view : Model -> Document Msg
view model =
    let
        new =
            div
                [ class "m-2 btn-group" ]
                [ input
                    [ type_ "button"
                    , class "btn btn-primary"
                    , value "New"
                    , onClick NewProof
                    ]
                    []
                ]


        proofState =
            case model.state of
                Proven _ ->
                    "Proven:"

                New ->
                    ""

        formulaText =
            case model.state of
                Proven p ->
                    p.formula

                New ->
                    ""

        formula =
            if model.state == New then
                []

            else
                [ div
                    [ class "input-group m-2 " ]
                    [ div
                        [ class "input-group-prepend" ]
                        [ div [ class "input-group-text" ] [ text proofState ] ]
                    , div
                        [ class "form-control input-group-text" ]
                        [ text formulaText ]
                    ]
                ]

        nVal =
            case model.state of
                Proven p ->
                    [ input
                        [ class "form-control pr-0"
                        , type_ "number"
                        , value p.n
                        , onInput SetN
                        , preventDefaultOn
                            "keydown"
                            ( enterDecoder ShowProof
                                |> Decode.map (\v -> (v, True))
                            )
                        ]
                        []
                    , div
                        [ class "input-group-append" ]
                        [ input
                            [ type_ "button"
                            , class "btn input-group-text"
                            , value "↲"
                            , onClick ShowProof
                            ]
                            []
                        ]
                    ]

                New ->
                    []

        n =
            if model.state == New then
                []

            else
                [ div
                    [ class "input-group m-2 " ]
                    ([ div
                        [ class "input-group-prepend" ]
                        [ div [ class "input-group-text" ] [ text "n:" ] ]
                    ]++nVal)
                ]

        proofTree =
            case model.state of
                Proven { pt } ->
                    case pt of
                        Just proofStepState ->
                            drawProofStepState proofStepState

                        Nothing ->
                            []

                _ ->
                    []

    in
    { title = "Netherite"
    , body =
        [ div
            []
            ([ Html.form
                [ class "border-bottom form-inline m-0" ]
                ([ new
                ]++formula++n)
            ]++proofTree)
        ] ++ showModal model.modal
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewProof ->
            ( { model | modal = Formula { formula = "", n1 = "", n2 = "" } }, Cmd.none )

        CancelModal ->
            ( { model | modal = None }, Cmd.none )

        SetFormula formula ->
            case model.modal of
                Formula modalForm ->
                    ( { model | modal = Formula { modalForm | formula = formula } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetN1 n1 ->
            case model.modal of
                Formula modalForm ->
                    ( { model | modal = Formula { modalForm | n1 = n1 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetN2 n2 ->
            case model.modal of
                Formula modalForm ->
                    ( { model | modal = Formula { modalForm | n2 = n2 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Discover ->
            case model.modal of
                Formula { formula, n1, n2 } ->
                    case (Equation.parse formula, String.toInt n1, String.toInt n2) of
                        (Ok eq, Just n1Parsed, Just n2Parsed) ->
                            case Discover.discover eq n1Parsed n2Parsed of
                                Just sp ->
                                    ( { model | state = Proven { formula = formula, schematic = sp, n = "", pt = Nothing }, modal = None }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetN n ->
            case model.state of
                Proven p ->
                    ({ model | state = Proven { p | n = n } }, Cmd.none)

                _ ->
                    ( model, Cmd.none )

        ShowProof ->
            case model.state of
                Proven ({ n, schematic } as p) ->
                    case String.toInt n of
                        Just nParsed ->
                            ( { model | state = Proven { p | pt = Maybe.map (\(start, proof) -> {proof = proof, start = start, n = 0, current = [(start,proof) ], hovering = False, max = countSteps proof }) (Discover.eval schematic nParsed) } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetStep step ->
            case model.state of
                Proven ({ pt } as p) ->
                    case pt of
                        Just jp ->
                            case ShowProof.step step [ ( jp.start, jp.proof ) ] of
                                Just current ->
                                    ( { model | state = Proven { p | pt = Just { jp | n = step, current = current } } }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Hovering ->
            case model.state of
                Proven ({ pt } as p) ->
                    ( { model | state = Proven { p | pt = Maybe.map (\jp -> { jp | hovering = True }) pt } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NotHovering ->
            case model.state of
                Proven ({ pt } as p) ->
                    ( { model | state = Proven { p | pt = Maybe.map (\jp -> { jp | hovering = False }) pt } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenM _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
